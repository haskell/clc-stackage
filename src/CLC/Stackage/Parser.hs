{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

module CLC.Stackage.Parser
  ( -- * Retrieving packages
    PackageSet (..),
    getPackageList,
    packageListToSet,

    -- * Misc helpers
    printPackageList,
    getPackageListByOsFmt,
  )
where

import CLC.Stackage.Parser.API
  ( StackageResponse (packages, snapshot),
  )
import CLC.Stackage.Parser.API qualified as API
import CLC.Stackage.Parser.API.CabalConfig qualified as CabalConfig
import CLC.Stackage.Parser.Utils qualified as Utils
import CLC.Stackage.Utils.Exception qualified as Ex
import CLC.Stackage.Utils.IO qualified as IO
import CLC.Stackage.Utils.JSON qualified as JSON
import CLC.Stackage.Utils.Logging qualified as Logging
import CLC.Stackage.Utils.OS (Os (Linux, Osx, Windows))
import CLC.Stackage.Utils.OS qualified as OS
import CLC.Stackage.Utils.Package (Package)
import CLC.Stackage.Utils.Package qualified as Package
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (Version (Version))
import Data.Version qualified as Vers
import GHC.Generics (Generic)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.OsPath (OsPath, osp)
import System.Process qualified as P
import Text.ParserCombinators.ReadP qualified as ReadP

-- | Retrieves the 'PackageSet', based on
-- 'CLC.Stackage.Parser.API.stackageUrl'.
getPackageList :: Logging.Handle -> Maybe OsPath -> IO PackageSet
getPackageList hLogger msnapshotPath = do
  response <- getStackageResponse hLogger msnapshotPath
  packageListToSet hLogger response.packages

-- | Given a list of packages, returns a 'PackageSet' i.e. all constraints
-- and filtered packages, according to package_index.jsonc
packageListToSet :: Logging.Handle -> [Package] -> IO PackageSet
packageListToSet hLogger packages =
  getPackageListByOs hLogger packages OS.currentOs

-- | Prints the package list to a file.
printPackageList :: Logging.Handle -> Maybe OsPath -> Maybe Os -> IO ()
printPackageList hLogger msnapshotPath mOs = do
  response <- getStackageResponse hLogger msnapshotPath
  case mOs of
    Just os -> printOsList response os
    Nothing -> for_ [minBound .. maxBound] (printOsList response)
  where
    file Linux = [osp|pkgs_linux.txt|]
    file Osx = [osp|pkgs_osx.txt|]
    file Windows = [osp|pkgs_windows.txt|]

    printOsList response os = do
      pkgs <- getPackageListByOsFmt hLogger response os
      let txt = T.unlines pkgs
      IO.writeFileUtf8 (file os) txt

-- | Retrieves the package list formatted to text.
getPackageListByOsFmt :: Logging.Handle -> StackageResponse -> Os -> IO [Text]
getPackageListByOsFmt hLogger response os = do
  ps <- getPackageListByOs hLogger response.packages os
  pure $ Package.toDisplayName <$> ps.packageList

-- | PackageSet corresponds to the package set after filtering i.e. we
-- get some list of packages either from Stackage or --snapshot-path,
-- then we apply filtering according to package_index.jsonc
data PackageSet = MkPackageSet
  { -- | Extra packages that are not in packageList, but nevertheless we want
    -- to pin e.g. because they are transitive dependencies.
    extraPins :: [Package],
    -- | Package set after filtering out excluded packages.
    packageList :: [Package],
    -- | Package in packageList that we do not want to pin.
    unpinned :: [Text]
  }
  deriving stock (Eq, Show)

-- | Helper in case we want to see what the package set for a given OS is.
getPackageListByOs :: Logging.Handle -> [Package] -> Os -> IO PackageSet
getPackageListByOs hLogger packageList os = do
  packageIndex <- getPackageIndex hLogger

  -- Setup extra pins i.e. all p in packageList in packageIndex.excluded_pinned.
  let excludedPinnedCfg = Set.fromList packageIndex.excluded_pinned
      isExcludedPinned = flip Set.member excludedPinnedCfg . (.name)
      excludedPinned = filter isExcludedPinned packageList

  -- Setup excluded. This is packageIndex.excluded +
  -- packageIndex.excluded_pinned.
  let excluded =
        Set.fromList (packageIndex.excluded.all ++ osSel packageIndex.excluded)
          `Set.union` excludedPinnedCfg

      isNotExcluded = flip Set.notMember excluded . (.name)
      packages = filter isNotExcluded packageList
      msg =
        mconcat
          [ "Filtered to ",
            T.pack $ show $ length packages,
            " packages (",
            T.toLower $ T.pack $ show os,
            ")."
          ]
  Logging.putTimeInfoStr hLogger msg

  pure $
    MkPackageSet
      { extraPins = excludedPinned,
        packageList = packages,
        unpinned = packageIndex.unpinned
      }
  where
    osSel :: Excluded -> [Text]
    osSel = case os of
      Linux -> (.linux)
      Osx -> (.osx)
      Windows -> (.windows)

getStackageResponse :: Logging.Handle -> Maybe OsPath -> IO StackageResponse
getStackageResponse hLogger msnapshotPath = do
  response <- case msnapshotPath of
    Nothing -> API.getStackage hLogger
    Just snapshotPath ->
      CabalConfig.parseCabalConfig
        <$> IO.readFileUtf8 snapshotPath

  let snapshotName = fromMaybe "<unknown>" response.snapshot
      numPackages = length $ response.packages
      numPackagesTxt = T.pack $ show numPackages
      snapshotMsg =
        mconcat
          [ "Snapshot: ",
            snapshotName,
            " (",
            numPackagesTxt,
            " packages)"
          ]
  Logging.putTimeInfoStr hLogger snapshotMsg

  when (numPackages < 2000) $ do
    let msg =
          mconcat
            [ "Only found ",
              numPackagesTxt,
              " packages. Is that right?"
            ]
    Logging.putTimeWarnStr hLogger msg

  checkGhcVersion hLogger response

  pure response

getPackageIndex :: Logging.Handle -> IO PackageIndex
getPackageIndex hLogger = do
  contents <- Ex.throwLeft . Utils.stripComments =<< IO.readBinaryFile path

  packageIndex <- case JSON.decode @PackageIndex contents of
    Left err -> fail err
    Right x -> pure x

  verifyIndex packageIndex

  pure packageIndex
  where
    path = [osp|package_index.jsonc|]

    -- Verifies index properties.
    verifyIndex index = do
      -- Each key should not have duplicates. A violation is mostly harmless,
      -- presumably an oversight.
      warnDuplicates "excluded.all" index.excluded.all
      warnDuplicates "excluded.linux" index.excluded.linux
      warnDuplicates "excluded.osx" index.excluded.osx
      warnDuplicates "excluded.windows" index.excluded.windows

      warnDuplicates "excluded_pinned" index.excluded_pinned
      warnDuplicates "unpinned" index.unpinned

      let allOs = Set.fromList index.excluded.all
          linux = Set.fromList index.excluded.linux
          osx = Set.fromList index.excluded.osx
          windows = Set.fromList index.excluded.windows

          excluded = Set.unions [allOs, linux, osx, windows]
          excludedPinned = Set.fromList index.excluded_pinned
          unpinned = Set.fromList index.unpinned

      -- Each of these should be disjoint. Note that various excluded subkeys
      -- may not be disjoint e.g. excluded.linux and excluded.osx both
      -- exclude vty-windows. Hence we only check the combined excluded
      -- against the other two.
      --
      -- This is more serious than duplicate violations as it could lead
      -- to confusing behavior e.g. if a package is in excluded_pinned
      -- and unpinned.
      errNonDisjoint "excluded" excluded "excluded_pinned" excludedPinned
      errNonDisjoint "excluded" excluded "unpinned" unpinned
      errNonDisjoint "excluded_pinned" excludedPinned "unpinned" unpinned

    errNonDisjoint xName x yName y = do
      let z = Set.intersection x y
      unless (Set.null z) $ do
        let libs = T.intercalate ", " $ Set.toList z
            msg =
              mconcat
                [ "package_index.jsonc: ",
                  xName,
                  " and ",
                  yName,
                  " are not disjoint: ",
                  libs
                ]
        Logging.putTimeErrStr hLogger msg
        throwIO $ ExitFailure 1

    warnDuplicates xName x = do
      let (_, duplicates) = foldl' go (Set.empty, Set.empty) x
          go (foundSoFar, dupes) l =
            if Set.member l foundSoFar
              then (foundSoFar, Set.insert l dupes)
              else (Set.insert l foundSoFar, dupes)

      unless (Set.null duplicates) $ do
        let libs = T.intercalate ", " $ Set.toList duplicates
            msg =
              mconcat
                [ "package_index.jsonc: ",
                  xName,
                  " has duplicates: ",
                  libs
                ]
        Logging.putTimeWarnStr hLogger msg

-- | Corresponds to package_index.jsonc
data PackageIndex = MkPackageIndex
  { -- | Excluded packages.
    excluded :: Excluded,
    -- | Excluded packages that we nevertheless pin.
    excluded_pinned :: [Text],
    -- | Normal packages that should not be pinned.
    unpinned :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | package_index.excluded.
data Excluded = MkExcluded
  { -- | Packages excluded from all Os's.
    all :: [Text],
    -- | Packages excluded from linux.
    linux :: [Text],
    -- | Packages excluded from osx.
    osx :: [Text],
    -- | Packages excluded from windows.
    windows :: [Text]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

checkGhcVersion :: Logging.Handle -> StackageResponse -> IO ()
checkGhcVersion hLogger response = do
  mPathGhc <- do
    -- Users are instructed to add their ghc to the PATH, so this should be
    -- correct.
    (ex, out, err) <- P.readProcessWithExitCode "ghc" ["--numeric-version"] ""
    case ex of
      ExitFailure _ -> do
        let msg = "Failed running 'ghc --numeric-version': " <> T.pack err
        Logging.putTimeWarnStr hLogger msg
        pure Nothing
      ExitSuccess -> do
        let out' = T.strip $ T.pack out
        case parseVers out' of
          Nothing -> do
            Logging.putTimeWarnStr hLogger $ "PATH ghc: Failed parsing: " <> out'
            pure Nothing
          Just v -> do
            Logging.putTimeInfoStr hLogger $ "PATH ghc: " <> showCanonical v
            pure $ Just v

  mStackageGhc <-
    case response.ghc of
      Nothing -> do
        let msg = "Failed detecting Stackage ghc version"
        Logging.putTimeWarnStr hLogger msg
        pure Nothing
      Just ghc -> do
        case parseVers ghc of
          Nothing -> do
            Logging.putTimeWarnStr hLogger $ "Stackage ghc: Failed parsing: " <> ghc
            pure Nothing
          Just v -> do
            Logging.putTimeInfoStr hLogger $ "Stackage ghc: " <> showCanonical v
            pure $ Just v

  case (mPathGhc, mStackageGhc) of
    (Just pathGhc, Just stackageGhc) -> case compareCanonical pathGhc stackageGhc of
      GhcEq -> pure ()
      GhcDiffMinor -> do
        let msg = "PATH and Stackage ghc have a minor difference. This may cause solver / build issues."
        Logging.putTimeWarnStr hLogger msg
      GhcDiffMajor -> do
        let msg = "PATH and Stackage ghc have a major difference. This will likely cause solver / build issues."
        Logging.putTimeWarnStr hLogger msg
    _ -> pure ()
  where
    parseVers txt =
      let ps = ReadP.readP_to_S Vers.parseVersion (T.unpack txt)
       in lastMaybe ps >>= versToCanonical . fst

    lastMaybe [] = Nothing
    lastMaybe [x] = Just x
    lastMaybe (_ : xs) = lastMaybe xs

-- Only take the first 3 e.g. 9.12.3. This is so we can compare custom
-- ghcs (e.g. 9.12.3.20260311) against expected major.major.minor format.
versToCanonical :: Version -> Maybe (Int, Int, Int)
versToCanonical (Version bs _tags) = case bs of
  (mj1 : mj2 : mn : _) -> Just (mj1, mj2, mn)
  _ -> Nothing

showCanonical :: (Int, Int, Int) -> Text
showCanonical (mj1, mj2, mn) =
  T.intercalate "." $
    T.pack . show
      <$> [mj1, mj2, mn]

compareCanonical :: (Int, Int, Int) -> (Int, Int, Int) -> GhcCompareResult
compareCanonical x@(xmj1, xmj2, _xmn) y@(ymj1, ymj2, _ymn)
  | x == y = GhcEq
  | xmj1 /= ymj1 = GhcDiffMajor
  | xmj2 /= ymj2 = GhcDiffMajor
  | otherwise = GhcDiffMinor

data GhcCompareResult
  = GhcEq
  | GhcDiffMinor
  | GhcDiffMajor
  deriving stock (Eq, Show)
