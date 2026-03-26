{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}

module CLC.Stackage.Parser
  ( -- * Retrieving packages
    getPackageList,

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
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
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

-- | Retrieves the list of packages, based on
-- 'CLC.Stackage.Parser.API.stackageUrl'.
getPackageList :: Logging.Handle -> Maybe OsPath -> IO [Package]
getPackageList hLogger msnapshotPath = do
  response <- getStackageResponse hLogger msnapshotPath
  getPackageListByOs hLogger response OS.currentOs

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
  ps <- getPackageListByOs hLogger response os
  pure $ Package.toDisplayName <$> ps

-- | Helper in case we want to see what the package set for a given OS is.
getPackageListByOs :: Logging.Handle -> StackageResponse -> Os -> IO [Package]
getPackageListByOs hLogger response os = do
  excludedPkgs <- getExcludedPkgs os
  let filterExcluded = flip Set.notMember excludedPkgs . (.name)
      packages = filter filterExcluded response.packages
      msg =
        mconcat
          [ "Filtered to ",
            T.pack $ show $ length packages,
            " packages (",
            T.toLower $ T.pack $ show os,
            ")."
          ]
  Logging.putTimeInfoStr hLogger msg

  pure packages

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

getExcludedPkgs :: Os -> IO (Set Text)
getExcludedPkgs os = do
  contents <- Ex.throwLeft . Utils.stripComments =<< IO.readBinaryFile path

  excluded <- case JSON.decode contents of
    Left err -> fail err
    Right x -> pure x

  pure $ Set.fromList (excluded.all ++ osSel excluded)
  where
    path = [osp|excluded_pkgs.jsonc|]

    osSel :: Excluded -> [Text]
    osSel = case os of
      Linux -> (.linux)
      Osx -> (.osx)
      Windows -> (.windows)

data Excluded = MkExcluded
  { all :: [Text],
    linux :: [Text],
    osx :: [Text],
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
