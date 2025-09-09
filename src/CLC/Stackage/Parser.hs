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
  ( StackageResponse (packages),
  )
import CLC.Stackage.Parser.API qualified as API
import CLC.Stackage.Parser.API.CabalConfig qualified as CabalConfig
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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.OsPath (OsPath, osp)

-- | Retrieves the list of packages, based on
-- 'CLC.Stackage.Parser.API.stackageUrl'.
getPackageList :: Logging.Handle -> Maybe OsPath -> IO [Package]
getPackageList hLogger msnapshotPath =
  getPackageListByOs hLogger msnapshotPath OS.currentOs

-- | Prints the package list to a file.
printPackageList :: Maybe Os -> IO ()
printPackageList mOs = do
  case mOs of
    Just os -> printOsList os
    Nothing -> for_ [minBound .. maxBound] printOsList
  where
    file Linux = [osp|pkgs_linux.txt|]
    file Osx = [osp|pkgs_osx.txt|]
    file Windows = [osp|pkgs_windows.txt|]

    printOsList os = do
      pkgs <- getPackageListByOsFmt os
      let txt = T.unlines pkgs
      IO.writeFileUtf8 (file os) txt

-- | Retrieves the package list formatted to text.
getPackageListByOsFmt :: Os -> IO [Text]
getPackageListByOsFmt =
  (fmap . fmap) Package.toDisplayName
    . getPackageListByOs Logging.mkDefaultLogger Nothing

-- | Helper in case we want to see what the package set for a given OS is.
getPackageListByOs :: Logging.Handle -> Maybe OsPath -> Os -> IO [Package]
getPackageListByOs hLogger msnapshotPath os = do
  excludedPkgs <- getExcludedPkgs os
  let filterExcluded = flip Set.notMember excludedPkgs . (.name)

  response <- case msnapshotPath of
    Nothing -> API.getStackage hLogger
    Just snapshotPath ->
      CabalConfig.parseCabalConfig
        <$> IO.readFileUtf8 snapshotPath

  let numPackages = length response.packages
  when (numPackages < 2000) $ do
    let msg =
          mconcat
            [ "Only found ",
              T.pack $ show numPackages,
              " packages. Is that right?"
            ]
    Logging.putTimeWarnStr hLogger msg

  let packages = filter filterExcluded response.packages

  pure packages

getExcludedPkgs :: Os -> IO (Set Text)
getExcludedPkgs os = do
  contents <- IO.readBinaryFile path

  excluded <- case JSON.decode contents of
    Left err -> fail err
    Right x -> pure x

  pure $ Set.fromList (excluded.all ++ osSel excluded)
  where
    path = [osp|excluded_pkgs.json|]

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
