module CLC.Stackage.Runner.Report
  ( -- * Results
    Results (..),
    emptyResults,
    allPackages,

    -- ** Cache
    readCache,
    saveCache,

    -- * Report
    Report (..),
    Stats (..),
    mkReport,
    saveReport,
  )
where

import CLC.Stackage.Utils.IO qualified as IO
import CLC.Stackage.Utils.JSON qualified as JSON
import CLC.Stackage.Utils.Logging qualified as Logging
import CLC.Stackage.Utils.Package (Package)
import CLC.Stackage.Utils.Paths qualified as Paths
import Control.Exception (throwIO)
import Data.Aeson (AesonException (AesonException), FromJSON, ToJSON)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.Directory.OsPath qualified as Dir
import System.OsPath qualified as OsPath

-- | Results of a run. This represents the current run __only__ i.e. it is not
-- the sum of the current run + cache.
data Results = MkResults
  { successes :: Set Package,
    failures :: Set Package,
    untested :: Set Package
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Empty results.
emptyResults :: Results
emptyResults = MkResults Set.empty Set.empty Set.empty

-- | Unions all packages in the results.
allPackages :: Results -> Set Package
allPackages r = Set.unions [r.successes, r.failures, r.untested]

data Stats = MkStats
  { numSuccesses :: Int,
    successRate :: Int,
    numFailures :: Int,
    failureRate :: Int,
    numUntested :: Int,
    untestedRate :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Full report.
data Report = MkReport
  { results :: Results,
    stats :: Stats,
    startTime :: Text,
    endTime :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Derives a report from results.
mkReport :: Results -> Text -> Text -> Report
mkReport results startTime endTime =
  MkReport
    { results,
      stats =
        MkStats
          { numSuccesses,
            successRate,
            numFailures,
            failureRate,
            numUntested,
            untestedRate
          },
      startTime,
      endTime
    }
  where
    numSuccesses = length results.successes
    successRate = dv numSuccesses

    numFailures = length results.failures
    failureRate = dv numFailures

    numUntested = length results.untested
    untestedRate = dv numUntested

    numAllTested :: Double
    numAllTested = fromIntegral $ numSuccesses + numFailures + numUntested

    dv :: Int -> Int
    dv n = floor $ 100 * (fromIntegral n / numAllTested)

-- | Reads results data, if the cache exists.
readCache :: Logging.Handle -> IO (Maybe Results)
readCache handle = do
  catchPathStr <- T.pack <$> OsPath.decodeUtf Paths.cachePath
  Dir.doesFileExist Paths.cachePath >>= \case
    False -> do
      Logging.putTimeInfoStr handle $ "Cached results do not exist: " <> catchPathStr
      pure Nothing
    True -> do
      contents <- IO.readBinaryFile Paths.cachePath
      case JSON.decode contents of
        Left err -> throwIO $ AesonException err
        Right r -> do
          Logging.putTimeInfoStr handle $ "Using cached results: " <> catchPathStr
          pure $ Just r

-- | Saves the current progress data as the next prior run.
saveCache :: Results -> IO ()
saveCache results = do
  Dir.createDirectoryIfMissing False Paths.outputDir
  JSON.writeJson Paths.cachePath results

-- | Saves the report
saveReport :: Report -> IO ()
saveReport report = do
  Dir.createDirectoryIfMissing False Paths.outputDir
  JSON.writeJson Paths.reportPath report
