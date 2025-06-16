{-# LANGUAGE QuasiQuotes #-}

module CLC.Stackage.Runner.Env
  ( RunnerEnv (..),
    setup,
    teardown,

    -- * Misc
    getResults,
    resultsToNewCache,
  )
where

import CLC.Stackage.Builder.Env
  ( BuildEnv
      ( MkBuildEnv,
        batch,
        colorLogs,
        groupFailFast,
        hLogger,
        packagesToBuild,
        progress,
        writeLogs
      ),
    Progress
      ( MkProgress,
        failuresRef,
        successesRef
      ),
  )
import CLC.Stackage.Builder.Env qualified as Builder.Env
import CLC.Stackage.Builder.Package (Package (MkPackage, name, version))
import CLC.Stackage.Parser qualified as Parser
import CLC.Stackage.Parser.Data.Response (PackageResponse (name, version))
import CLC.Stackage.Runner.Args
  ( ColorLogs
      ( ColorLogsDetect,
        ColorLogsOff,
        ColorLogsOn
      ),
  )
import CLC.Stackage.Runner.Args qualified as Args
import CLC.Stackage.Runner.Report (Results (MkResults))
import CLC.Stackage.Runner.Report qualified as Report
import CLC.Stackage.Utils.Exception qualified as Ex
import CLC.Stackage.Utils.IO qualified as IO
import CLC.Stackage.Utils.Logging qualified as Logging
import CLC.Stackage.Utils.Paths qualified as Paths
import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.Bool (Bool (False, True), not)
import Data.Foldable (Foldable (foldl'))
import Data.IORef (newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (Maybe (Just, Nothing), fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (LocalTime)
import System.Console.Pretty (supportsPretty)
import System.Directory.OsPath qualified as Dir
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath (osp)
import System.OsPath qualified as OsP
import Prelude (IO, Monad ((>>=)), mconcat, pure, show, ($), (++), (.), (<$>), (<>))

-- | Args used for building all packages.
data RunnerEnv = MkRunnerEnv
  { -- | Environment used in building.
    buildEnv :: BuildEnv,
    -- | Status from previous run.
    cache :: Maybe Results,
    -- | The complete package set from stackage. This is used to write the
    -- cabal.project.local's constraint section, to ensure we always use the
    -- same transitive dependencies.
    completePackageSet :: [Package],
    -- | Disables the cache, which otherwise saves the outcome of a run in a
    -- json file. The cache is used for resuming a run that was interrupted.
    noCache :: Bool,
    -- | If we do not revert the cabal file at the end (i.e. we leave the
    -- last attempted build).
    noCleanup :: Bool,
    -- | Whether to retry packages that failed.
    retryFailures :: Bool,
    -- | Start time.
    startTime :: LocalTime
  }

-- | Creates an environment based on cli args and cache data. The parameter
-- modifies the package set returned by stackage.
setup :: Logging.Handle -> ([Package] -> [Package]) -> IO RunnerEnv
setup hLogger modifyPackages = do
  startTime <- hLogger.getLocalTime
  cliArgs <- Args.getArgs

  -- Set up build args for cabal, filling in missing defaults
  let buildArgs =
        "build"
          : keepGoingArg
          ++ cliArgs.cabalOpts

      -- when packageFailFast is false, add keep-going so that we build as many
      -- packages in the group.
      keepGoingArg = ["--keep-going" | not cliArgs.packageFailFast]

  let cabalPathRaw = fromMaybe [osp|cabal|] cliArgs.cabalPath
  cabalPath <-
    Dir.findExecutable cabalPathRaw >>= \case
      -- TODO: It would be nice to avoid the decode here and keep everything
      -- in OsPath, though that is blocked until process support OsPath.
      Just p -> OsP.decodeUtf p
      Nothing -> Ex.throwText "Cabal not found"

  successesRef <- newIORef Set.empty
  failuresRef <- newIORef Set.empty

  colorLogs <-
    case cliArgs.colorLogs of
      ColorLogsOff -> pure False
      ColorLogsOn -> pure True
      ColorLogsDetect -> supportsPretty

  cache <-
    if cliArgs.noCache
      then pure Nothing
      else Report.readCache hLogger colorLogs

  -- (entire set, packages to build)
  (completePackageSet, pkgsList) <- case cache of
    Nothing -> do
      -- if no cache exists, query stackage
      pkgsResponses <- Parser.getPackageList
      let completePackageSet = responseToPkgs <$> pkgsResponses
          pkgs = modifyPackages completePackageSet
      pure (completePackageSet, pkgs)
    Just oldResults -> do
      -- cache exists, use it rather than stackage
      oldFailures <-
        if cliArgs.retryFailures
          then do
            -- NOTE: [Remove old logs]
            --
            -- Remove previous errors if we are retrying.
            IO.removeDirectoryRecursiveIfExists Paths.logsDir
            pure oldResults.failures
          else pure Set.empty

      let completePackageSet = Report.allPackages oldResults
          untested = oldResults.untested
          toTest = Set.union untested oldFailures

      pure (Set.toList completePackageSet, Set.toList toTest)

  packagesToBuild <- case pkgsList of
    (p : ps) -> pure (p :| ps)
    [] -> do
      Logging.putTimeInfoStr hLogger colorLogs "Cache exists but has no packages to test."
      throwIO ExitSuccess

  let progress =
        MkProgress
          { successesRef,
            failuresRef
          }

      buildEnv =
        MkBuildEnv
          { batch = cliArgs.batch,
            buildArgs,
            cabalPath,
            colorLogs,
            groupFailFast = cliArgs.groupFailFast,
            hLogger,
            packagesToBuild,
            progress,
            writeLogs = cliArgs.writeLogs
          }

  -- delete this if they were leftover from a previous run
  IO.removeFileIfExists Paths.generatedCabalPath
  IO.removeFileIfExists Paths.generatedCabalProjectLocalPath

  pure $
    MkRunnerEnv
      { buildEnv,
        cache,
        completePackageSet,
        noCache = cliArgs.noCache,
        noCleanup = cliArgs.noCleanup,
        retryFailures = cliArgs.retryFailures,
        startTime
      }
  where
    responseToPkgs p =
      MkPackage
        { name = p.name,
          version = p.version
        }

-- | Prints summary and writes results to disk.
teardown :: RunnerEnv -> IO ()
teardown env = do
  endTime <- env.buildEnv.hLogger.getLocalTime
  unless env.noCleanup $ do
    Dir.removeFile Paths.generatedCabalPath
    Dir.removeFile Paths.generatedCabalProjectLocalPath

  results <- getResults env.buildEnv
  let report =
        Report.mkReport
          results
          (Logging.formatLocalTime env.startTime)
          (Logging.formatLocalTime endTime)

  unless env.noCache (updateCache env results)

  Report.saveReport report

  env.buildEnv.hLogger.logStrLn $
    T.unlines
      [ "",
        "",
        Logging.colorGreen env.buildEnv.colorLogs $ "- Successes: " <> successStr report,
        Logging.colorRed env.buildEnv.colorLogs $ "- Failures:  " <> failureStr report,
        Logging.colorMagenta env.buildEnv.colorLogs $ "- Untested:  " <> untestedStr report,
        "",
        Logging.colorBlue env.buildEnv.colorLogs $ "- Start: " <> report.startTime,
        Logging.colorBlue env.buildEnv.colorLogs $ "- End:   " <> report.endTime
      ]
  where
    successStr r = fmtPercent r.stats.numSuccesses r.stats.successRate
    failureStr r = fmtPercent r.stats.numFailures r.stats.failureRate
    untestedStr r = fmtPercent r.stats.numUntested r.stats.untestedRate

    fmtPercent n p =
      mconcat
        [ T.pack $ show n,
          " (",
          T.pack $ show p,
          "%)"
        ]

getResults :: BuildEnv -> IO Results
getResults env = do
  currSuccesses :: Set Package <- readIORef env.progress.successesRef
  currFailures :: Set Package <- readIORef env.progress.failuresRef

  let currAllTested = Set.union currSuccesses currFailures

      currUntested = foldl' addUntested Set.empty env.packagesToBuild

      addUntested acc d =
        if Set.member d currAllTested
          then acc
          else Set.insert d acc

  pure $
    MkResults
      { successes = currSuccesses,
        failures = currFailures,
        untested = currUntested
      }

updateCache :: RunnerEnv -> Results -> IO ()
updateCache env = Report.saveCache . resultsToNewCache env

resultsToNewCache :: RunnerEnv -> Results -> Results
resultsToNewCache env newResults = newCache
  where
    oldCache = fromMaybe Report.emptyResults env.cache
    newCache =
      MkResults
        { -- Successes is append-only.
          successes = Set.union oldCache.successes newResults.successes,
          -- Untested is always the latest as each cached run always adds the
          -- previous untested to the pkgsToBuild.
          untested = newResults.untested,
          failures =
            if env.retryFailures
              then -- Case 1: Retrying previous failures: Then the cache's
              --              results are out-of-date i.e. old failures might
              --              have passed or become untested. Only save the new
              --              results.
                newResults.failures
              else -- Case 2: No retry: total failures are previous + new.
                Set.union oldCache.failures newResults.failures
        }
