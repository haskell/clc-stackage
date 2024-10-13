module Unit.Prelude
  ( module X,
    mkRunnerEnv,
    mkBuildEnv,
  )
where

import CLC.Stackage.Builder.Env
  ( BuildEnv
      ( MkBuildEnv,
        batch,
        buildArgs,
        colorLogs,
        groupFailFast,
        hLogger,
        packagesToBuild,
        progress,
        writeLogs
      ),
    Progress (MkProgress, failuresRef, successesRef),
  )
import CLC.Stackage.Runner.Env
  ( RunnerEnv
      ( MkRunnerEnv,
        buildEnv,
        cache,
        completePackageSet,
        noCache,
        noCleanup,
        retryFailures,
        startTime
      ),
  )
import CLC.Stackage.Utils.Logging qualified as Logging
import Data.IORef (newIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Time.LocalTime (LocalTime (LocalTime), midday)
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.Golden as X (goldenVsFile)
import Test.Tasty.HUnit as X (assertFailure, testCase, (@=?))

mkRunnerEnv :: IO RunnerEnv
mkRunnerEnv = do
  buildEnv <- mkBuildEnv

  pure $
    MkRunnerEnv
      { buildEnv,
        cache = Nothing,
        completePackageSet = NE.toList buildEnv.packagesToBuild,
        noCache = False,
        noCleanup = False,
        retryFailures = False,
        startTime = mkLocalTime
      }

mkBuildEnv :: IO BuildEnv
mkBuildEnv = do
  successesRef <- newIORef (Set.fromList ["p1 ==1", "p2 ==1"])
  failuresRef <- newIORef (Set.fromList ["p3 ==1", "p4 ==1"])

  pure $
    MkBuildEnv
      { batch = Nothing,
        buildArgs = [],
        colorLogs = True,
        groupFailFast = False,
        hLogger =
          Logging.MkHandle
            { getLocalTime = pure mkLocalTime,
              logStrErrLn = const (pure ()),
              logStrLn = const (pure ()),
              terminalWidth = 80
            },
        packagesToBuild = "p1 ==1" :| ["p2 ==1", "p3 ==1", "p4 ==1", "p5 ==1", "p6 ==1"],
        progress =
          MkProgress
            { failuresRef,
              successesRef
            },
        writeLogs = Nothing
      }

mkLocalTime :: LocalTime
mkLocalTime = LocalTime (toEnum 59_000) midday
