{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Unit.CLC.Stackage.Runner.Env (tests) where

import CLC.Stackage.Runner.Env (RunnerEnv (cache, retryFailures))
import CLC.Stackage.Runner.Env qualified as Env
import CLC.Stackage.Runner.Report
  ( Results (MkResults, failures, successes, untested),
  )
import Data.Set qualified as Set
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "CLC.Stackage.Runner.Env"
    [ testResults,
      newCacheTests
    ]

testResults :: TestTree
testResults = testCase "Retrieves expected results" $ do
  buildEnv <- mkBuildEnv
  results <- Env.getResults buildEnv

  expected @=? results
  where
    expected =
      MkResults
        { successes = Set.fromList ["p1 ==1", "p2 ==1"],
          failures = Set.fromList ["p3 ==1", "p4 ==1"],
          untested = Set.fromList ["p5 ==1", "p6 ==1"]
        }

newCacheTests :: TestTree
newCacheTests =
  testGroup
    "resultsToNewCache"
    [ testEmptyCacheUpdate,
      testCacheUpdate,
      testCacheUpdateRetryFailures
    ]

testEmptyCacheUpdate :: TestTree
testEmptyCacheUpdate = testCase "Empty cache updated to new results" $ do
  runnerEnv <- mkRunnerEnv
  let newResults =
        MkResults
          { successes = Set.fromList ["p1 ==1", "p2 ==1"],
            failures = Set.fromList ["p3 ==1", "p4 ==1"],
            untested = Set.fromList ["p5 ==1", "p6 ==1"]
          }
      newCache = Env.resultsToNewCache runnerEnv newResults

  newResults @=? newCache

testCacheUpdate :: TestTree
testCacheUpdate = testCase "Cache updated to new results" $ do
  runnerEnv <- mkRunnerEnv

  let oldCache =
        MkResults
          { successes = Set.fromList ["p1 ==1"],
            failures = Set.fromList ["p3 ==1"],
            untested = Set.fromList ["p2 ==1", "p4 ==1", "p5 ==1", "p6 ==1"]
          }
      runnerEnv' = runnerEnv {cache = Just oldCache}

  let newResults =
        MkResults
          { successes = Set.fromList ["p2 ==1"],
            failures = Set.fromList ["p4 ==1"],
            untested = Set.fromList ["p5 ==1", "p6 ==1"]
          }
      newCache = Env.resultsToNewCache runnerEnv' newResults

  expected @=? newCache
  where
    expected =
      MkResults
        { successes = Set.fromList ["p1 ==1", "p2 ==1"],
          failures = Set.fromList ["p3 ==1", "p4 ==1"],
          untested = Set.fromList ["p5 ==1", "p6 ==1"]
        }

testCacheUpdateRetryFailures :: TestTree
testCacheUpdateRetryFailures = testCase "Cache updated to new results with retryFailures" $ do
  runnerEnv <- mkRunnerEnv

  let oldCache =
        MkResults
          { successes = Set.fromList ["p1 ==1"],
            failures = Set.fromList ["p3 ==1"],
            untested = Set.fromList ["p2 ==1", "p4 ==1", "p5 ==1", "p6 ==1"]
          }
      runnerEnv' = runnerEnv {cache = Just oldCache, retryFailures = True}

  let newResults =
        MkResults
          { successes = Set.fromList ["p2 ==1"],
            failures = Set.fromList ["p4 ==1"],
            untested = Set.fromList ["p5 ==1", "p6 ==1"]
          }
      newCache = Env.resultsToNewCache runnerEnv' newResults

  expected @=? newCache
  where
    expected =
      MkResults
        { successes = Set.fromList ["p1 ==1", "p2 ==1"],
          failures = Set.fromList ["p4 ==1"],
          untested = Set.fromList ["p5 ==1", "p6 ==1"]
        }
