{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Unit.CLC.Stackage.Runner.Report (tests) where

import CLC.Stackage.Runner.Report
  ( Report (MkReport, endTime, results, startTime, stats),
    Results (MkResults, failures, successes, untested),
    Stats
      ( MkStats,
        failureRate,
        numFailures,
        numSuccesses,
        numUntested,
        successRate,
        untestedRate
      ),
  )
import CLC.Stackage.Runner.Report qualified as Report
import CLC.Stackage.Utils.IO qualified as IO
import CLC.Stackage.Utils.JSON qualified as JSON
import CLC.Stackage.Utils.Paths qualified as Paths
import Data.Set qualified as Set
import System.OsPath (OsPath, osp, (</>))
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "CLC.Stackage.Runner.Report"
    [ testMkReport,
      testResultJsonEncode,
      testReportJsonEncode
    ]

testMkReport :: TestTree
testMkReport = testCase "Creates a report" $ do
  let report = Report.mkReport results "start" "end"

  expected @=? report
  where
    results =
      MkResults
        { successes = Set.fromList ["p1 ==1", "p2 ==1", "p3 ==1", "p4 ==1", "p5 ==1"],
          failures = Set.fromList ["p4 ==1", "p5 ==1"],
          untested = Set.fromList ["p6 ==1", "p7 ==1", "p8 ==1"]
        }

    expected =
      MkReport
        { results = results,
          stats =
            MkStats
              { numSuccesses = 5,
                successRate = 50,
                numFailures = 2,
                failureRate = 20,
                numUntested = 3,
                untestedRate = 30
              },
          startTime = "start",
          endTime = "end"
        }

testResultJsonEncode :: TestTree
testResultJsonEncode = goldenDiffCustom desc goldenFilePath actualFilePath $ do
  let json = JSON.encodePretty results <> "\n"

  IO.writeBinaryFile actualOsPath json
  where
    desc = "Encodes Result to JSON"

    goldenFilePath = Paths.unsafeDecodeUtf $ goldenDir </> [osp|result.golden|]
    actualOsPath = goldenDir </> [osp|result.actual|]
    actualFilePath = Paths.unsafeDecodeUtf actualOsPath

    results =
      MkResults
        { successes = Set.fromList ["p1 ==1", "p2 ==1", "p3 ==1", "p4 ==1", "p5 ==1"],
          failures = Set.fromList ["p4 ==1", "p5 ==1"],
          untested = Set.fromList ["p6 ==1", "p7 ==1", "p8 ==1"]
        }

testReportJsonEncode :: TestTree
testReportJsonEncode = goldenDiffCustom desc goldenFilePath actualFilePath $ do
  let json = JSON.encodePretty report <> "\n"

  IO.writeBinaryFile actualOsPath json
  where
    desc = "Encodes Report to JSON"

    goldenFilePath = Paths.unsafeDecodeUtf $ goldenDir </> [osp|report.golden|]
    actualOsPath = goldenDir </> [osp|report.actual|]
    actualFilePath = Paths.unsafeDecodeUtf actualOsPath

    results =
      MkResults
        { successes = Set.fromList ["p1 ==1", "p2 ==1", "p3 ==1", "p4 ==1", "p5 ==1"],
          failures = Set.fromList ["p4 ==1", "p5 ==1"],
          untested = Set.fromList ["p6 ==1", "p7 ==1", "p8 ==1"]
        }

    report =
      MkReport
        { results = results,
          stats =
            MkStats
              { numSuccesses = 5,
                successRate = 50,
                numFailures = 2,
                failureRate = 20,
                numUntested = 3,
                untestedRate = 30
              },
          startTime = "start",
          endTime = "end"
        }

goldenDir :: OsPath
goldenDir = [osp|test/|] </> [osp|unit|] </> [osp|goldens|]
