{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import CLC.Stackage.Runner qualified as Runner
import CLC.Stackage.Utils.IO qualified as IO
import CLC.Stackage.Utils.Logging qualified as Logging
import CLC.Stackage.Utils.OS (Os (Windows), currentOs)
import CLC.Stackage.Utils.Package (Package (name))
import CLC.Stackage.Utils.Paths qualified as Paths
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as L
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import Data.Time.LocalTime (LocalTime (LocalTime), midday)
import System.Environment (lookupEnv, withArgs)
import System.Environment.Guard (guardOrElse')
import System.Environment.Guard.Lifted (ExpectEnv (ExpectEnvSet))
import System.OsPath (OsPath, osp, (</>))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass))
import Test.Utils (goldenDiffCustom)

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.localOption OnPass $
      Tasty.withResource setup (const teardown) specs
  where
    specs getNoCleanup =
      testGroup
        "Functional"
        [ testSmall getNoCleanup,
          testSmallBatch getNoCleanup,
          testSmallSnapshotPath getNoCleanup
        ]

testSmall :: IO Bool -> TestTree
testSmall getNoCleanup = runGolden getNoCleanup params
  where
    params =
      MkGoldenParams
        { args = [],
          runner = runSmall,
          testDesc = "Finishes clc-stackage with small package list",
          testName = [osp|testSmall|]
        }

testSmallBatch :: IO Bool -> TestTree
testSmallBatch getNoCleanup = runGolden getNoCleanup params
  where
    params =
      MkGoldenParams
        { args = ["--batch", "2"],
          runner = runSmall,
          testDesc = "Finishes clc-stackage with small package list and --batch",
          testName = [osp|testSmallBatch|]
        }

testSmallSnapshotPath :: IO Bool -> TestTree
testSmallSnapshotPath getNoCleanup = runGolden getNoCleanup params
  where
    params =
      MkGoldenParams
        { args = ["--snapshot-path", snapshotPath],
          runner = runSmall,
          testDesc,
          testName = [osp|testSmallSnapshotPath|]
        }
    testDesc = "Finishes clc-stackage with small package list and --snapshot-path"

    snapshotPath =
      Paths.unsafeDecodeUtf $
        [osp|test|] </> [osp|functional|] </> [osp|snapshot.txt|]

-- | Tests building only a few packages
runSmall :: IO [ByteString]
runSmall = do
  (hLogger, logsRef) <- mkHLogger

  Runner.runModifyPackages hLogger modifyPackages

  readLogs logsRef
  where
    readLogs = fmap (fmap toBS . L.reverse) . readIORef
    toBS = TEnc.encodeUtf8

modifyPackages :: [Package] -> [Package]
modifyPackages = filter (\p -> Set.member p.name pkgs)
  where
    pkgs =
      -- chosen at semi-random due to small dep footprint + no system deps
      Set.fromList
        [ "cborg",
          "clock",
          "mtl",
          "optics-core",
          "profunctors"
        ]

setup :: IO Bool
setup = do
  IO.removeFileIfExists Paths.generatedCabalPath
  IO.removeFileIfExists Paths.generatedCabalProjectLocalPath
  IO.removeDirectoryRecursiveIfExists Paths.outputDir

  isJust <$> lookupEnv "NO_CLEANUP"

-- NOTE: [Skipping cleanup]
--
-- guardOrElse' will run doNothing over cleanup if NO_CLEANUP is set.
teardown :: IO ()
teardown = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = do
      IO.removeFileIfExists Paths.generatedCabalPath
      IO.removeFileIfExists Paths.generatedCabalProjectLocalPath
      IO.removeDirectoryRecursiveIfExists Paths.outputDir

    doNothing = putStrLn "*** Not cleaning up output or generated dir"

mkHLogger :: IO (Logging.Handle, IORef [Text])
mkHLogger = do
  logsRef <- newIORef []

  let hLogger =
        Logging.MkHandle
          { Logging.color = False,
            Logging.getLocalTime = pure mkLocalTime,
            Logging.logStrLn = \s -> modifyIORef' logsRef (s :),
            Logging.logStrErrLn = \s -> modifyIORef' logsRef (s :),
            Logging.terminalWidth = 80
          }

  pure (hLogger, logsRef)

mkLocalTime :: LocalTime
mkLocalTime = LocalTime (toEnum 59_000) midday

data GoldenParams = MkGoldenParams
  { args :: [String],
    runner :: IO [ByteString],
    testDesc :: TestName,
    testName :: OsPath
  }

runGolden :: IO Bool -> GoldenParams -> TestTree
runGolden getNoCleanup params =
  goldenDiffCustom params.testDesc goldenFilePath actualFilePath $ do
    -- we always need to cleanup the cache prior to a run so that the generated
    -- cache from a previous run does not interfere
    IO.removeFileIfExists Paths.cachePath

    -- While NOTE: [Skipping cleanup] will prevent the test cleanup from running,
    -- the clc-stackage also performs a cleanup. Thus if no cleanup is desired
    -- (NO_CLEANUP is set), we also need to pass the --no-cleanup arg to the
    -- exe.
    noCleanup <- getNoCleanup
    let noCleanupArgs = ["--no-cleanup" | noCleanup]
        finalArgs = args' ++ noCleanupArgs

    logs <- withArgs finalArgs params.runner

    writeActualFile $ toBS logs
  where
    -- test w/ color off since CI can't handle it, apparently
    args' = "--color-logs" : "off" : params.args

    baseTestPath =
      goldensDir
        </> params.testName
        <> ext

    actualOsPath = baseTestPath <> [osp|.actual|]
    actualFilePath = Paths.unsafeDecodeUtf actualOsPath
    goldenFilePath = Paths.unsafeDecodeUtf $ baseTestPath <> [osp|.golden|]

    toBS = C8.unlines

    writeActualFile :: ByteString -> IO ()
    writeActualFile = IO.writeBinaryFile actualOsPath

goldensDir :: OsPath
goldensDir = [osp|test|] </> [osp|functional|] </> [osp|goldens|]

ext :: OsPath
ext = case currentOs of
  Windows -> [osp|_windows|]
  _ -> [osp|_posix|]
