{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import CLC.Stackage.Parser.Utils qualified as Parser.Utils
import CLC.Stackage.Runner qualified as Runner
import CLC.Stackage.Utils.IO qualified as IO
import CLC.Stackage.Utils.Logging qualified as Logging
import CLC.Stackage.Utils.OS (Os (Linux, Osx, Windows), currentOs)
import CLC.Stackage.Utils.Package (Package (name))
import CLC.Stackage.Utils.Paths qualified as Paths
import Control.Applicative (asum)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C8
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List qualified as L
import Data.Maybe (fromMaybe, isJust)
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
          "extra",
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
    let logs' = filter (not . skipLog) $ fmap massageLogs logs

    writeActualFile $ toBS logs'
  where
    -- Logs whose presence is non-deterministic i.e. need to be removed
    -- entirely.
    skipLog = BS.isInfixOf "PATH and Stackage ghc"

    -- Strip non-determinism from logs (e.g. version numbers, snapshots).
    -- At most one of these should match. If none do, we return the original
    -- string.
    massageLogs bs =
      fromMaybe bs $
        asum $
          fmap
            ($ bs)
            [ fixGhcStr,
              fixSnapshotStr,
              fixNumPkgs,
              fixLibs
            ]
      where
        fixGhcStr b = do
          (pre, r1) <- Parser.Utils.stripInfix "ghc: " b
          let (_vers, rest) = BS.break (== Parser.Utils.spaceW8) r1
          pure $ pre <> "ghc: <version>" <> rest

        fixSnapshotStr b = do
          (pre, r1) <- Parser.Utils.stripInfix "Snapshot: " b
          let (_vers, r2) = BS.break (== Parser.Utils.spaceW8) r1
              rest = fromMaybe r2 (fixNumPkgs r2)
          pure $ pre <> "Snapshot: <snapshot>" <> rest

        fixNumPkgs b = do
          (r1, post) <- Parser.Utils.stripInfix " packages" b
          let (pre, _num) = BS.breakEnd (not . Parser.Utils.isNum) r1
          pure $ pre <> "<num> packages" <> post

        -- Idea: For a given bytestring, try to find an expected lib string
        -- e.g. 'aeson'. If we find it, we place the version number with
        -- '<vers>', then recursively run on the rest of the string.
        fixLibs :: ByteString -> Maybe ByteString
        fixLibs b = do
          (p1, r1) <- fixLib b
          pure $ p1 <> fromMaybe r1 (fixLibs r1)
          where
            fixLib :: ByteString -> Maybe (ByteString, ByteString)
            fixLib c = asum $ fmap (tryLib c) libs

            libs =
              [ "aeson",
                "cborg",
                "clock",
                "extra",
                "kan-extensions",
                "mtl",
                "optics-core",
                "profunctors",
                "servant"
              ]

            -- E.g. tryLib "abc lib-1.2.3 def" "lib"
            -- Just ("abc lib-<vers>"," def")
            tryLib :: ByteString -> ByteString -> Maybe (ByteString, ByteString)
            tryLib c lib = do
              (pre, r1) <- Parser.Utils.stripInfix lib c
              let (_vers, rest) = BS.break isDelim r1
              pure (pre <> lib <> "-<vers>", rest)
              where
                isDelim d =
                  d == Parser.Utils.commaW8
                    || d == Parser.Utils.spaceW8

    -- test w/ color off since CI can't handle it, apparently
    args' =
      "--color-logs"
        : "off"
        : "--no-cabal-update"
        : params.args

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
  Linux -> [osp|_linux|]
  Osx -> [osp|_osx|]
  Windows -> [osp|_windows|]
