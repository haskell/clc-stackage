{-# LANGUAGE QuasiQuotes #-}

module CLC.Stackage.Builder.Process
  ( buildProject,
  )
where

import CLC.Stackage.Builder.Batch (PackageGroup (unPackageGroup))
import CLC.Stackage.Builder.Env
  ( BuildEnv
      ( cabalPath,
        groupFailFast,
        hLogger,
        progress,
        writeLogs
      ),
    Progress (failuresRef, successesRef),
    WriteLogs (WriteLogsCurrent, WriteLogsNone, WriteLogsSaveFailures),
  )
import CLC.Stackage.Builder.Env qualified as Env
import CLC.Stackage.Builder.Writer qualified as Writer
import CLC.Stackage.Utils.IO qualified as IO
import CLC.Stackage.Utils.Logging qualified as Logging
import CLC.Stackage.Utils.Package qualified as Package
import CLC.Stackage.Utils.Paths qualified as Paths
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.IORef (modifyIORef')
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text qualified as T
import System.Directory.OsPath qualified as Dir
import System.Directory.OsPath qualified as OsPath
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.OsPath (OsPath, osp, (</>))
import System.Process qualified as P

-- | Given the build environment, index, and package group, writes the package
-- group to the cabal file and attempts to build it. The index is for logging.
buildProject :: BuildEnv -> Int -> PackageGroup -> IO ()
buildProject env idx pkgs = do
  -- write the package group to the cabal file
  Writer.writeCabal pkgs

  let buildNoLogs :: IO ExitCode
      buildNoLogs =
        withGeneratedDir $
          (\(ec, _, _) -> ec) <$> P.readProcessWithExitCode env.cabalPath env.buildArgs ""

      buildLogs :: Bool -> IO ExitCode
      buildLogs saveFailures = do
        (dirPath, stdoutPath, stderrPath) <- createCurrentLogsDir

        IO.withBinaryFileWriteMode stdoutPath $ \stdoutHandle ->
          IO.withBinaryFileWriteMode stderrPath $ \stderrHandle -> do
            let createProc = P.proc env.cabalPath env.buildArgs
                createProc' =
                  createProc
                    { P.std_out = P.UseHandle stdoutHandle,
                      P.std_err = P.UseHandle stderrHandle,
                      P.close_fds = False
                    }

            ec <-
              withGeneratedDir $
                P.withCreateProcess createProc' (\_ _ _ ph -> P.waitForProcess ph)
            case ec of
              -- Build dir will be overwritten next run anyway, so no need
              -- to do anything
              ExitSuccess -> pure ()
              -- Rename build dir to pkg specific name
              ExitFailure _ -> when saveFailures $ do
                pkgsDirPath <- getPkgGroupLogsDirPath pkgs

                -- Unnecessary with "normal" usage, as the only time
                -- pkgsDirPath exists is if we are retrying a previous failure,
                -- in which case the entire logs dir would have been deleted
                -- on startup (see NOTE: [Remove old logs]).
                --
                -- That said, it is occasionally useful to manually manipulate
                -- the cache e.g. we want to retry a single package, so we
                -- manually move the cache entry from 'failed' to 'untested'.
                --
                -- In this case, the directory will not have been deleted,
                -- hence we do it here.
                IO.removeDirectoryRecursiveIfExists pkgsDirPath

                Dir.renamePath dirPath pkgsDirPath

            pure ec

  exitCode <-
    case env.writeLogs of
      Just WriteLogsNone -> buildNoLogs
      Just WriteLogsCurrent -> buildLogs False
      Just WriteLogsSaveFailures -> buildLogs True
      Nothing -> buildLogs True

  case exitCode of
    ExitSuccess -> do
      -- save results
      modifyIORef' env.progress.successesRef addPackages
      Logging.putTimeSuccessStr env.hLogger msg
    ExitFailure _ -> do
      -- save results
      modifyIORef' env.progress.failuresRef addPackages
      Logging.putTimeErrStr env.hLogger msg

      -- throw error if fail fast
      when env.groupFailFast $ throwIO exitCode
  where
    withGeneratedDir = OsPath.withCurrentDirectory Paths.generatedDir

    msg =
      mconcat
        [ T.pack $ show idx,
          ": ",
          T.intercalate ", " (Package.toTextInstalled <$> pkgsList)
        ]
    pkgsList = NE.toList pkgs.unPackageGroup
    pkgsSet = Set.fromList pkgsList

    addPackages = Set.union pkgsSet

createCurrentLogsDir :: IO (OsPath, OsPath, OsPath)
createCurrentLogsDir = do
  let dirPath = Paths.logsDir </> [osp|current-build|]
      stdoutPath = dirPath </> [osp|stdout.log|]
      stderrPath = dirPath </> [osp|stderr.log|]

  Dir.createDirectoryIfMissing True dirPath
  pure (dirPath, stdoutPath, stderrPath)

-- Name the dir after the first package in the group
getPkgGroupLogsDirPath :: PackageGroup -> IO OsPath
getPkgGroupLogsDirPath pkgs = do
  dirName <- Package.toDirName . NE.head $ pkgs.unPackageGroup
  pure $ Paths.logsDir </> dirName
