-- | Entry-point for the project. Provides libraries functions over a mere
-- executable for testing.
module CLC.Stackage.Runner
  ( run,
    runModifyPackages,
  )
where

import CLC.Stackage.Builder qualified as Builder
import CLC.Stackage.Builder.Env
  ( BuildEnv (progress),
    Progress (failuresRef),
  )
import CLC.Stackage.Builder.Writer qualified as Writer
import CLC.Stackage.Runner.Env (RunnerEnv (completePackageSet))
import CLC.Stackage.Runner.Env qualified as Env
import CLC.Stackage.Utils.Logging qualified as Logging
import CLC.Stackage.Utils.Package (Package)
import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import Data.Foldable (for_)
import Data.IORef (readIORef)
import System.Exit (ExitCode (ExitFailure))

-- | Entry-point for testing clc-stackage. In particular:
--
-- 1. Sets up environment based on CLI args and possible cache data from a
--    previous run.
--
-- 2. For each group of packages, write a cabal file for the group and attempt
--    a build.
--
-- 3. Once all groups are finished (or the first failure, if
--    'CLC.Stackage.Builder.Env.failFast' is active), print a summary and
--    update the cache if 'CLC.Stackage.Runner.Env.noCache' is /inactive/.
run :: Logging.Handle -> IO ()
run hLogger = runModifyPackages hLogger id

-- | Like 'run', except takes a package modifier. This is used for testing, so
-- that we can whittle down the (very large) package set.
runModifyPackages :: Logging.Handle -> ([Package] -> [Package]) -> IO ()
runModifyPackages hLogger modifyPackages = do
  bracket (Env.setup hLogger modifyPackages) Env.teardown $ \env -> do
    let buildEnv = env.buildEnv
        pkgGroupsIdx = Builder.batchPackages buildEnv

    -- write the entire package set to the cabal.project.local's constraints
    Writer.writeCabalProjectLocal env.completePackageSet

    for_ pkgGroupsIdx $ \(pkgGroup, idx) -> Builder.buildProject buildEnv idx pkgGroup

    numErrors <- length <$> readIORef buildEnv.progress.failuresRef
    when (numErrors > 0) $ throwIO $ ExitFailure 1
