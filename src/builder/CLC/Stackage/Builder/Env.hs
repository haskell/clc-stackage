-- | Provides the environment for building.
module CLC.Stackage.Builder.Env
  ( BuildEnv (..),
    CabalVerbosity (..),
    cabalVerbosityToArg,
    Jobs (..),
    jobsToArg,
    Progress (..),
    WriteLogs (..),
  )
where

import CLC.Stackage.Builder.Package (Package)
import CLC.Stackage.Utils.Logging qualified as Logging
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Data.Word (Word8)

-- | Cabal's --verbose flag
data CabalVerbosity
  = -- | V0
    CabalVerbosity0
  | -- | V1
    CabalVerbosity1
  | -- | V2
    CabalVerbosity2
  | -- | V3
    CabalVerbosity3
  deriving stock (Eq, Show)

cabalVerbosityToArg :: CabalVerbosity -> String
cabalVerbosityToArg CabalVerbosity0 = "--verbose=0"
cabalVerbosityToArg CabalVerbosity1 = "--verbose=1"
cabalVerbosityToArg CabalVerbosity2 = "--verbose=2"
cabalVerbosityToArg CabalVerbosity3 = "--verbose=3"

-- | Number of build jobs.
data Jobs
  = -- | Literal number of jobs.
    JobsN Word8
  | -- | String "$ncpus"
    JobsNCpus
  | -- | Job semaphore. Requires GHC 9.8 and Cabal 3.12
    JobsSemaphore
  deriving stock (Eq, Show)

jobsToArg :: Jobs -> String
jobsToArg (JobsN n) = "--jobs=" ++ show n
jobsToArg JobsNCpus = "--jobs=$ncpus"
jobsToArg JobsSemaphore = "--semaphore"

data Progress = MkProgress
  { -- | Dependencies that built successfully.
    successesRef :: IORef (Set Package),
    -- | Dependencies that failed to build.
    failuresRef :: IORef (Set Package)
  }

-- | Determines what cabal output to write
data WriteLogs
  = -- | No logs written.
    WriteLogsNone
  | -- | Current logs written (overwritten).
    WriteLogsCurrent
  | -- | Current logs written with failures saved.
    WriteLogsSaveFailures
  deriving stock (Eq, Show)

-- | Environment for the builder.
data BuildEnv = MkBuildEnv
  { -- | If we have @Just n@, 'packagesToBuild' will be split into groups of at most
    -- size @n@. If @Nothing@, the entire set will be built in one go.
    batch :: Maybe Int,
    -- | Build arguments for cabal.
    buildArgs :: [String],
    -- | If true, colors logs.
    colorLogs :: Bool,
    -- | If true, the first group that fails to completely build stops
    -- clc-stackage. Defaults to false.
    groupFailFast :: Bool,
    -- | Logging handler
    hLogger :: Logging.Handle,
    -- | All packages that are to be built during the entire run. This may
    -- be split into groups, if 'batch' exists.
    packagesToBuild :: NonEmpty Package,
    -- | Status for this run.
    progress :: Progress,
    -- | Determines logging behavior.
    writeLogs :: Maybe WriteLogs
  }
