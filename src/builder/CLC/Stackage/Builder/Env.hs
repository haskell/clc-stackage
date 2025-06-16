-- | Provides the environment for building.
module CLC.Stackage.Builder.Env
  ( BuildEnv (..),
    Progress (..),
    WriteLogs (..),
  )
where

import CLC.Stackage.Builder.Package (Package)
import CLC.Stackage.Utils.Logging qualified as Logging
import Data.IORef (IORef)
import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)

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
    -- | Optional path to cabal executable.
    cabalPath :: FilePath,
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
