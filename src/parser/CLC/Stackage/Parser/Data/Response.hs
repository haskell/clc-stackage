-- | Types returned by stackage API.
module CLC.Stackage.Parser.Data.Response
  ( StackageResponse (..),
    SnapshotResponse (..),
    PackageResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Response returned by primary stackage endpoint e.g.
-- @stackage.org\/lts-20.14@.
data StackageResponse = MkStackageResponse
  { snapshot :: SnapshotResponse,
    packages :: [PackageResponse]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Stackage snapshot data.
data SnapshotResponse = MkSnapshotResponse
  { ghc :: Text,
    created :: Text,
    name :: Text,
    compiler :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Package in a stackage snapshot.
data PackageResponse = MkPackageResponse
  { origin :: Text,
    name :: Text,
    version :: Text,
    synopsis :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
