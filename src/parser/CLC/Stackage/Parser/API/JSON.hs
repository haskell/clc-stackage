module CLC.Stackage.Parser.API.JSON
  ( -- * Query json endpoint
    getStackage,
  )
where

import CLC.Stackage.Parser.API.Common
  ( ExceptionReason (ReasonDecodeJson, ReasonReadBody, ReasonStatus),
    StackageException (MkStackageException),
    getStatusCode,
  )
import CLC.Stackage.Parser.API.Common qualified as Common
import CLC.Stackage.Utils.Exception qualified as Ex
import CLC.Stackage.Utils.JSON qualified as JSON
import CLC.Stackage.Utils.Package qualified as Package
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (BodyReader, Manager, Request, Response)
import Network.HTTP.Client qualified as HttpClient

-- | Given http manager and snapshot string, queries the primary json
-- endpoint.
getStackage :: Manager -> String -> IO Common.StackageResponse
getStackage manager stackageSnapshot = do
  req <- getRequest
  HttpClient.withResponse req manager readStackageResponse
  where
    readStackageResponse :: Response BodyReader -> IO Common.StackageResponse
    readStackageResponse res = do
      let bodyReader = HttpClient.responseBody res
          status = HttpClient.responseStatus res
          statusCode = getStatusCode res
          mkEx = MkStackageException stackageSnapshot

      when (statusCode /= 200) $
        throwIO $
          mkEx (ReasonStatus status)

      bodyBs <-
        Ex.mapThrowLeft
          (mkEx . ReasonReadBody)
          =<< Ex.tryAny (mconcat <$> HttpClient.brConsume bodyReader)

      Ex.mapThrowLeft
        (mkEx . ReasonDecodeJson bodyBs)
        (toSnapshotCommon <$> JSON.decode bodyBs)

    getRequest :: IO Request
    getRequest = updateReq <$> mkReq
      where
        mkReq = HttpClient.parseRequest stackageUrl
        updateReq r =
          r
            { HttpClient.requestHeaders =
                [ ("Accept", "application/json;charset=utf-8,application/json")
                ]
            }

    -- Url for the stackage snapshot.
    stackageUrl :: String
    stackageUrl = "https://stackage.org/" <> stackageSnapshot

toSnapshotCommon :: StackageResponse -> Common.StackageResponse
toSnapshotCommon (MkStackageResponse _ pkgs) =
  Common.MkStackageResponse
    { packages = toPackageCommon <$> pkgs
    }

toPackageCommon :: PackageResponse -> Package.Package
toPackageCommon pr =
  Package.MkPackage
    { name = pr.name,
      version = Package.PackageVersionText pr.version
    }

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
