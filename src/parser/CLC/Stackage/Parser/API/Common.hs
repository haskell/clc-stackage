-- | Types and functions common to stackage JSON and CabalConfig APIs.
module CLC.Stackage.Parser.API.Common
  ( -- * Types
    StackageResponse (..),
    PackageResponse (..),

    -- * Exception
    StackageException (..),
    ExceptionReason (..),

    -- * Misc
    getStatusCode,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception
  ( Exception (displayException),
    SomeException,
  )
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding.Error (UnicodeException)
import GHC.Generics (Generic)
import Network.HTTP.Client (Response)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status

-- | Stackage response. This type unifies different stackage responses.
newtype StackageResponse = MkStackageResponse
  { packages :: [PackageResponse]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Package in a stackage snapshot.
data PackageResponse = MkPackageResponse
  { name :: Text,
    version :: Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

-- | Exception reason.
data ExceptionReason
  = -- | Received non-200.
    ReasonStatus Status
  | -- | Exception when reading the body.
    ReasonReadBody SomeException
  | -- | Exception decoding JSON. The first string is the json we attempted
    -- to decode. The second is the error message.
    ReasonDecodeJson ByteString String
  | -- | Exception decoding JSON. The first string is the bytestring we
    -- attempted to decode. The second is the error message.
    ReasonDecodeUtf8 ByteString UnicodeException
  deriving stock (Show)

-- | General network exception.
data StackageException = MkStackageException
  { snapshot :: String,
    reason :: ExceptionReason
  }
  deriving stock (Show)

instance Exception StackageException where
  displayException ex =
    case ex.reason of
      ReasonStatus status ->
        if is404 status
          then
            mconcat
              [ "Received 404 for snapshot '",
                snapshot,
                "'. Is the snapshot correct? ",
                statusMessage status
              ]
          else
            mconcat
              [ "Received ",
                show $ Status.statusCode status,
                " for snapshot '",
                snapshot,
                "'. ",
                statusMessage status
              ]
      ReasonReadBody readBodyEx ->
        mconcat
          [ "Exception reading body for snapshot '",
            snapshot,
            "':\n\n",
            displayException readBodyEx
          ]
      ReasonDecodeJson jsonBs err ->
        mconcat
          [ "Could not decode JSON: ",
            err,
            "This is likely due to the endpoint returning HTML, not JSON. Bytes: ",
            show jsonBs
          ]
      ReasonDecodeUtf8 bs err ->
        mconcat
          [ "Could not decode UTF-8: ",
            displayException err,
            ". Bytes: ",
            show bs
          ]
    where
      snapshot = ex.snapshot
      is404 x = Status.statusCode x == 404

      statusMessage s =
        mconcat
          [ "Status message: ",
            show $ Status.statusMessage s
          ]

getStatusCode :: Response body -> Int
getStatusCode = Status.statusCode . HttpClient.responseStatus
