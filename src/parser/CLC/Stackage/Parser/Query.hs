{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module CLC.Stackage.Parser.Query
  ( -- * Querying stackage
    getStackage,

    -- ** Exceptions
    StackageException (..),
    ExceptionReason (..),
  )
where

import CLC.Stackage.Parser.API
  ( stackageSnapshot,
    withResponse,
  )
import CLC.Stackage.Parser.Data.Response (StackageResponse)
import CLC.Stackage.Utils.Exception qualified as Ex
import CLC.Stackage.Utils.JSON qualified as JSON
import Control.Exception
  ( Exception (displayException),
    SomeException,
    throwIO,
  )
import Control.Monad (when)
import Data.ByteString (ByteString)
import Network.HTTP.Client (Response)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status

-- | Returns the 'StackageResponse' corresponding to the given snapshot.
getStackage :: IO StackageResponse
getStackage = withResponse $ \res -> do
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
    (JSON.decode bodyBs)

-- | Exception reason.
data ExceptionReason
  = -- | Received non-200.
    ReasonStatus Status
  | -- | Exception when reading the body.
    ReasonReadBody SomeException
  | -- | Exception decoding JSON. The first string is the json we attempted
    -- to decode. The second is the error message.
    ReasonDecodeJson ByteString String
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
          [ "Could not decode JSON:\n\n",
            show jsonBs,
            "\n\nError: ",
            err
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
