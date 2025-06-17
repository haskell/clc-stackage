-- | REST API for stackage.org.
module CLC.Stackage.Parser.API
  ( -- * Querying stackage
    StackageResponse (..),
    getStackage,

    -- ** Exceptions
    StackageException (..),
    ExceptionReason (..),

    -- * Misc
    stackageSnapshot,
  )
where

import CLC.Stackage.Parser.API.CabalConfig qualified as CabalConfig
import CLC.Stackage.Parser.API.Common
  ( ExceptionReason
      ( ReasonDecodeJson,
        ReasonDecodeUtf8,
        ReasonReadBody,
        ReasonStatus
      ),
    StackageException (MkStackageException),
    StackageResponse (MkStackageResponse, packages),
  )
import CLC.Stackage.Parser.API.JSON qualified as JSON
import CLC.Stackage.Utils.Exception qualified as Ex
import CLC.Stackage.Utils.Logging qualified as Logging
import Control.Exception (Exception (displayException))
import Data.Text qualified as T
import Network.HTTP.Client.TLS qualified as TLS

-- | Returns the 'StackageResponse' corresponding to the given snapshot.
getStackage :: Logging.Handle -> IO StackageResponse
getStackage hLogger = do
  manager <- TLS.newTlsManager
  Ex.tryAny (JSON.getStackage manager stackageSnapshot) >>= \case
    Right r1 -> pure $ r1
    Left jsonEx -> do
      let msg =
            mconcat
              [ "Json endpoint failed. Trying cabal config next: ",
                T.pack $ displayException jsonEx
              ]

      Logging.putTimeWarnStr hLogger msg

      CabalConfig.getStackage manager stackageSnapshot

-- | Stackage snapshot. Note that picking a "good" snapshot is something of
-- an art i.e. not all valid snapshots return json output at the
-- expected endpoint. I essentially try snapshots with
--
--    curl -H "Accept: application/json" -L https://stackage.org/nightly-yyyy-mm-dd
--
-- until one returns json.
stackageSnapshot :: String
stackageSnapshot = "nightly-2025-05-23"
