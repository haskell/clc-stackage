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
    StackageResponse (MkStackageResponse, ghc, packages, snapshot),
  )
import CLC.Stackage.Parser.API.JSON qualified as JSON
import CLC.Stackage.Utils.Exception qualified as Ex
import CLC.Stackage.Utils.Logging qualified as Logging
import Control.Exception (Exception (displayException), throwIO)
import Data.Text qualified as T
import Network.HTTP.Client.TLS qualified as TLS
import System.Exit (ExitCode (ExitFailure))

-- | Returns the 'StackageResponse' corresponding to the given snapshot.
getStackage :: Logging.Handle -> IO StackageResponse
getStackage hLogger = do
  manager <- TLS.newTlsManager
  Ex.tryAny (JSON.getStackage manager stackageSnapshot) >>= \case
    Right r1 -> pure r1
    Left jsonEx -> do
      let e1 =
            mconcat
              [ "Json endpoint failed. Trying cabal config next: ",
                T.pack $ displayException jsonEx
              ]

      Logging.putTimeWarnStr hLogger e1

      Ex.tryAny (CabalConfig.getStackage manager stackageSnapshot) >>= \case
        Right r2 -> pure r2
        Left ex -> do
          let e2 =
                mconcat
                  [ "Cabal config endpoint failed: ",
                    T.pack $ displayException ex
                  ]

          Logging.putTimeErrStr hLogger e2
          throwIO $ ExitFailure 1

-- | Stackage snapshot. Currently just 'nightly' to hopefully allow clc-stackage
-- to be more flexible.
stackageSnapshot :: String
stackageSnapshot = "nightly"
