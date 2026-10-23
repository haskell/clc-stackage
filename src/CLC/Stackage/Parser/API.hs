-- | REST API for stackage.org.
module CLC.Stackage.Parser.API
  ( -- * Querying stackage
    StackageResponse (..),
    getStackage,

    -- ** Exceptions
    StackageException (..),
    ExceptionReason (..),

    -- * Misc
    defaultSnapshotUrl,
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
    Url (MkUrl),
  )
import CLC.Stackage.Parser.API.JSON qualified as JSON
import CLC.Stackage.Utils.Exception qualified as Ex
import CLC.Stackage.Utils.Logging qualified as Logging
import Control.Exception (Exception (displayException), throwIO)
import Data.List qualified as L
import Data.Text qualified as T
import Network.HTTP.Client.TLS qualified as TLS
import System.Exit (ExitCode (ExitFailure))

-- | Returns the 'StackageResponse' corresponding to the given snapshot.
getStackage :: Logging.Handle -> Maybe String -> IO StackageResponse
getStackage hLogger msnapshotUrl = do
  manager <- TLS.newTlsManager
  Ex.tryAny (JSON.getStackage manager snapshotUrl) >>= \case
    Right r1 -> pure r1
    Left jsonEx -> do
      let e1 =
            mconcat
              [ "Json endpoint failed. Trying cabal config next: ",
                T.pack $ displayException jsonEx
              ]

      Logging.putTimeWarnStr hLogger e1

      Ex.tryAny (CabalConfig.getStackage manager snapshotUrl) >>= \case
        Right r2 -> pure r2
        Left ex -> do
          let e2 =
                mconcat
                  [ "Cabal config endpoint failed: ",
                    T.pack $ displayException ex
                  ]

          Logging.putTimeErrLongStr hLogger e2
          throwIO $ ExitFailure 1
  where
    snapshotUrl = mkStackageUrl msnapshotUrl

-- | Stackage snapshot. Nightly is often okay, but we seem to run into
-- issues a few times a month.
defaultSnapshotUrl :: Url
defaultSnapshotUrl = MkUrl $ stackageDomain <> defaultSnapshot

defaultSnapshot :: String
defaultSnapshot = "nightly"

mkStackageUrl :: Maybe String -> Url
mkStackageUrl Nothing = defaultSnapshotUrl
mkStackageUrl (Just s)
  | "lts" `L.isPrefixOf` s = MkUrl $ stackageDomain <> s
  | "nightly" `L.isPrefixOf` s = MkUrl $ stackageDomain <> s
  | otherwise = MkUrl s

stackageDomain :: String
stackageDomain = "https://stackage.org/"
