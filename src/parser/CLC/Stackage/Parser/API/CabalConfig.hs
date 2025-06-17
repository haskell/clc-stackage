module CLC.Stackage.Parser.API.CabalConfig
  ( -- * Primary
    getStackage,

    -- * Misc
    parseCabalConfig,
  )
where

import CLC.Stackage.Parser.API.Common
  ( ExceptionReason
      ( ReasonDecodeUtf8,
        ReasonReadBody,
        ReasonStatus
      ),
    StackageException (MkStackageException),
    StackageResponse (MkStackageResponse),
    getStatusCode,
  )
import CLC.Stackage.Utils.Exception qualified as Ex
import CLC.Stackage.Utils.Package qualified as Package
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Network.HTTP.Client (BodyReader, Manager, Request, Response)
import Network.HTTP.Client qualified as HttpClient

-- | Given http manager and snapshot string, queries the cabal config
-- endpoint. This is intended as a backup, for when the primary endpoint fails.
getStackage :: Manager -> String -> IO StackageResponse
getStackage manager stackageSnapshot = do
  req <- getRequest
  HttpClient.withResponse req manager readStackageResponse
  where
    readStackageResponse :: Response BodyReader -> IO StackageResponse
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

      bodyTxt <-
        Ex.mapThrowLeft
          (mkEx . ReasonDecodeUtf8 bodyBs)
          $ TEnc.decodeUtf8' bodyBs

      pure $ parseCabalConfig bodyTxt

    getRequest :: IO Request
    getRequest = HttpClient.parseRequest stackageUrl

    -- Url for the stackage snapshot.
    stackageUrl :: String
    stackageUrl =
      "https://stackage.org/"
        <> stackageSnapshot
        <> "/cabal.config"

parseCabalConfig :: Text -> StackageResponse
parseCabalConfig =
  MkStackageResponse
    . catMaybes
    . fmap parseCabalConfigLine
    . T.lines

-- | Parses a line like '<pkg> ==<vers>'.
parseCabalConfigLine :: Text -> Maybe Package.Package
parseCabalConfigLine txt = do
  -- Strip leading 'constraints:' keyword, if it exists.
  let s = case T.stripPrefix "constraints:" txt' of
        Nothing -> txt'
        Just rest -> T.stripStart rest
  Package.packageParser s
  where
    txt' = T.stripStart txt
