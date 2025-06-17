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
    PackageResponse
      ( MkPackageResponse,
        name,
        version
      ),
    StackageException (MkStackageException),
    StackageResponse (MkStackageResponse),
    getStatusCode,
  )
import CLC.Stackage.Utils.Exception qualified as Ex
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.List qualified as L
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

-- | Parses a line like '<pkg> ==<vers>'. This does not currently handle
-- "installed" packages e.g. 'mtl installed'. This probably isn't a big deal,
-- since all such libs will be built transitively anyway. That said, if
-- we wanted to fix it, we would probably want to change PackageResponse's
--
--   version :: Text
--
-- field to
--
--   version :: Maybe Text
--
-- and parse "installed" to Nothing. Then, when we go to write the generated
-- cabal file, Nothing will correspond to writing no version number.
-- (CLC.Stackage.Builder.Package.toText).
parseCabalConfigLine :: Text -> Maybe PackageResponse
-- splitOn rather than breakOn since the former drops the delim, which is
-- convenient.
parseCabalConfigLine txt = case T.splitOn delim txt of
  [nameRaw, versRaw] -> do
    (v, c) <- T.unsnoc versRaw
    -- Strip trailing comma if it exists. Otherwise take everything.
    let version = if c == ',' then v else T.snoc v c

    -- This line handles prefixes e.g. whitespace or a stanza e.g.
    --
    --     constraints: abstract-deque ==0.3,
    --                  abstract-deque-tests ==0.3,
    --                  ...
    --
    -- We split pre-delim on whitespace, and take the last word.
    (_, name) <- L.unsnoc $ T.words nameRaw

    -- T.strip as trailing characters can cause problems e.g. windows can
    -- pick up \r.
    Just $
      MkPackageResponse
        { name = T.strip name,
          version = T.strip version
        }
  _ -> Nothing
  where
    delim = " =="
