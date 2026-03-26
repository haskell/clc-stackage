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
    StackageResponse (MkStackageResponse, ghc, packages, snapshot),
    getStatusCode,
  )
import CLC.Stackage.Utils.Exception qualified as Ex
import CLC.Stackage.Utils.Package qualified as Package
import Control.Applicative (asum, (<|>))
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Foldable qualified as F
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
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
parseCabalConfig txt =
  MkStackageResponse
    { ghc = acc.ghc,
      packages = F.toList acc.packages,
      snapshot = acc.snapshot
    }
  where
    acc = foldMap parseLine (T.lines txt)

data Acc = MkAcc
  { ghc :: Maybe Text,
    packages :: Seq Package.Package,
    snapshot :: Maybe Text
  }

instance Semigroup Acc where
  MkAcc x1 x2 x3 <> MkAcc y1 y2 y3 =
    MkAcc (x1 <|> y1) (x2 <> y2) (x3 <|> y3)

instance Monoid Acc where
  mempty = MkAcc Nothing Seq.empty Nothing

parseLine :: Text -> Acc
parseLine txt =
  fromMaybe mempty
    . asum
    $ ($ txt)
      <$> [ parsePackage,
            parseGhc,
            parseSnapshot
          ]

-- | Parses a line like 'with-compiler: ghc-9.12.3.
parseGhc :: Text -> Maybe Acc
parseGhc txt = do
  ghc <- stripInfixSnd "compiler: ghc-" txt
  pure $
    MkAcc
      { ghc = Just ghc,
        packages = Seq.empty,
        snapshot = Nothing
      }

-- | Parses a line like '<pkg> ==<vers>'.
parsePackage :: Text -> Maybe Acc
parsePackage txt = do
  -- Strip leading 'constraints:' keyword, if it exists.
  let s = maybe txt' T.stripStart (T.stripPrefix "constraints:" txt')
  p <- Package.fromCabalConstraintsText s
  pure $
    MkAcc
      { ghc = Nothing,
        packages = Seq.singleton p,
        snapshot = Nothing
      }
  where
    txt' = T.stripStart txt

-- | Parses a line like:
--
-- 'Stackage snapshot from: http://www.stackage.org/snapshot/nightly-2026-03-25'
parseSnapshot :: Text -> Maybe Acc
parseSnapshot txt = do
  snapshot <- stripInfixSnd "http://www.stackage.org/snapshot/" txt
  pure $
    MkAcc
      { ghc = Nothing,
        packages = Seq.empty,
        snapshot = Just snapshot
      }

stripInfixSnd :: Text -> Text -> Maybe Text
stripInfixSnd t1 = fmap snd . stripInfix t1

stripInfix :: Text -> Text -> Maybe (Text, Text)
stripInfix t1 t2 = (pre,) <$> T.stripPrefix t1 rest
  where
    (pre, rest) = T.breakOn t1 t2
