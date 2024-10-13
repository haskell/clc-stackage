-- | REST API for stackage.org.
module CLC.Stackage.Parser.API
  ( withResponse,
    stackageSnapshot,
  )
where

import Network.HTTP.Client (BodyReader, Request, Response)
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as TLS

-- | Hits the stackage endpoint, invoking the callback on the result.
withResponse :: (Response BodyReader -> IO a) -> IO a
withResponse onResponse = do
  manager <- TLS.newTlsManager
  req <- getRequest
  HttpClient.withResponse req manager onResponse

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

-- | Url for the stackage snapshot.
stackageUrl :: String
stackageUrl = "https://stackage.org/" <> stackageSnapshot

-- | Stackage snapshot.
stackageSnapshot :: String
stackageSnapshot = "nightly-2024-03-26"
