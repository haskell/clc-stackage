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

-- | Stackage snapshot. Note that picking a "good" snapshot is something of
-- an art i.e. not all valid snapshots return json output at the
-- expected endpoint. I essentially try snapshots with
--
--    curl -H "Accept: application/json" -L https://stackage.org/nightly-yyyy-mm-dd
--
-- until one returns json.
stackageSnapshot :: String
stackageSnapshot = "nightly-2025-05-23"
