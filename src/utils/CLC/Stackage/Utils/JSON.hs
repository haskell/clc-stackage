module CLC.Stackage.Utils.JSON
  ( writeJson,
    decode,
    encodePretty,
  )
where

import CLC.Stackage.Utils.IO qualified as IO
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Asn
import Data.Aeson.Encode.Pretty qualified as AsnPretty
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import System.OsPath (OsPath)

-- | Decodes JSON.
decode :: (FromJSON a) => ByteString -> Either String a
decode = Asn.eitherDecodeStrict

-- | Write to the file.
writeJson :: (ToJSON a) => OsPath -> a -> IO ()
writeJson p = IO.writeBinaryFile p . encodePretty

-- | Encodes JSON
encodePretty :: (ToJSON a) => a -> ByteString
encodePretty =
  BSL.toStrict
    . AsnPretty.encodePretty'
      ( AsnPretty.defConfig
          { AsnPretty.confCompare = orderJsonKeys
          }
      )
  where
    orderJsonKeys :: Text -> Text -> Ordering
    orderJsonKeys l r = case liftA2 (,) (topKeyInt l) (topKeyInt r) of
      Just (lInt, rInt) -> compare lInt rInt
      Nothing -> case liftA2 (,) (resultsKeyInt l) (resultsKeyInt r) of
        Just (lInt, rInt) -> compare lInt rInt
        Nothing -> EQ

    topKeyInt :: Text -> Maybe Int
    topKeyInt "startTime" = Just 0
    topKeyInt "endTime" = Just 1
    topKeyInt "stats" = Just 2
    topKeyInt "results" = Just 3
    topKeyInt _ = Nothing

    resultsKeyInt :: Text -> Maybe Int
    resultsKeyInt "failures" = Just 0
    resultsKeyInt "untested" = Just 1
    resultsKeyInt "successes" = Just 2
    resultsKeyInt _ = Nothing
