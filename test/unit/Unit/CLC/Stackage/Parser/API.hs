{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Unit.CLC.Stackage.Parser.API (tests) where

import CLC.Stackage.Parser.API qualified as API
import CLC.Stackage.Parser.API.CabalConfig qualified as CabalConfig
import CLC.Stackage.Parser.API.JSON qualified as JSON
import CLC.Stackage.Utils.Exception qualified as Ex
import Control.DeepSeq (NFData, rnf)
import Control.Exception (displayException, evaluate)
import Network.HTTP.Client.TLS qualified as TLS
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "CLC.Stackage.Parser.API"
    [ testCabalConfigEndpoint,
      testJsonEndpoint
    ]

testCabalConfigEndpoint :: TestTree
testCabalConfigEndpoint = testCase desc $ do
  manager <- TLS.newTlsManager
  eval =<< CabalConfig.getStackage manager API.stackageSnapshot
  where
    desc = "Queries stackage cabal.config endpoint"

testJsonEndpoint :: TestTree
testJsonEndpoint = testCase desc $ do
  manager <- TLS.newTlsManager
  eval =<< JSON.getStackage manager API.stackageSnapshot
  where
    desc = "Queries stackage json endpoint"

eval :: (NFData a) => a -> IO ()
eval x =
  Ex.tryAny (evaluate $ rnf x) >>= \case
    Right _ -> pure ()
    Left ex -> assertFailure $ "Exception during eval: " ++ displayException ex
