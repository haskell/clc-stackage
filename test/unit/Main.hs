module Main (main) where

import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Golden (DeleteOutputFile (OnPass))
import Unit.CLC.Stackage.Runner.Env qualified as Env
import Unit.CLC.Stackage.Runner.Report qualified as Report

main :: IO ()
main =
  defaultMain $
    localOption OnPass $
      testGroup
        "Unit"
        [ Env.tests,
          Report.tests
        ]
