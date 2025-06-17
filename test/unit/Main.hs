module Main (main) where

import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Golden (DeleteOutputFile (OnPass))
import Unit.CLC.Stackage.Parser.API qualified as Parser.API
import Unit.CLC.Stackage.Runner.Env qualified as Runner.Env
import Unit.CLC.Stackage.Runner.Report qualified as Runner.Report
import Unit.CLC.Stackage.Utils.Package qualified as Utils.Package

main :: IO ()
main =
  defaultMain $
    localOption OnPass $
      testGroup
        "Unit"
        [ Parser.API.tests,
          Runner.Env.tests,
          Runner.Report.tests,
          Utils.Package.tests
        ]
