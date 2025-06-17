module Main (main) where

import CLC.Stackage.Runner qualified as Runner
import CLC.Stackage.Utils.Logging qualified as Logging
import System.Console.Terminal.Size qualified as TermSize

main :: IO ()
main = do
  mWidth <- (fmap . fmap) TermSize.width TermSize.size

  let hLogger = Logging.mkDefaultLogger
  case mWidth of
    Just w -> Runner.run $ hLogger {Logging.terminalWidth = w}
    Nothing -> do
      Logging.putTimeInfoStr hLogger "Failed detecting terminal width"
      Runner.run hLogger
