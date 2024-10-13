module Main (main) where

import CLC.Stackage.Runner qualified as Runner
import CLC.Stackage.Utils.Logging qualified as Logging
import Data.Text qualified as T
import Data.Time.LocalTime qualified as Local
import System.Console.Terminal.Size qualified as TermSize
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  mWidth <- (fmap . fmap) TermSize.width TermSize.size

  case mWidth of
    Just w -> Runner.run $ mkLogger w
    Nothing -> do
      let hLogger = mkLogger 80
      Logging.putTimeInfoStr hLogger False "Failed detecting terminal width"
      Runner.run hLogger
  where
    mkLogger w =
      Logging.MkHandle
        { Logging.getLocalTime = Local.zonedTimeToLocalTime <$> Local.getZonedTime,
          Logging.logStrErrLn = hPutStrLn stderr . T.unpack,
          Logging.logStrLn = putStrLn . T.unpack,
          Logging.terminalWidth = w
        }
