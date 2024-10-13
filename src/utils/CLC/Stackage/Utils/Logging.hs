module CLC.Stackage.Utils.Logging
  ( -- * Logging Handler
    Handle (..),

    -- * Printing with timestamps
    putTimeInfoStr,
    putTimeSuccessStr,
    putTimeErrStr,

    -- ** ANSI Colors
    colorBlue,
    colorGreen,
    colorRed,
    colorMagenta,

    -- * Misc
    formatLocalTime,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (LocalTime)
import Data.Word (Word16)
import System.Console.Pretty (Color (Blue, Green, Magenta, Red))
import System.Console.Pretty qualified as Pretty

-- | Simple handle for logging, for testing output.
data Handle = MkHandle
  { -- | Retrieve local time.
    getLocalTime :: IO LocalTime,
    -- | Log stderr.
    logStrErrLn :: Text -> IO (),
    -- | Log stdout.
    logStrLn :: Text -> IO (),
    -- | Max terminal width.
    terminalWidth :: Word16
  }

-- | 'putStrLn' with a timestamp and info prefix.
putTimeInfoStr :: Handle -> Bool -> Text -> IO ()
putTimeInfoStr hLogger b s = do
  timeStr <- getLocalTimeString hLogger
  hLogger.logStrLn $ colorBlue b $ "[" <> timeStr <> "][Info]    " <> s'
  where
    s' = truncateIfNeeded hLogger.terminalWidth s

-- | 'putStrLn' with a timestamp and info prefix.
putTimeSuccessStr :: Handle -> Bool -> Text -> IO ()
putTimeSuccessStr hLogger b s = do
  timeStr <- getLocalTimeString hLogger
  hLogger.logStrLn $ colorGreen b $ "[" <> timeStr <> "][Success] " <> s'
  where
    s' = truncateIfNeeded hLogger.terminalWidth s

-- | 'putStrErrLn' with a timestamp and error prefix.
putTimeErrStr :: Handle -> Bool -> Text -> IO ()
putTimeErrStr hLogger b s = do
  timeStr <- getLocalTimeString hLogger
  hLogger.logStrErrLn $ colorRed b $ "[" <> timeStr <> "][Error]   " <> s'
  where
    s' = truncateIfNeeded hLogger.terminalWidth s

getLocalTimeString :: Handle -> IO Text
getLocalTimeString hLogger = formatLocalTime <$> hLogger.getLocalTime

formatLocalTime :: LocalTime -> Text
formatLocalTime = T.pack . Format.formatTime Format.defaultTimeLocale fmt
  where
    fmt = "%0Y-%m-%d %H:%M:%S"

colorBlue :: Bool -> Text -> Text
colorBlue b = colorIf b Blue

colorMagenta :: Bool -> Text -> Text
colorMagenta b = colorIf b Magenta

colorGreen :: Bool -> Text -> Text
colorGreen b = colorIf b Green

colorRed :: Bool -> Text -> Text
colorRed b = colorIf b Red

colorIf :: Bool -> Color -> Text -> Text
colorIf True = Pretty.color
colorIf False = const id

-- | Given maxLen, constant extra and Text t, returns t' s.t.
-- len t' <= max (0, maxLen - extra). An ellipse is added if t' was truncated.
truncateIfNeeded :: Word16 -> Text -> Text
truncateIfNeeded maxLen txt
  | T.length txt <= maxAllowed = txt
  | otherwise = txt'
  where
    maxAllowed = fromIntegral $ maxLen `monus` extra
    txt' = T.take (maxAllowed - 3) txt <> "..."

    extra = timeStrLen + constLen

-- subtraction clamped to zero.
monus :: Word16 -> Word16 -> Word16
monus x y
  | y >= x = 0
  | otherwise = x - y

-- | This is the total space after the timestamp before the message. Constant
-- for every message because whitespace makes up the difference i.e.
--
-- '[Info]    '
-- '[Success] '
-- '[Error]   '
--
-- We add one more so that long messages leave at least one space before the
-- terminal edge.
constLen :: Word16
constLen = 11

-- e.g. [2024-10-14 15:14:00]
timeStrLen :: Word16
timeStrLen = 21
