module CLC.Stackage.Utils.IO
  ( -- * Files
    readBinaryFile,
    writeBinaryFile,
    withBinaryFileWriteMode,

    -- ** UTF8
    readFileUtf8,
    writeFileUtf8,

    -- * Directories
    removeDirectoryRecursiveIfExists,
    removeFileIfExists,
  )
where

import Control.Exception (throwIO)
import Control.Monad (when, (>=>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TEnc
import System.Directory.OsPath qualified as Dir
import System.File.OsPath qualified as FileIO
import System.IO (Handle, IOMode (WriteMode))
import System.OsPath (OsPath)

-- | Reads a UTF-8 file into 'Text', throw an exception if decoding fails.
readFileUtf8 :: OsPath -> IO Text
readFileUtf8 = readBinaryFile >=> either throwIO pure . TEnc.decodeUtf8'

-- | Reads a file.
readBinaryFile :: OsPath -> IO ByteString
readBinaryFile = FileIO.readFile'

-- | Writes a file.
writeBinaryFile :: OsPath -> ByteString -> IO ()
writeBinaryFile = FileIO.writeFile'

-- | Writes a Text file.
writeFileUtf8 :: OsPath -> Text -> IO ()
writeFileUtf8 p = writeBinaryFile p . TEnc.encodeUtf8

-- | With a file in write mode.
withBinaryFileWriteMode :: OsPath -> (Handle -> IO r) -> IO r
withBinaryFileWriteMode p = withBinaryFile p WriteMode

-- | With a file.
withBinaryFile :: OsPath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = FileIO.withBinaryFile

-- | Removes the directory if it exists.
removeDirectoryRecursiveIfExists :: OsPath -> IO ()
removeDirectoryRecursiveIfExists p =
  Dir.doesDirectoryExist p >>= (`when` Dir.removeDirectoryRecursive p)

-- | Removes the directory if it exists.
removeFileIfExists :: OsPath -> IO ()
removeFileIfExists p = Dir.doesFileExist p >>= (`when` Dir.removeFile p)
