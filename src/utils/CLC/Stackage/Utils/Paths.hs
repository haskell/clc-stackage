{-# LANGUAGE QuasiQuotes #-}

module CLC.Stackage.Utils.Paths
  ( -- * CLC-Stackage paths
    outputDir,
    cachePath,
    reportPath,
    logsDir,
    generatedDir,
    generatedCabalPath,
    generatedCabalProjectLocalPath,

    -- * Utils
    canonicalizePath,
    OsPath.encodeUtf,
    decodeUtfLenient,
    unsafeDecodeUtf,
  )
where

import GHC.IO.Encoding.Failure (CodingFailureMode (TransliterateCodingFailure))
import GHC.IO.Encoding.UTF16 qualified as UTF16
import GHC.IO.Encoding.UTF8 qualified as UTF8
import System.Directory.OsPath qualified as Dir
import System.OsPath (OsPath, osp, (</>))
import System.OsPath qualified as OsPath
import System.OsString qualified as OsStr

-- | Leniently decodes OsPath to String.
decodeUtfLenient :: OsPath -> String
decodeUtfLenient =
  either (error . show) id
    . OsPath.decodeWith uft8Encoding utf16Encoding
  where
    uft8Encoding = UTF8.mkUTF8 TransliterateCodingFailure
    utf16Encoding = UTF16.mkUTF16le TransliterateCodingFailure

unsafeDecodeUtf :: OsPath -> FilePath
unsafeDecodeUtf p = case OsPath.decodeUtf p of
  Just fp -> fp
  Nothing -> error $ "Error decoding ospath: " <> show p

-- | Calls canonicalizePath, after manually expanding tilde (~) to the home
-- directory. The latter usually shouldn't be needed, as the shell normally
-- performs such expansions before the string makes it to the program.
-- But when it is part of an argument e.g.
--
--   --cabal-path=~/...
--
-- it is not expanded.
canonicalizePath :: OsPath -> IO OsPath
canonicalizePath p = case OsStr.stripPrefix [osp|~/|] p of
  Nothing -> Dir.canonicalizePath p
  Just rest -> do
    home <- Dir.getHomeDirectory
    Dir.canonicalizePath $ home </> rest

-- | Output directory.
outputDir :: OsPath
outputDir = [osp|output|]

-- | Cache path.
cachePath :: OsPath
cachePath = outputDir </> [osp|cache.json|]

-- | Report path.
reportPath :: OsPath
reportPath = outputDir </> [osp|report.json|]

-- | Path to generated project.
generatedDir :: OsPath
generatedDir = [osp|generated|]

generatedCabalPath :: OsPath
generatedCabalPath = generatedDir </> [OsPath.osp|generated.cabal|]

generatedCabalProjectLocalPath :: OsPath
generatedCabalProjectLocalPath = generatedDir </> [OsPath.osp|cabal.project.local|]

-- | Logs dir.
logsDir :: OsPath
logsDir = outputDir </> [osp|logs|]
