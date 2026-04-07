{-# LANGUAGE OverloadedLists #-}

module CLC.Stackage.Runner.Args
  ( Args (..),
    ColorLogs (..),
    getArgs,
  )
where

import CLC.Stackage.Builder.Env
  ( WriteLogs (WriteLogsCurrent, WriteLogsNone, WriteLogsSaveFailures),
  )
import CLC.Stackage.Utils.Paths qualified as Paths
import Control.Exception (IOException, try)
import Data.Either (fromRight)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String qualified as Str
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Options.Applicative
  ( Mod,
    OptionFields,
    Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
    (<**>),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Builder.Completer (Completer)
import Options.Applicative.Builder.Completer qualified as OAC
import Options.Applicative.Help (Doc)
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse), ReadM)
import System.Directory.OsPath qualified as Dir
import System.OsPath (OsPath)
import System.OsPath qualified as OsP
import System.Process qualified as P

-- | Log coloring option.
data ColorLogs
  = ColorLogsOff
  | ColorLogsOn
  | ColorLogsDetect
  deriving stock (Eq, Show)

-- | CLI args.
data Args = MkArgs
  { -- | If given, batches packages together so we build more than one.
    -- Defaults to batching everything together in the same group.
    batch :: Maybe Int,
    -- | 1-based index for building the Nth package group only, according to
    -- --batch. Intended for CI use, where building all groups takes too much
    -- time.
    batchIndex :: Maybe Int,
    -- | Global options to pass to cabal e.g. --store-dir.
    cabalGlobalOpts :: [String],
    -- | Options to pass to cabal e.g. --semaphore.
    cabalOpts :: [String],
    -- | Optional path to cabal executable.
    cabalPath :: Maybe OsPath,
    -- | If true, the 'cabal update' step is run.
    cabalUpdate :: Bool,
    -- | Enables the cache, which saves the outcome of a run in a json file.
    -- The cache is used for resuming a run that was interrupted.
    cache :: Bool,
    -- | If true, leaves the last generated cabal files.
    cleanup :: Bool,
    -- | Determines if we color the logs. If 'Nothing', attempts to detect
    -- if colors are supported.
    colorLogs :: ColorLogs,
    -- | If true, the first group that fails to completely build stops
    -- clc-stackage.
    groupFailFast :: Bool,
    -- | If true, the first package that fails _within_ a package group will
    -- cause the entire group to fail.
    packageFailFast :: Bool,
    -- | If true, prints the package set that will be used, and exits.
    printPackageSet :: Bool,
    -- | Whether to retry packages that failed.
    retryFailures :: Bool,
    -- | Optional path to snapshot file. If given, we use the file's contents
    -- as the package set, rather than the stackage server.
    snapshotPath :: Maybe OsPath,
    -- | Determines what logs to write.
    writeLogs :: Maybe WriteLogs
  }
  deriving stock (Eq, Show)

-- | Returns CLI args.
getArgs :: IO Args
getArgs = OA.execParser parserInfoArgs
  where
    parserInfoArgs =
      ParserInfo
        { infoParser = parseCliArgs,
          infoFullDesc = True,
          infoProgDesc = desc,
          infoHeader = Chunk headerTxt,
          infoFooter = Chunk Nothing,
          infoFailureCode = 1,
          infoPolicy = Intersperse
        }
    headerTxt = Just "clc-stackage: Builds all packages in a stackage snapshot."
    desc = intro <> completions <> examples

    intro =
      Chunk.vsepChunks
        [ Chunk.paragraph $
            mconcat
              [ "clc-stackage is an executable that downloads a stackage ",
                "snapshot and attempts to build all packages in it. Build ",
                "logs are saved in ./output, where they can be examined for ",
                "determining which packages failed."
              ],
          Chunk.paragraph $
            mconcat
              [ "The '--batch N' arg will divide the package set into groups ",
                "of size N, then build each group sequentially. This process ",
                "can be interrupted at any time, in which case the progress ",
                "will be saved to a cache, so we can pick up where we left ",
                "off."
              ],
          Chunk.paragraph "Alternatively, to build everything in one go, run:",
          Pretty.indent 2 <$> Chunk.paragraph "$ clc-stackage",
          Chunk.paragraph $
            mconcat
              [ "This will build everything in one package group, and pass ",
                "--keep-going to cabal."
              ]
        ]

    completions =
      Chunk.vsepChunks
        [ line,
          Chunk.paragraph "Shell completions are available e.g.",
          Pretty.indent 2 <$> Chunk.stringChunk "$ source <(clc-stackage --bash-completion-script `which clc-stackage`)"
        ]

    examples =
      Chunk.vsepChunks
        [ line,
          Chunk.paragraph "Examples:",
          mkExample
            [ "1. Basic example:",
              "",
              "$ clc-stackage"
            ],
          mkExample
            [ "2. Batch with groups of 100 and some cabal options:",
              "",
              "$ clc-stackage --batch 100 --cabal-options='--semaphore --verbose=1'"
            ],
          mkExample
            [ "3. Run with custom cabal:",
              "",
              "$ clc-stackage \\",
              "  --cabal-path=path/to/cabal \\",
              "  --cabal-global-options='--store-dir=path/to/store'"
            ],
          mkExample
            [ "4. Run with custom snapshot:",
              "",
              "$ clc-stackage --snapshot-path=path/to/snapshot-file"
            ]
        ]

    mkExample :: NonEmpty String -> Chunk Doc
    mkExample = identPara 2 5

    identPara :: Int -> Int -> NonEmpty String -> Chunk Doc
    identPara hIndent lIndent (h :| xs) =
      Chunk.vcatChunks
        . (\ys -> toChunk hIndent h : ys)
        . fmap (toChunk lIndent)
        $ xs

    toChunk _ "" = line
    toChunk i other = fmap (Pretty.indent i) . Chunk.stringChunk $ other

    line = Chunk (Just Pretty.softline)

parseCliArgs :: Parser Args
parseCliArgs =
  ( do
      ~(cabalGlobalOpts, cabalOpts, cabalPath, cabalUpdate) <- parseCabalGroup
      ~(cache, retryFailures) <- parseCacheGroup
      ~(groupFailFast, packageFailFast) <- parseFailuresGroup
      ~(batch, batchIndex, printPackageSet, snapshotPath) <- parseMiscGroup
      ~(cleanup, colorLogs, writeLogs) <- parseOutputGroup

      pure $
        MkArgs
          { batch,
            batchIndex,
            cabalGlobalOpts,
            cabalOpts,
            cabalPath,
            cabalUpdate,
            cache,
            cleanup,
            colorLogs,
            groupFailFast,
            packageFailFast,
            printPackageSet,
            retryFailures,
            snapshotPath,
            writeLogs
          }
  )
    <**> OA.helper
  where
    parseCabalGroup =
      OA.parserOptionGroup "Cabal options:" $
        (,,,)
          <$> parseCabalGlobalOpts
          <*> parseCabalOpts
          <*> parseCabalPath
          <*> parseCabalUpdate

    parseCacheGroup =
      OA.parserOptionGroup "Cache options:" $
        (,)
          <$> parseCache
          <*> parseRetryFailures

    parseFailuresGroup =
      OA.parserOptionGroup "Failure options:" $
        (,)
          <$> parseGroupFailFast
          <*> parsePackageFailFast

    parseMiscGroup =
      OA.parserOptionGroup "Misc options:" $
        (,,,)
          <$> parseBatch
          <*> parseBatchIndex
          <*> parsePrintPackageSet
          <*> parseSnapshotPath

    parseOutputGroup =
      OA.parserOptionGroup "Output options:" $
        (,,)
          <$> parseCleanup
          <*> parseColorLogs
          <*> parseWriteLogs

parseBatch :: Parser (Maybe Int)
parseBatch =
  OA.optional $
    OA.option
      OA.auto
      ( mconcat
          [ OA.long "batch",
            OA.metavar "NAT",
            mkHelp $
              mconcat
                [ "Divides the package set into groups of at ",
                  "most size N. This can be useful when building everything ",
                  "in one build is infeasible, or we want to take advantage of ",
                  "the better status reporting. No option means we batch ",
                  "everything in the same group."
                ]
          ]
      )

-- Determines which --batch group to build. Normally we want to build all
-- groups, so this arg is intended only for CI, where a single CI job cannot
-- build everything, or it will time out. Hence this is marked 'internal'
-- to hide it from the --help page, as its presence would only confuse.
parseBatchIndex :: Parser (Maybe Int)
parseBatchIndex =
  OA.optional $
    OA.option
      OA.auto
      ( mconcat
          [ OA.long "batch-index",
            OA.internal,
            OA.metavar "NAT"
          ]
      )

parseCabalGlobalOpts :: Parser [String]
parseCabalGlobalOpts =
  OA.option
    readOpts
    ( mconcat
        [ OA.long "cabal-global-options",
          OA.metavar "ARGS...",
          OA.value [],
          mkHelp $
            mconcat
              [ "Global arguments to pass to cabal e.g. '--store-dir=path/to/store'. ",
                "These precede the build command."
              ]
        ]
    )
  where
    readOpts = Str.words <$> OA.str

parseCabalOpts :: Parser [String]
parseCabalOpts =
  OA.option
    readOpts
    ( mconcat
        [ OA.long "cabal-options",
          OA.metavar "ARGS...",
          OA.value [],
          mkHelp "Quoted arguments to pass to cabal e.g. '--semaphore --verbose=1'."
        ]
    )
  where
    readOpts = Str.words <$> OA.str

parseCabalPath :: Parser (Maybe OsPath)
parseCabalPath =
  OA.optional $
    OA.option
      readOsPath
      ( mconcat
          [ OA.long "cabal-path",
            OA.metavar "PATH",
            OA.completer compgenCwdPathsCompleter,
            mkHelp "Optional path to cabal executable."
          ]
      )

parseColorLogs :: Parser ColorLogs
parseColorLogs =
  OA.option
    readColorLogs
    ( mconcat
        [ OA.long "color-logs",
          OA.metavar "(detect | on | off)",
          OA.completeWith ["detect", "on", "off"],
          OA.value ColorLogsDetect,
          mkHelp "Determines whether we color logs. Defaults to detect."
        ]
    )
  where
    readColorLogs =
      OA.str >>= \case
        "off" -> pure ColorLogsOff
        "on" -> pure ColorLogsOn
        "detect" -> pure ColorLogsDetect
        bad -> fail $ "Expected one of (detect | on | off), received: " <> bad

parseGroupFailFast :: Parser Bool
parseGroupFailFast = mkSwitch opts
  where
    opts =
      mconcat
        [ OA.long "group-fail-fast",
          OA.value False,
          mkHelp helpTxt
        ]
    helpTxt =
      mconcat
        [ "If on, the first batch group that fails to completely build stops ",
          "clc-stackage. Defaults to 'off'."
        ]

parseCabalUpdate :: Parser Bool
parseCabalUpdate = mkSwitch opts
  where
    opts =
      mconcat
        [ OA.long "cabal-update",
          OA.value True,
          mkHelpNoLine helpTxt
        ]
    helpTxt = "Runs 'cabal update' before building. Defaults to 'on'."

parseCache :: Parser Bool
parseCache = mkSwitch opts
  where
    opts =
      mconcat
        [ OA.long "cache",
          OA.value True,
          mkHelp $
            mconcat
              [ "Saves the outcome of a run to a json cache, useful for resuming ",
                "a run that was interrupted (e.g. CTRL-C). The next run will fetch ",
                "the packages to build from the cache. Defaults to 'on'."
              ]
        ]

parseCleanup :: Parser Bool
parseCleanup = mkSwitch opts
  where
    opts =
      mconcat
        [ OA.long "cleanup",
          OA.value True,
          mkHelp "Removes generated files after finishing. Defaults to 'on'."
        ]

parsePackageFailFast :: Parser Bool
parsePackageFailFast = mkSwitch opts
  where
    opts =
      mconcat
        [ OA.long "package-fail-fast",
          OA.value False,
          mkHelpNoLine helpTxt
        ]
    helpTxt =
      mconcat
        [ "If on, the first package that fails _within_ a batch group ",
          "will cause the entire group to fail. We then move to the next ",
          "group, as normal. The default 'off' behavior is equivalent to ",
          "cabal's --keep-going)."
        ]

-- Notice that unlike other on/off switches, this is an actual flag
-- (--print-package-set) vs. an on/off option (--print-package-set (on | off)).
--
-- We have this exception because this isn't really an on/off switch but
-- rather an alternative command which bypasses the build entirely. This
-- would make more sense using optparse's command syntax, except that would
-- require normal usage to also have some command (e.g. build), which doesn't
-- seem worth it for the normal, happy path.
--
-- Ideally this would be a command and normal usage would be a "default command",
-- i.e. require no actual command, but optparse has no such notion.
parsePrintPackageSet :: Parser Bool
parsePrintPackageSet =
  OA.switch
    ( mconcat
        [ OA.long "print-package-set",
          mkHelp helpTxt
        ]
    )
  where
    helpTxt =
      mconcat
        [ "Instead of running the builder, prints the package set that will ",
          "be used and exits."
        ]

parseRetryFailures :: Parser Bool
parseRetryFailures = mkSwitch opts
  where
    opts =
      mconcat
        [ OA.long "retry-failures",
          OA.value False,
          mkHelpNoLine $
            mconcat
              [ "Retries failures from the cache. Incompatible with '--cache off'. ",
                "Defaults to 'off'."
              ]
        ]

parseSnapshotPath :: Parser (Maybe OsPath)
parseSnapshotPath =
  OA.optional $
    OA.option
      readOsPath
      ( mconcat
          [ OA.long "snapshot-path",
            OA.metavar "PATH",
            OA.completer compgenCwdPathsCompleter,
            mkHelpNoLine $
              mconcat
                [ "Optional path to snapshot file. This overrides ",
                  "the stackage snapshot; that is, we use the file's contents, ",
                  "rather than the stackage server. The file should be ",
                  "formatted similar to ",
                  "https://www.stackage.org/<snapshot>/cabal.config i.e. each ",
                  "line should be '<pkg> ==<vers>' e.g. 'lens ==5.3.4'. Note ",
                  "that the snapshot is still filtered according to ",
                  "package_index.jsonc."
                ]
          ]
      )

parseWriteLogs :: Parser (Maybe WriteLogs)
parseWriteLogs =
  OA.optional $
    OA.option
      readWriteLogs
      ( mconcat
          [ OA.long "write-logs",
            OA.metavar "(current | save-failures | off)",
            OA.completeWith ["current", "save-failures", "off"],
            helpTxt
          ]
      )
  where
    readWriteLogs =
      OA.str >>= \case
        "off" -> pure WriteLogsNone
        "current" -> pure WriteLogsCurrent
        "save-failures" -> pure WriteLogsSaveFailures
        other ->
          fail $
            mconcat
              [ "Expected one of (current | save-failures | off), received: ",
                other
              ]

    helpTxt =
      itemizeNoLine
        [ "Determines what cabal logs to write to the output/ directory. 'save-failures' is the default.",
          "current: Writes stdout and sdterr for the currently building project.",
          "save-failures: Same as 'current', except the files are not deleted if the build failed.",
          "off: Writes nothing."
        ]

readOsPath :: ReadM OsPath
readOsPath = do
  fp <- OA.str
  case OsP.encodeUtf fp of
    Just osp -> pure osp
    Nothing -> fail $ "Failed encoding to ospath: " ++ fp

mkHelp :: String -> Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

-- The last entry in each option group should use this, to prevent an extra
-- new line.
mkHelpNoLine :: String -> Mod f a
mkHelpNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . Chunk.paragraph

-- | 'itemize' that does not append a trailing newline. Useful for the last
-- option in a group, as groups already start a newline.
itemizeNoLine :: NonEmpty String -> Mod OptionFields a
itemizeNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . itemizeHelper

itemizeHelper :: NonEmpty String -> Chunk Doc
itemizeHelper (intro :| ds) =
  Chunk.vcatChunks $
    Chunk.paragraph intro
      : toChunk Pretty.softline
      : (toItem <$> ds)
  where
    toItem d =
      fmap (Pretty.nest 2)
        . Chunk.paragraph
        $ ("- " <> d)

    toChunk :: a -> Chunk a
    toChunk = Chunk . Just

-- | Paths completer that tries compgen first, then falls back to
-- directory.
compgenCwdPathsCompleter :: Completer
compgenCwdPathsCompleter = bashCompleterQuiet "file" <> cwdPathsCompleter

-- | Like optparse's bashCompleter, except this does not report completions
-- errors, which can otherwise make the output difficult to read.
bashCompleterQuiet :: String -> Completer
bashCompleterQuiet action = OAC.mkCompleter $ \word -> do
  let cmd = L.unwords ["compgen", "-A", action, "--", OAC.requote word]
  (ec, out, _err) <- P.readCreateProcessWithExitCode (P.shell cmd) ""
  pure $ case ec of
    ExitFailure _ -> []
    ExitSuccess -> L.lines out

-- | Paths completer that uses directory.
cwdPathsCompleter :: Completer
cwdPathsCompleter = OAC.mkCompleter $ \word -> do
  eFiles <- tryIO $ do
    cwd <- Dir.getCurrentDirectory
    Dir.listDirectory cwd

  let files = fromRight [] eFiles

  pure $ foldr (go word) [] files
  where
    go :: String -> OsPath -> [String] -> [String]
    go word p acc = do
      let pStr = Paths.decodeUtfLenient p
          matchesPat = word `L.isPrefixOf` pStr

      if matchesPat
        then pStr : acc
        else acc

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

-- Makes a switch that takes '(on | off)'. For consistency, this should be
-- preferred for any on/off switch, rather than a normal flag
-- (e.g. --foo (on | off) vs. --foo).
mkSwitch :: Mod OptionFields Bool -> Parser Bool
mkSwitch opts = OA.option readSwitch opts'
  where
    opts' =
      OA.metavar "(on | off)"
        <> OA.completeWith ["on", "off"]
        <> opts

readSwitch :: ReadM Bool
readSwitch =
  OA.str >>= \case
    "off" -> pure False
    "on" -> pure True
    other -> fail $ "Expected (on | off), received: " ++ other
