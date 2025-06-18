module CLC.Stackage.Runner.Args
  ( Args (..),
    ColorLogs (..),
    getArgs,
  )
where

import CLC.Stackage.Builder.Env
  ( WriteLogs (WriteLogsCurrent, WriteLogsNone, WriteLogsSaveFailures),
  )
import Data.String qualified as Str
import Options.Applicative
  ( Mod,
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
import Options.Applicative.Help (Doc)
import Options.Applicative.Help.Chunk (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse), ReadM)
import System.OsPath (OsPath)
import System.OsPath qualified as OsP

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
    -- | Global options to pass to cabal e.g. --store-dir.
    cabalGlobalOpts :: [String],
    -- | Options to pass to cabal e.g. --semaphore.
    cabalOpts :: [String],
    -- | Optional path to cabal executable.
    cabalPath :: Maybe OsPath,
    -- | Determines if we color the logs. If 'Nothing', attempts to detect
    -- if colors are supported.
    colorLogs :: ColorLogs,
    -- | If true, the first group that fails to completely build stops
    -- clc-stackage.
    groupFailFast :: Bool,
    -- | Disables the cache, which otherwise saves the outcome of a run in a
    -- json file. The cache is used for resuming a run that was interrupted.
    noCache :: Bool,
    -- | If true, leaves the last generated cabal files.
    noCleanup :: Bool,
    -- | If true, the first package that fails _within_ a package group will
    -- cause the entire group to fail.
    packageFailFast :: Bool,
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
    desc =
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
          Pretty.indent 2 <$> Chunk.paragraph "clc-stackage",
          Chunk.paragraph $
            mconcat
              [ "This will build everything in one package group, and pass ",
                "--keep-going to cabal."
              ],
          Chunk.paragraph "Examples:",
          mkExample
            [ "# Basic example",
              "$ clc-stackage"
            ],
          mkExample
            [ "# Batch with groups of 100 and some cabal options",
              "$ clc-stackage --batch 100 --cabal-options='--semaphore --verbose=1'"
            ],
          mkExample
            [ "# Run with custom cabal",
              "$ clc-stackage --cabal-path=path/to/cabal --cabal-global-options='--store-dir=path/to/store'"
            ],
          mkExample
            [ "# Run with custom snapshot",
              "$ clc-stackage --snapshot-path=path/to/snapshot-file"
            ]
        ]
    mkExample :: [String] -> Chunk Doc
    mkExample =
      Chunk.vcatChunks
        . fmap (fmap (Pretty.indent 2) . Chunk.stringChunk)

parseCliArgs :: Parser Args
parseCliArgs =
  ( do
      batch <- parseBatch
      cabalGlobalOpts <- parseCabalGlobalOpts
      cabalOpts <- parseCabalOpts
      cabalPath <- parseCabalPath
      colorLogs <- parseColorLogs
      groupFailFast <- parseGroupFailFast
      noCache <- parseNoCache
      noCleanup <- parseNoCleanup
      packageFailFast <- parsePackageFailFast
      retryFailures <- parseRetryFailures
      snapshotPath <- parseSnapshotPath
      writeLogs <- parseWriteLogs

      pure $
        MkArgs
          { batch,
            cabalGlobalOpts,
            cabalOpts,
            cabalPath,
            colorLogs,
            groupFailFast,
            noCache,
            noCleanup,
            packageFailFast,
            retryFailures,
            snapshotPath,
            writeLogs
          }
  )
    <**> OA.helper

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
                [ "If given N, divides the package set into groups of at ",
                  "most size N. This can be useful when building everything ",
                  "in one build is infeasible, or taking advantage of the ",
                  "better status reporting. No option means we batch ",
                  "everything in the same group."
                ]
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
            mkHelp "Optional path to cabal executable."
          ]
      )

parseColorLogs :: Parser ColorLogs
parseColorLogs =
  OA.option
    readColorLogs
    ( mconcat
        [ OA.long "color-logs",
          OA.metavar "(off | on | detect)",
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
        bad -> fail $ "Expected one of (off | on | detect), received: " <> bad

parseGroupFailFast :: Parser Bool
parseGroupFailFast =
  OA.switch
    ( mconcat
        [ OA.long "group-fail-fast",
          mkHelp helpTxt
        ]
    )
  where
    helpTxt =
      mconcat
        [ "If true, the first group that fails to completely build stops ",
          "clc-stackage."
        ]

parseNoCache :: Parser Bool
parseNoCache =
  OA.switch
    ( mconcat
        [ OA.long "no-cache",
          mkHelp $
            mconcat
              [ "Disables the cache. Normally, the outcome of a run is saved ",
                "to a json cache. This is useful for resuming a run that was ",
                "interrupted (e.g. CTRL-C). The next run will fetch the ",
                "packages to build from the cache."
              ]
        ]
    )

parseNoCleanup :: Parser Bool
parseNoCleanup =
  OA.switch
    ( mconcat
        [ OA.long "no-cleanup",
          mkHelp "Will not remove the generated cabal files after exiting."
        ]
    )

parsePackageFailFast :: Parser Bool
parsePackageFailFast =
  OA.switch
    ( mconcat
        [ OA.long "package-fail-fast",
          mkHelp helpTxt
        ]
    )
  where
    helpTxt =
      mconcat
        [ "If true, the first package that fails _within_ a package group ",
          "will cause the entire group to fail. We then move to the next ",
          "group, as normal. The default (off) behavior is equivalent to ",
          "cabal's --keep-going)."
        ]

parseRetryFailures :: Parser Bool
parseRetryFailures =
  OA.switch
    ( mconcat
        [ OA.long "retry-failures",
          mkHelp "Retries failures from the cache. Incompatible with --no-cache. "
        ]
    )

parseSnapshotPath :: Parser (Maybe OsPath)
parseSnapshotPath =
  OA.optional $
    OA.option
      readOsPath
      ( mconcat
          [ OA.long "snapshot-path",
            OA.metavar "PATH",
            mkHelp $
              mconcat
                [ "Optional path to snapshot file. If given, this overrides ",
                  "the stackage snapshot; that is, we use the file's contents, ",
                  "rather than the stackage server. The file should be ",
                  "formatted similar to ",
                  "https://www.stackage.org/<snapshot>/cabal.config i.e. each ",
                  "line should be '<pkg> ==<vers>' e.g. 'lens ==5.3.4'. Note ",
                  "that the snapshot is still filtered according to ",
                  "excluded_pkgs.json."
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
            OA.metavar "(none | current | save-failures)",
            mkHelp $
              mconcat
                [ "Determines what cabal logs to write to the output/ ",
                  "directory. 'None' writes nothing. 'Current' writes stdout ",
                  "and stderr for the currently building project. ",
                  "'Save-failures' is the same as 'current' except the files ",
                  "are not deleted if the build failed. Defaults to ",
                  "save-failures."
                ]
          ]
      )
  where
    readWriteLogs =
      OA.str >>= \case
        "none" -> pure WriteLogsNone
        "current" -> pure WriteLogsCurrent
        "save-failures" -> pure WriteLogsSaveFailures
        other ->
          fail $
            mconcat
              [ "Expected one of (none | current | save-failures), received: ",
                other
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
