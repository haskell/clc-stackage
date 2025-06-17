{-# LANGUAGE ViewPatterns #-}

-- | Provides the type representing a package with version.
module CLC.Stackage.Utils.Package
  ( -- * Package
    Package (..),

    -- ** Creation
    fromCabalConstraintsText,

    -- ** Elimination
    toCabalDepText,
    toCabalConstraintsText,
    toDirName,
    toTextInstalled,

    -- * Version
    PackageVersion (..),

    -- * Parsers
    packageParser,
  )
where

import CLC.Stackage.Utils.Paths qualified as Paths
import Control.Applicative (Alternative ((<|>)))
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as Asn
import Data.Char qualified as Ch
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import GHC.Generics (Generic)
import System.OsPath (OsPath)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char qualified as MPC
import Text.Megaparsec.Char.Lexer qualified as MPCL

-- | Wrapper for package version.
data PackageVersion
  = -- | Basic version text e.g. "2.3".
    PackageVersionText Text
  | -- | Represents an installed lib e.g. "foo installed" from cabal
    -- constraints. This is included in the from/toJSON instances, for the
    -- writing/reading the report.
    --
    -- Generally speaking, this is only used when clc-stackage is falls back
    -- to the cabal.config endpoint, or is used with an explicit
    -- --snapshot-path argument.
    PackageVersionInstalled
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (NFData)

instance FromJSON PackageVersion where
  parseJSON = Asn.withText "PackageVersion" $ \case
    "installed" -> pure PackageVersionInstalled
    other -> pure $ PackageVersionText other

instance ToJSON PackageVersion where
  toJSON (PackageVersionText t) = toJSON t
  toJSON PackageVersionInstalled = "installed"

-- | Package data.
data Package = MkPackage
  { name :: Text,
    version :: PackageVersion
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, NFData, ToJSON)

instance IsString Package where
  fromString s = case fromCabalConstraintsText (T.pack s) of
    Nothing ->
      error $
        mconcat
          [ "String '",
            s,
            "' did no match expected package format: <name> ==<vers>"
          ]
    Just p -> p

-- | Text representation suitable for cabal file build-depends.
toCabalDepText :: Package -> Text
toCabalDepText = (", " <>) . toTextNoInstalled

-- | Text representation suitable for cabal file constraints.
toCabalConstraintsText :: Package -> Text
toCabalConstraintsText = (<> ",") . toTextInstalled

-- | Returns an OsPath name based on this package i.e. the OsPath
-- representation of 'toText'. Used when naming an error directory for a
-- package that fails.
toDirName :: Package -> IO OsPath
toDirName = Paths.encodeUtf . T.unpack . toTextInstalled

-- | Text representation of the package respecting "installed" versions e.g.
--
-- - "aeson ==1.2.3"
-- - "mtl installed"
--
-- This output is suitable for cabal constraints, _not_ a cabal file.
toTextInstalled :: Package -> Text
toTextInstalled p = p.name <> versToTextInstalled p.version

-- | Text representation of the package, dropping "installed" versions e.g.
--
-- - "aeson ==1.2.3"
-- - "mtl"
--
-- This output is suitable for cabal file, _not_ constraints.
toTextNoInstalled :: Package -> Text
toTextNoInstalled p = p.name <> versToTextNoInstalled p.version

-- | Attempts to parse the text to the package. Some flexibility e.g.
-- supports:
--
-- - "aeson ==2.0.0,"
-- - "mtl installed"
fromCabalConstraintsText :: Text -> Maybe Package
fromCabalConstraintsText txt =
  case MP.parse (MPC.space *> packageParser) "package" txt of
    Right x -> Just x
    Left _ -> Nothing

type Parser = MP.Parsec Void Text

-- | Parses packages e.g.
--
-- - "aeson ==2.0.0,"
-- - "mtl installed"
packageParser :: Parser Package
packageParser = do
  name <- nameParser
  vers <- versionTextParser <|> versionInstalledParser
  pure $ MkPackage name vers

nameParser :: Parser Text
nameParser = lexeme $ MP.takeWhile1P (Just "name") isNameChar
  where
    isNameChar c = c /= ' ' && c /= '='

versionInstalledParser :: Parser PackageVersion
versionInstalledParser = do
  MPC.string "installed"
  mcommaParser
  pure PackageVersionInstalled

versionTextParser :: Parser PackageVersion
versionTextParser = do
  lexeme $ MPC.string delim
  vers <- lexeme $ MP.takeWhile1P (Just "version") isVersChar
  mcommaParser
  pure $ PackageVersionText vers
  where
    isVersChar c = Ch.isDigit c || c == '.'

mcommaParser :: Parser ()
mcommaParser = lexeme $ void $ MP.optional $ MPC.char ','

lexeme :: Parser a -> Parser a
lexeme = MPCL.lexeme MPC.space

versToTextInstalled :: PackageVersion -> Text
versToTextInstalled (PackageVersionText t) = " " <> delim <> t
versToTextInstalled PackageVersionInstalled = " installed"

versToTextNoInstalled :: PackageVersion -> Text
versToTextNoInstalled (PackageVersionText t) = " " <> delim <> t
versToTextNoInstalled PackageVersionInstalled = ""

delim :: Text
delim = "=="
