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
    toDisplayName,

    -- * Version
    PackageVersion (..),
  )
where

import CLC.Stackage.Utils.Paths qualified as Paths
import Control.Applicative (Alternative ((<|>)))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as Asn
import Data.Char qualified as Ch
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.OsPath (OsPath)

-- | Wrapper for package version.
data PackageVersion
  = -- | Basic version text e.g. "2.3".
    PackageVersionText Text
  | -- | Represents an installed lib e.g. "foo installed" from cabal
    -- constraints. This is included in the from/toJSON instances, for
    -- writing/reading the report.
    --
    -- Generally speaking, this is only used when clc-stackage falls back
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
toDirName = Paths.encodeUtf . T.unpack . toDisplayName

-- | Slightly nicer display name e.g. "mtl-installed", "aeson-1.2.3".
toDisplayName :: Package -> Text
toDisplayName (MkPackage name vers) = txt
  where
    txt = name <> "-" <> v
    v = case vers of
      PackageVersionText t -> t
      PackageVersionInstalled -> "installed"

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
fromCabalConstraintsText = packageParser . T.stripStart

-- NOTE: [*Parsers]
--
-- DIY parser, where each function parses only as much as it needs, then
-- returns the rest to be fed into the next parser. Following megaparsec's
-- lead, each parser assumes that it is at the start of relevant text
-- (i.e. no leading whitespace), and consumes trailing whitespace.
--
-- Hence the "rest" that is returned must have its leading whitespace stripped,
-- so that the next parser can make the same assumption.

packageParser :: Text -> Maybe Package
packageParser txt = do
  (name, r1) <- nameParser txt
  (vers, _) <- versionTextParser r1 <|> versionInstalledParser r1
  pure $ MkPackage name vers

-- Split on whitepspace or equals e.g. "mtl installed", "aeson ==1.2.3".
nameParser :: Text -> Maybe (Text, Text)
nameParser txt
  | T.null name = Nothing
  | otherwise = Just (name, T.stripStart rest)
  where
    (name, rest) = T.break isNameChar txt
    isNameChar c = c == ' ' || c == '='

-- Parse "installed".
versionInstalledParser :: Text -> Maybe (PackageVersion, Text)
versionInstalledParser txt = do
  rest <- T.stripPrefix "installed" txt
  pure (PackageVersionInstalled, T.stripStart rest)

-- Parse e.g. "==1.2.3".
versionTextParser :: Text -> Maybe (PackageVersion, Text)
versionTextParser txt = do
  r1 <- T.stripPrefix delim txt
  let (vers, r2) = T.span isVersChar (T.stripStart r1)
  if not (T.null vers)
    then Just (PackageVersionText vers, T.stripStart r2)
    else Nothing
  where
    isVersChar c = Ch.isDigit c || c == '.'

versToTextInstalled :: PackageVersion -> Text
versToTextInstalled (PackageVersionText t) = " " <> delim <> t
versToTextInstalled PackageVersionInstalled = " installed"

versToTextNoInstalled :: PackageVersion -> Text
versToTextNoInstalled (PackageVersionText t) = " " <> delim <> t
versToTextNoInstalled PackageVersionInstalled = ""

delim :: Text
delim = "=="
