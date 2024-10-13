{-# LANGUAGE ViewPatterns #-}

-- | Provides the type representing a package with version.
module CLC.Stackage.Builder.Package
  ( Package (..),
    fromText,
    toText,
    toDepText,
    toDirName,
  )
where

import CLC.Stackage.Utils.Paths qualified as Paths
import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.OsPath (OsPath)

-- | Package data.
data Package = MkPackage
  { name :: Text,
    version :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

instance IsString Package where
  fromString s = case fromText (T.pack s) of
    Nothing ->
      error $
        mconcat
          [ "String '",
            s,
            "' did no match expected package format: <name> ==<vers>"
          ]
    Just p -> p

fromText :: Text -> Maybe Package
fromText txt = case T.breakOn delim txt of
  (xs, T.stripPrefix delim -> Just ys)
    -- point exists but version is empty
    | T.null ys -> Nothing
    -- correct
    | otherwise -> Just $ MkPackage xs ys
  -- point does not exist
  _ -> Nothing

-- | Text representation of the package e.g. 'foo ==1.2.3'.
toText :: Package -> Text
toText p = p.name <> delim <> p.version

delim :: Text
delim = " =="

-- | Text representation suitable for cabal file build-depends.
toDepText :: Package -> Text
toDepText = (", " <>) . toText

-- | Returns an OsPath name based on this package i.e. the OsPath
-- representation of 'toText'. Used when naming an error directory for a
-- package that fails.
toDirName :: Package -> IO OsPath
toDirName = Paths.encodeUtf . T.unpack . toText
