{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Unit.CLC.Stackage.Utils.Package (tests) where

import CLC.Stackage.Utils.Package
  ( Package (MkPackage, name, version),
    PackageVersion (PackageVersionInstalled, PackageVersionText),
  )
import CLC.Stackage.Utils.Package qualified as Package
import Unit.Prelude

tests :: TestTree
tests =
  testGroup
    "CLC.Stackage.Utils.Package"
    [ testFromCabalConstraintsTextSuccesses,
      testToCabalDepText,
      testToCabalConstraintsText,
      testToDisplayName
    ]

testFromCabalConstraintsTextSuccesses :: TestTree
testFromCabalConstraintsTextSuccesses = testCase desc $ do
  Just e1 @=? Package.fromCabalConstraintsText "aeson ==2.0.1"
  Just e1 @=? Package.fromCabalConstraintsText "aeson ==2.0.1,"
  Just e1 @=? Package.fromCabalConstraintsText "  aeson == 2.0.1  ,  "
  Just e2 @=? Package.fromCabalConstraintsText "mtl installed"
  Just e2 @=? Package.fromCabalConstraintsText "mtl installed,"
  Just e2 @=? Package.fromCabalConstraintsText " mtl   installed  , "
  where
    desc = "fromCabalConstraintsText successes"

testToCabalDepText :: TestTree
testToCabalDepText = testCase desc $ do
  ", aeson ==2.0.1" @=? Package.toCabalDepText e1
  ", mtl" @=? Package.toCabalDepText e2
  where
    desc = "toCabalDepText"

testToCabalConstraintsText :: TestTree
testToCabalConstraintsText = testCase desc $ do
  "aeson ==2.0.1," @=? Package.toCabalConstraintsText e1
  "mtl installed," @=? Package.toCabalConstraintsText e2
  where
    desc = "toCabalConstraintsText"

testToDisplayName :: TestTree
testToDisplayName = testCase desc $ do
  "aeson-2.0.1" @=? Package.toDisplayName e1
  "mtl-installed" @=? Package.toDisplayName e2
  where
    desc = "toDisplayName"

e1 :: Package
e1 =
  MkPackage
    { name = "aeson",
      version = PackageVersionText "2.0.1"
    }

e2 :: Package
e2 =
  MkPackage
    { name = "mtl",
      version = PackageVersionInstalled
    }
