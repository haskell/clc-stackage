module CLC.Stackage.Builder.Writer
  ( writeCabal,
    writeCabalProjectLocal,
  )
where

import CLC.Stackage.Builder.Batch (PackageGroup (unPackageGroup))
import CLC.Stackage.Utils.IO qualified as IO
import CLC.Stackage.Utils.Package (Package)
import CLC.Stackage.Utils.Package qualified as Package
import CLC.Stackage.Utils.Paths qualified as Paths
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc

-- | Writes a cabal.project.local for the _entire_ package set. This should
-- only be called once, regardless of the number of builds. The purpose of
-- this function is it to ensure we always use the same transitive dependencies
--
-- For example, suppose our snapshot contains aeson-2.2.3.0. When aeson is
-- in the group, there are no problems, since we will write the exact version
-- in the cabal file in 'writeCabal'. But if aeson is _not_ in the group but
-- a __transitive dependency__, then we are at the mercy of cabal's constraint
-- solver.
--
-- By writing the entire (exact) dependency set into the cabal.project.local's
-- constraints section, we ensure the same version of aeson is used every time
-- it is a (transitive) dependency.
writeCabalProjectLocal :: [Package] -> IO ()
writeCabalProjectLocal pkgs = IO.writeBinaryFile path constraintsSrc
  where
    path = Paths.generatedCabalProjectLocalPath
    constraintsSrc = TEnc.encodeUtf8 constraintsTxt
    constraintsTxt = T.unlines $ "constraints:" : constraints
    constraints = (\p -> "  " <> Package.toCabalConstraintsText p) <$> pkgs

-- | Writes the package set to a cabal file for building. This will be called
-- for each group we want to build.
writeCabal :: PackageGroup -> IO ()
writeCabal pkgs = IO.writeBinaryFile Paths.generatedCabalPath cabalFileSrc
  where
    cabalFileSrc = TEnc.encodeUtf8 cabalFileTxt
    cabalFileTxt = mkCabalFile pkgs

mkCabalFile :: PackageGroup -> Text
mkCabalFile pkgs =
  T.unlines $
    [ "cabal-version:      3.0",
      "name:               generated",
      "version:            0.1.0.0",
      "build-type:         Simple",
      "",
      "library",
      "    exposed-modules:  Lib",
      "    build-depends:"
    ]
      <> pkgsTxt
      <> [ "    hs-source-dirs:   src",
           "    default-language: Haskell2010"
         ]
  where
    pkgsTxt = (\p -> pkgsIndent <> Package.toCabalDepText p) <$> NE.toList pkgs.unPackageGroup

-- build-depends is indented 4, then 2 for the package itself.
pkgsIndent :: Text
pkgsIndent = T.replicate 6 " "
