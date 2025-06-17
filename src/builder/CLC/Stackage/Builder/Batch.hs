module CLC.Stackage.Builder.Batch
  ( PackageGroup (..),
    batchPackages,
  )
where

import CLC.Stackage.Builder.Env
  ( BuildEnv
      ( batch,
        packagesToBuild
      ),
  )
import CLC.Stackage.Utils.Package (Package)
import Data.Bifunctor (Bifunctor (first))
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import Data.List.NonEmpty qualified as NE

-- | A (non-proper) subset of the original stackage package set.
newtype PackageGroup = MkPackageGroup {unPackageGroup :: NonEmpty Package}
  deriving stock (Eq, Show)

-- | Split entire package set from the BuildEnv into package groups.
batchPackages :: BuildEnv -> NonEmpty (PackageGroup, Int)
batchPackages env = first MkPackageGroup <$> pkgGroupsIdx
  where
    pkgGroups = case env.batch of
      Nothing -> NE.singleton env.packagesToBuild
      Just n -> chunksOf n env.packagesToBuild

    -- NOTE: NE.fromList is obviously unsafe in general, but here it is fine
    -- as 'n := length pkgGroups > 0' and [1 .. n] is non-empty such n.
    indexes = NE.fromList $ reverse [1 .. length pkgGroups]
    pkgGroupsIdx = NE.zip pkgGroups indexes

chunksOf :: Int -> NonEmpty a -> NonEmpty (NonEmpty a)
chunksOf n = go
  where
    go xs = case splitAtNE n xs of
      This ys -> NE.singleton ys
      That _ -> e
      These ys zs -> ys <| go zs
    e = error "chunksOf: A non-empty can only be broken up into positively-sized chunks."
{-# INLINEABLE chunksOf #-}

splitAtNE :: Int -> NonEmpty a -> These (NonEmpty a) (NonEmpty a)
splitAtNE n xs0@(x :| xs)
  | n <= 0 = That xs0
  | otherwise = case (NE.nonEmpty ys, NE.nonEmpty zs) of
      (Nothing, Nothing) -> This (NE.singleton x)
      (Just _, Nothing) -> This xs0
      (Nothing, Just zs') -> These (NE.singleton x) zs'
      (Just ys', Just zs') -> These (x <| ys') zs'
  where
    (ys, zs) = L.splitAt (n - 1) xs
{-# INLINEABLE splitAtNE #-}

data These a b
  = This a
  | That b
  | These a b
