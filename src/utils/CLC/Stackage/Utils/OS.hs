{-# LANGUAGE CPP #-}

module CLC.Stackage.Utils.OS
  ( Os (..),
    currentOs,
  )
where

data Os
  = Linux
  | Osx
  | Windows
  deriving stock (Bounded, Enum, Eq, Show)

{- ORMOLU_DISABLE -}

currentOs :: Os
currentOs =
#if OSX
  Osx
#elif WINDOWS
  Windows
#else
  Linux
#endif

{- ORMOLU_ENABLE -}
