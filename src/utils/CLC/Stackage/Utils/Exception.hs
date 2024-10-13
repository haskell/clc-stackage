-- | Provides utils for exceptions.
module CLC.Stackage.Utils.Exception
  ( try,
    tryAny,
    throwLeft,
    mapThrowLeft,
  )
where

import Control.Exception
  ( Exception (fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException,
    throwIO,
  )
import Control.Exception qualified as Ex
import Data.Bifunctor (first)

mapThrowLeft :: (Exception e2) => (e1 -> e2) -> Either e1 a -> IO a
mapThrowLeft f = throwLeft . first f

-- | Throws left.
throwLeft :: (Exception e) => Either e a -> IO a
throwLeft (Right x) = pure x
throwLeft (Left e) = throwIO e

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try @SomeException

-- | Try but it does not catch async exceptions. This allows us to catch
-- 'SomeException' more safely.
try :: (Exception e) => IO a -> IO (Either e a)
try io =
  Ex.try io >>= \case
    Left ex
      | isSyncException ex -> pure $ Left ex
      | otherwise -> throwIO ex
    Right x -> pure $ Right x

isSyncException :: (Exception e) => e -> Bool
isSyncException e =
  case fromException (toException e) of
    Just (SomeAsyncException _) -> False
    Nothing -> True
