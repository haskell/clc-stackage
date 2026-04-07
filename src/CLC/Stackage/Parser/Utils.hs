module CLC.Stackage.Parser.Utils
  ( CommentsException (..),
    stripComments,
    stripInfix,

    -- * Misc
    isNum,
    commaW8,
    spaceW8,
  )
where

import Control.Exception (Exception (displayException))
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char qualified as Ch
import Data.String (IsString)
import Data.Word (Word8)

newtype CommentsException = MkCommentsException String
  deriving stock (Show)
  deriving newtype (IsString)

instance Exception CommentsException where
  displayException (MkCommentsException s) =
    "Error stripping comments: " <> s

-- | Strips a bytestring of line (//) and block (/* */) comments.
stripComments :: ByteString -> Either CommentsException ByteString
stripComments bs =
  let (preComment, mWithFSlash) = BS.break (== fslashW8) bs
   in case uncons2 mWithFSlash of
        -- 1. Remaining text is either 1 char or a single fslash: It cannot
        -- possibly start a comment, so return it.
        Nothing -> Right $ preComment <> mWithFSlash
        -- 2. Some remaining text. Need to check for a comment start.
        --
        -- - fslashChar: a single fslash
        -- - fslashNext: next char, need to check
        -- - mPostCommentStart: The part after potential comment start.
        Just (fslashChar, fslashNext, mPostCommentStart)
          | fslashNext == fslashW8 ->
              -- 2.1. Another fslash, we have started a line comment. Skip until
              -- the next newline.
              second
                (preComment <>)
                (skipLineComment mPostCommentStart >>= stripComments)
          | fslashNext == starW8 ->
              -- 2.2. A star, we have started a block comment. Skip until the
              -- next '*/'.
              second
                (preComment <>)
                (skipBlockComment mPostCommentStart >>= stripComments)
          -- 2.3. No fslash or star i.e. not a comment start. Concat it back in,
          -- and proceed with the rest of the string.
          | otherwise ->
              let start = preComment <> BS.pack [fslashChar, fslashNext]
               in second (start <>) (stripComments mPostCommentStart)
  where
    -- es is the start of a line comment, w/o the opening '//'.
    skipLineComment es =
      let (_lineComment, mCommentEnd) = BS.break (== newlineW8) es
       in case BS.uncons mCommentEnd of
            -- Did not find a closing newline: error!
            Nothing -> Left "Found line comment (//) without ending newline."
            Just (_nl, rest) -> Right rest

    -- es is the start of a block comment, w/o the opening '/*'.
    skipBlockComment es =
      let (_blockComment, mCommentEnd) = BS.break (== starW8) es
       in case uncons2 mCommentEnd of
            -- es had fewer than 2 chars, so it could not possibly end the
            -- comment, error.
            Nothing -> Left "Found block comment (/*) without ending (*/)."
            -- If we get here we know we have found a star char and at least one
            -- other char (starNext).
            Just (_starChar, starNext, mPostCommentStart)
              -- Found an ending slash, we have successfully ended the comment.
              | starNext == fslashW8 -> Right mPostCommentStart
              -- The next char is another star. Need to add it back in, in
              -- case it is part of the ending */ e.g. we have /***/.
              | starNext == starW8 -> skipBlockComment (BS.cons starW8 mPostCommentStart)
              -- starNext is something else. Search the rest of the string.
              | otherwise -> skipBlockComment mPostCommentStart

uncons2 :: ByteString -> Maybe (Word8, Word8, ByteString)
uncons2 bs = case BS.uncons bs of
  Nothing -> Nothing
  Just (c1, rest1) -> case BS.uncons rest1 of
    Nothing -> Nothing
    Just (c2, rest2) -> Just (c1, c2, rest2)

newlineW8 :: Word8
newlineW8 = i2w8 $ Ch.ord '\n'

starW8 :: Word8
starW8 = i2w8 $ Ch.ord '*'

fslashW8 :: Word8
fslashW8 = i2w8 $ Ch.ord '/'

spaceW8 :: Word8
spaceW8 = i2w8 $ Ch.ord ' '

commaW8 :: Word8
commaW8 = i2w8 $ Ch.ord ','

isNum :: Word8 -> Bool
isNum w = w >= i2w8 (Ch.ord '0') && w <= i2w8 (Ch.ord '9')

i2w8 :: Int -> Word8
i2w8 = fromIntegral

stripInfix :: ByteString -> ByteString -> Maybe (ByteString, ByteString)
stripInfix bs1 bs2 = (pre,) <$> BS.stripPrefix bs1 rest
  where
    (pre, rest) = BS.breakSubstring bs1 bs2
