-- |
-- Module      : Streamly.String
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- The 'String' type in this module is just a synonym for the type @List Char@.
-- It provides better performance compared to the standard Haskell @String@
-- type and can be used almost as a drop-in replacement, especially when used
-- with @OverloadedStrings@ extension, with little differences.
--
-- See "Streamly.List", <src/docs/streamly-vs-lists.md> for more details and
-- <src/test/PureStreams.hs> for comprehensive usage examples.
--
--
module Streamly.String
    ( String
    )
where

import Data.Char (ord, chr)
import Data.Word (Word32)
import Streamly (IsStream)
import Streamly.List (List)
import Prelude hiding (String)

import qualified Streamly.Prelude as S
import qualified Streamly.Fold as FL
import qualified Streamly.Array as A

type String = List Char

-------------------------------------------------------------------------------
-- Encoding/Decoding Characters
-------------------------------------------------------------------------------

-- decodeWith :: TextEncoding -> t m Word8 -> t m Char

-------------------------------------------------------------------------------
-- Encoding/Decoding Unicode Characters
-------------------------------------------------------------------------------

-- We read byte streams from file handles. Byte stream can be decoded into a
-- Char stream using an appropriate decoder. The Char streams are the
-- equivalent of Haskell Strings. We can use usual text/string processing
-- routines on these streams. Processed streams can finally be encoded and then
-- written to a handle.

{-
-- | Decode a UTF-8 encoded bytestream to a stream of Unicode characters.
decodeUtf8 :: IsStream t => t m Word8 -> t m Char

-- | Encode a stream of Unicode characters to a UTF-8 encoded bytestream.
encodeUtf8 :: IsStream t => t m Char -> t m Word8
-}

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

-- | Split a stream of characters on both sides of a separator sequence,
-- dropping the separator.
--
-- For easier illustration, let's define a function that operates on lists:
--
-- @
-- splitList pat str = S.toList $ STR.splitOn (A.fromList pat) (FL.toList) (S.fromList str)
-- @
--
-- >>> splitList "" "hello"
-- > ["h","e","l","l","o"]
--
-- >>> splitList "x" "hello"
-- > ["hello"]
--
-- >>> splitList "h" "hello"
-- > ["","ello"]
--
-- >>> splitList "o" "hello"
-- > ["hell",""]
--
-- >>> splitList "e" "hello"
-- > ["h","llo"]
--
-- >>> splitList "l" "hello"
-- > ["he","","o"]
--
-- >>> splitList "ll" "hello"
-- > ["he","o"]
--
-- >>> splitList "hello" "hello"
-- > ["",""]
--
-- 'splitOn' is an inverse of 'intercalate'. The following law always holds:
--
-- > intercalate . splitOn == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitOn . intercalate == id
--
-- The following law always holds:
--
-- > concat . splitOn . intercalate == concat
--
{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, Monad m)
    => A.Array Char -> FL.Fold m Char b -> t m Char -> t m b
splitOn pat f str =
    FL.splitOn (A.map (fromIntegral . ord :: Char -> Word32) pat)
               (FL.lmap (chr . fromIntegral) f)
               (S.map (fromIntegral . ord :: Char -> Word32) str)
