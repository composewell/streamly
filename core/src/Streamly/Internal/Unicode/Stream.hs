{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Unicode.Stream
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Unicode.Stream
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

      module Streamly.Internal.Unicode.Encode
    , module Streamly.Internal.Unicode.Decode

    {-
    -- * Operations on character strings
    , strip -- (dropAround isSpace)
    , stripEnd
    -}

    -- * Transformation
    , stripHead
    , lines -- foldLines
    , words -- foldWords
    , unlines -- unfoldLines
    , unwords -- unfoldWords
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO)
import Data.Char (ord)
#if MIN_VERSION_base(4,17,0)
import Data.Char (generalCategory, GeneralCategory(Space))
#endif
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream as D

import Prelude hiding (lines, words, unlines, unwords)

import Streamly.Internal.Unicode.Encode
import Streamly.Internal.Unicode.Decode

#include "DocTestUnicodeStream.hs"

{-
-------------------------------------------------------------------------------
-- Utility operations on strings
-------------------------------------------------------------------------------

strip :: IsStream t => Stream m Char -> Stream m Char
strip = undefined

stripTail :: IsStream t => Stream m Char -> Stream m Char
stripTail = undefined
-}

-- | Remove leading whitespace from a string.
--
-- >>> stripHead = Stream.dropWhile Char.isSpace
--
-- /Pre-release/
{-# INLINE stripHead #-}
stripHead :: Monad m => Stream m Char -> Stream m Char
stripHead = Stream.dropWhile isSpace

-- | Fold each line of the stream using the supplied 'Fold'
-- and stream the result.
--
-- Definition:
--
-- >>> lines f = Stream.foldMany (Fold.takeEndBy_ (== '\n') f)
--
-- Usage:
--
-- >>> Stream.toList $ Unicode.lines Fold.toList (Stream.fromList "line1\nline2\nline3\n\n\n")
-- ["line1","line2","line3","",""]
--
-- /Pre-release/
{-# INLINE lines #-}
lines :: Monad m => Fold m Char b -> Stream m Char -> Stream m b
lines f = Stream.foldMany (Fold.takeEndBy_ (== '\n') f)

#if !MIN_VERSION_base(4,17,0)
foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int
#endif

-- | Code copied from base/Data.Char to INLINE it
{-# INLINE isSpace #-}
isSpace :: Char -> Bool
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
#if MIN_VERSION_base(4,17,0)
  | otherwise = generalCategory c == Space
#else
  | otherwise = iswspace (ord c) /= 0
#endif
  where
    uc = fromIntegral (ord c) :: Word

-- | Fold each word of the stream using the supplied 'Fold'.
--
-- Definition:
--
-- >>> words = Stream.wordsBy Char.isSpace
--
-- Usage:
--
-- >>> Stream.toList $ Unicode.words Fold.toList (Stream.fromList " ab  cd   ef ")
-- ["ab","cd","ef"]
--
-- /Pre-release/
{-# INLINE words #-}
words :: Monad m => Fold m Char b -> Stream m Char -> Stream m b
words = D.wordsBy isSpace

-- | Unfold a stream to character streams using the supplied 'Unfold'
-- and concat the results suffixing a newline character @\\n@ to each stream.
--
-- Definition:
--
-- >>> unlines = Stream.unfoldEachEndBy '\n'
-- >>> unlines = Stream.unfoldEachEndBySeq "\n" Unfold.fromList
--
-- /Pre-release/
{-# INLINE unlines #-}
unlines :: MonadIO m => Unfold m a Char -> Stream m a -> Stream m Char
unlines = Stream.unfoldEachEndBy '\n'

-- | Unfold the elements of a stream to character streams using the supplied
-- 'Unfold' and concat the results with a whitespace character infixed between
-- the streams.
--
-- >>> unwords = Stream.unfoldEachSepBy ' '
-- >>> unwords = Stream.unfoldEachSepBySeq " " Unfold.fromList
--
-- /Pre-release/
{-# INLINE unwords #-}
unwords :: MonadIO m => Unfold m a Char -> Stream m a -> Stream m Char
unwords = Stream.unfoldEachSepBy ' '
