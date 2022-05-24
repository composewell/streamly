-- |
-- Module      : Streamly.Internal.Data.Stream.Eliminate
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Eliminate
    (
      fold
    , foldBreak
    , foldContinue

    -- * Running a 'Parser'
    -- "Streamly.Internal.Data.Parser".
    , parse
    , parseK
    , parseD
    , parseBreak
    , parseBreakD

    -- * Stream Deconstruction
    -- | foldr and foldl do not provide the remaining stream.  'uncons' is more
    -- general, as it can be used to implement those as well.  It allows to use
    -- the stream one element at a time, and we have the remaining stream all
    -- the time.
    , uncons

    -- * Right Folds
    , foldrM
    , foldr

    -- * Specific Fold Functions
    -- | Folds as functions of the shape @t m a -> m b@.
    --
    -- These functions are good to run individually but they do not compose
    -- well. Prefer writing folds as the 'Fold' data type. Use folds from
    -- "Streamly.Internal.Data.Fold" instead of using the functions in this
    -- section.
    --
    -- This section can possibly be removed in future.  Are these better in
    -- some case compared to 'Fold'? When the input stream is in CPS style
    -- (StreamK) we may want to rewrite the function call to CPS implementation
    -- of the fold through these definitions. Will that be more efficient for
    -- StreamK?

    -- -- ** Lazy Folds
    -- ** To Containers
    , toList

    -- * Multi-Stream folds
    -- Full equivalence
    , eqBy
    , cmpBy

    -- finding subsequences
    , isPrefixOf
    , isSuffixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix
    , stripSuffix
    )
where

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif

import Control.Monad.Catch (MonadThrow)
import Streamly.Internal.Data.Parser (Parser (..))
import Streamly.Internal.Data.Fold.Type (Fold)

import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Parser.ParserK.Type as PRK
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD.Type as StreamD

import Streamly.Internal.Data.Stream.Bottom
import Streamly.Internal.Data.Stream.Type

import Prelude hiding
       ( map, mapM, repeat, filter, drop, take, takeWhile, foldr , foldl, mapM_
       , product, elem, notElem, maximum, minimum, head, last, tail, length
       , null , reverse, init, and, or, lookup, foldr1, (!!) , splitAt, break
       , mconcat, sequence, all, any, sum)

#include "inline.hs"

-- $setup
-- >>> :m
-- >>> import Streamly.Internal.Data.Stream (Stream)
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold

-- | Fold a stream using the supplied left 'Fold' and reducing the resulting
-- expression strictly at each step. The behavior is similar to 'foldl''. A
-- 'Fold' can terminate early without consuming the full stream. See the
-- documentation of individual 'Fold's for termination behavior.
--
-- >>> Stream.fold Fold.sum ((Stream.unfold Unfold.enumerateFromTo (1::Int, 100)) :: Stream IO Int)
-- 5050
--
-- Folds never fail, therefore, they produce a default value even when no input
-- is provided. It means we can always fold an empty stream and get a valid
-- result.  For example:
--
-- >>> Stream.fold Fold.sum Stream.nil
-- 0
--
-- However, 'foldMany' on an empty stream results in an empty stream.
-- Therefore, @Stream.foldWith f@ is not the same as @Stream.head . Stream.foldMany
-- f@.
--
-- @fold f = Stream.parse (Parser.fromFold f)@
--
-- /Pre-release/
{-# INLINE fold #-}
fold :: Monad m => Fold m a b -> Stream m a -> m b
fold fld = StreamD.fold fld . toStreamD

------------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns @Just (a, ma)@, where @a@ is
-- the head of the stream and @ma@ its tail.
--
-- This can be used to do pretty much anything in an imperative manner, as it
-- just breaks down the stream into individual elements and we can loop over
-- them as we deem fit. For example, this can be used to convert a streamly
-- stream into other stream types.
--
-- All the folds in this module can be expressed in terms of 'uncons', however,
-- this is generally less efficient than specific folds because it takes apart
-- the stream one element at a time, therefore, does not take adavantage of
-- stream fusion.
--
-- 'foldBreak' is a more general way of consuming a stream piecemeal.
--
-- >>> :{
-- uncons xs = do
--     r <- Stream.foldBreak Fold.one xs
--     return $ case r of
--         (Nothing, _) -> Nothing
--         (Just h, t) -> Just (h, t)
-- :}
--
-- @since 0.1.0
{-# INLINE uncons #-}
uncons :: (Monad m) => Stream m a -> m (Maybe (a, Stream m a))
uncons m = fmap (fmap (fmap fromStreamK)) $ K.uncons (toStreamK m)

------------------------------------------------------------------------------
-- Running a Parser
------------------------------------------------------------------------------

-- | Parse a stream using the supplied ParserD 'PRD.Parser'.
--
-- /Internal/
--
{-# INLINE_NORMAL parseD #-}
parseD :: MonadThrow m => PRD.Parser m a b -> Stream m a -> m b
parseD p = D.parse p . toStreamD

-- | Parse a stream using the supplied ParserK 'PRK.Parser'.
--
-- /Internal/
{-# INLINE parseK #-}
parseK :: MonadThrow m => PRK.Parser m a b -> Stream m a -> m b
parseK = parse

-- | Parse a stream using the supplied 'Parser'.
--
-- Unlike folds, parsers may not always result in a valid output, they may
-- result in an error.  For example:
--
-- >>> Stream.parse (Parser.takeEQ 1 Fold.drain) Stream.nil
-- *** Exception: ParseError "takeEQ: Expecting exactly 1 elements, input terminated on 0"
--
-- Note:
--
-- @
-- fold f = Stream.parse (Parser.fromFold f)
-- @
--
-- @parse p@ is not the same as  @head . parseMany p@ on an empty stream.
--
-- /Pre-release/
--
{-# INLINE [3] parse #-}
parse :: MonadThrow m => Parser m a b -> Stream m a -> m b
parse = parseD . PRD.fromParserK

{-# INLINE_NORMAL parseBreakD #-}
parseBreakD :: MonadThrow m => PRD.Parser m a b -> Stream m a -> m (b, Stream m a)
parseBreakD parser strm = do
    (b, strmD) <- D.parseBreak parser (toStreamD strm)
    return $! (b, fromStreamD strmD)

-- | Parse a stream using the supplied 'Parser'.
--
-- /Internal/
--
{-# INLINE parseBreak #-}
parseBreak :: MonadThrow m => Parser m a b -> Stream m a -> m (b, Stream m a)
parseBreak p strm = fmap f $ K.parseBreak (PRD.fromParserK p) (toStreamK strm)

    where

    f (b, str) = (b, fromStreamK str)

------------------------------------------------------------------------------
-- Multi-stream folds
------------------------------------------------------------------------------

-- | Returns 'True' if the first stream is the same as or a prefix of the
-- second. A stream is a prefix of itself.
--
-- >>> Stream.isPrefixOf (Stream.unfold Unfold.fromList "hello") (Stream.unfold Unfold.fromList "hello" :: Stream IO Char)
-- True
--
-- @since 0.6.0
{-# INLINE isPrefixOf #-}
isPrefixOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
isPrefixOf m1 m2 = D.isPrefixOf (toStreamD m1) (toStreamD m2)

-- Note: isPrefixOf uses the prefix stream only once. In contrast, isSuffixOf
-- may use the suffix stream many times. To run in optimal memory we do not
-- want to buffer the suffix stream in memory therefore  we need an ability to
-- clone (or consume iStream multiple times) the suffix stream without any side
-- effects so thaStream multiple potential suffix matches can proceed in parallel
-- without buffering the suffix stream. For example, we may create the suffix
-- stream from a file handle, however, if we evaluate the stream multiple
-- times, once for each match, we will need a different file handle each time
-- which may exhaust the file descriptors. Instead, we want to share the same
-- underlying file descriptor, use pread on it to generate the stream and clone
-- the stream for each match. Therefore the suffix stream should be built in
-- such a way that it can be consumed multiple times without any problems.

-- XXX Can be implemented with better space/time complexity.
-- Space: @O(n)@ worst case where @n@ is the length of the suffix.

-- | Returns 'True' if the first stream is a suffix of the second. A stream is
-- considered a suffix of itself.
--
-- >>> Stream.isSuffixOf (Stream.unfold Unfold.fromList "hello") (Stream.unfold Unfold.fromList "hello" :: Stream IO Char)
-- True
--
-- Space: @O(n)@, buffers entire input stream and the suffix.
--
-- /Pre-release/
--
-- /Suboptimal/ - Help wanted.
--
{-# INLINE isSuffixOf #-}
isSuffixOf :: (Monad m, Eq a) => Stream m a -> Stream m a -> m Bool
isSuffixOf suffix stream = reverse suffix `isPrefixOf` reverse stream

-- | Returns 'True' if all the elements of the first stream occur, in order, in
-- the second stream. The elements do not have to occur consecutively. A stream
-- is a subsequence of itself.
--
-- >>> Stream.isSubsequenceOf (Stream.unfold Unfold.fromList "hlo") (Stream.unfold Unfold.fromList "hello" :: Stream IO Char)
-- True
--
-- @since 0.6.0
{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: (Eq a, Monad m) => Stream m a -> Stream m a -> m Bool
isSubsequenceOf m1 m2 = D.isSubsequenceOf (toStreamD m1) (toStreamD m2)

-- Note: If we want to return a Maybe value to know whether the
-- suffix/infix was present or not along with the stripped stream then
-- we need to buffer the whole input stream.
--
-- | @stripPrefix prefix stream@ strips @prefix@ from @stream@ if it is a
-- prefix of stream. Returns 'Nothing' if the stream does not start with the
-- given prefix, stripped stream otherwise. Returns @Just nil@ when the prefix
-- is the same as the stream.
--
-- See also "Streamly.Internal.Data.Stream.Nesting.dropPrefix".
--
-- Space: @O(1)@
--
-- @since 0.6.0
{-# INLINE stripPrefix #-}
stripPrefix
    :: (Eq a, Monad m)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripPrefix m1 m2 = fmap fromStreamD <$>
    D.stripPrefix (toStreamD m1) (toStreamD m2)

-- | Drops the given suffix from a stream. Returns 'Nothing' if the stream does
-- not end with the given suffix. Returns @Just nil@ when the suffix is the
-- same as the stream.
--
-- IStream may be more efficient to convert the stream to an Array and use
-- stripSuffix on that especially if the elements have a Storable or Prim
-- instance.
--
-- See also "Streamly.Internal.Data.Stream.Nesting.dropSuffix".
--
-- Space: @O(n)@, buffers the entire input stream as well as the suffix
--
-- /Pre-release/
{-# INLINE stripSuffix #-}
stripSuffix
    :: (Monad m, Eq a)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripSuffix m1 m2 = fmap reverse <$> stripPrefix (reverse m1) (reverse m2)
