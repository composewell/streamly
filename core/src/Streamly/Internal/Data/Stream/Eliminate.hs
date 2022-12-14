-- |
-- Module      : Streamly.Internal.Data.Stream.Eliminate
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains functions ending in the shape:
--
-- @
-- Stream m a -> m b
-- @
--
-- We call them stream folding functions, they reduce a stream @Stream m a@ to
-- a monadic value @m b@.

module Streamly.Internal.Data.Stream.Eliminate
    (
    -- * Running Examples
    -- $setup

    -- * Running a 'Fold'
    --  See "Streamly.Internal.Data.Fold".
      fold
    , foldBreak
    , foldBreak2
    , foldEither
    , foldEither2
    , foldConcat

    -- * Builders
    , build
    , buildl

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
    , init

    -- * Right Folds
    , foldrM
    , foldr

    -- * Left Folds
    -- Lazy left folds are useful only for reversing the stream
    , foldlS

    -- * Multi-Stream folds
    -- Full equivalence
    , eqBy
    , cmpBy

    -- finding subsequences
    , isPrefixOf
    , isInfixOf
    , isSuffixOf
    , isSubsequenceOf

    -- trimming sequences
    , stripPrefix
    -- , stripInfix
    , stripSuffix
    )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Parser (Parser (..), ParseError (..))
import Streamly.Internal.Data.Unboxed (Unbox)

import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Parser.ParserK.Type as PRK
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.StreamK as K

import Streamly.Internal.Data.Stream.Bottom
import Streamly.Internal.Data.Stream.Type

import Prelude hiding (foldr, init, reverse)

-- $setup
-- >>> :m
-- >>> import Streamly.Internal.Data.Stream (Stream)
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold

------------------------------------------------------------------------------
-- Deconstruction
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns @Just (a, ma)@, where @a@ is
-- the head of the stream and @ma@ its tail.
--
-- Properties:
--
-- >>> Nothing <- Stream.uncons Stream.nil
-- >>> Just ("a", t) <- Stream.uncons (Stream.cons "a" Stream.nil)
--
-- This can be used to consume the stream in an imperative manner one element
-- at a time, as it just breaks down the stream into individual elements and we
-- can loop over them as we deem fit. For example, this can be used to convert
-- a streamly stream into other stream types.
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
-- /CPS/
--
{-# INLINE uncons #-}
uncons :: Monad m => Stream m a -> m (Maybe (a, Stream m a))
uncons m = fmap (fmap (fmap fromStreamK)) $ K.uncons (toStreamK m)

-- | Extract all but the last element of the stream, if any.
--
-- Note: This will end up buffering the entire stream.
--
-- /Pre-release/
{-# INLINE init #-}
init :: Monad m => Stream m a -> m (Maybe (Stream m a))
init m = fmap (fmap fromStreamK) $ K.init $ toStreamK m

------------------------------------------------------------------------------
-- Right Folds
------------------------------------------------------------------------------

-- | Right associative/lazy pull fold. @foldrM build final stream@ constructs
-- an output structure using the step function @build@. @build@ is invoked with
-- the next input element and the remaining (lazy) tail of the output
-- structure. It builds a lazy output expression using the two. When the "tail
-- structure" in the output expression is evaluated it calls @build@ again thus
-- lazily consuming the input @stream@ until either the output expression built
-- by @build@ is free of the "tail" or the input is exhausted in which case
-- @final@ is used as the terminating case for the output structure. For more
-- details see the description in the previous section.
--
-- Example, determine if any element is 'odd' in a stream:
--
-- >>> s = Stream.fromList (2:4:5:undefined)
-- >>> step x xs = if odd x then return True else xs
-- >>> Stream.foldrM step (return False) s
-- True
--
{-# INLINE foldrM #-}
foldrM ::  Monad m => (a -> m b -> m b) -> m b -> Stream m a -> m b
foldrM step acc m = D.foldrM step acc $ toStreamD m

-- | Right fold, lazy for lazy monads and pure streams, and strict for strict
-- monads.
--
-- Please avoid using this routine in strict monads like IO unless you need a
-- strict right fold. This is provided only for use in lazy monads (e.g.
-- Identity) or pure streams. Note that with this signature it is not possible
-- to implement a lazy foldr when the monad @m@ is strict. In that case it
-- would be strict in its accumulator and therefore would necessarily consume
-- all its input.
--
-- >>> foldr f z = Stream.foldrM (\a b -> f a <$> b) (return z)
--
{-# INLINE foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> Stream m a -> m b
foldr f z = foldrM (\a b -> f a <$> b) (return z)

------------------------------------------------------------------------------
-- Left Folds
------------------------------------------------------------------------------

-- | Lazy left fold to a stream.
{-# INLINE foldlS #-}
foldlS ::
    (Stream m b -> a -> Stream m b) -> Stream m b -> Stream m a -> Stream m b
foldlS f z =
    fromStreamK
        . K.foldlS
            (\xs x -> toStreamK $ f (fromStreamK xs) x)
            (toStreamK z)
        . toStreamK

------------------------------------------------------------------------------
-- Running a Parser
------------------------------------------------------------------------------

-- | Parse a stream using the supplied ParserD 'PRD.Parser'.
--
-- /Internal/
--
{-# INLINE_NORMAL parseD #-}
parseD :: Monad m => PRD.Parser a m b -> Stream m a -> m (Either ParseError b)
parseD p = D.parse p . toStreamD

-- XXX Drive directly as parserK rather than converting to parserD first.

-- | Parse a stream using the supplied ParserK 'PRK.Parser'.
--
-- /Internal/
{-# INLINE parseK #-}
parseK :: Monad m => PRK.Parser a m b -> Stream m a -> m (Either ParseError b)
parseK = parse

-- | Parse a stream using the supplied 'Parser'.
--
-- Parsers (See "Streamly.Internal.Data.Parser") are more powerful folds that
-- add backtracking and error functionality to terminating folds. Unlike folds,
-- parsers may not always result in a valid output, they may result in an
-- error.  For example:
--
-- >>> Stream.parse (Parser.takeEQ 1 Fold.drain) Stream.nil
-- Left (ParseError "takeEQ: Expecting exactly 1 elements, input terminated on 0")
--
-- Note: @parse p@ is not the same as  @head . parseMany p@ on an empty stream.
--
{-# INLINE [3] parse #-}
parse :: Monad m => Parser a m b -> Stream m a -> m (Either ParseError b)
parse = parseD . PRD.fromParserK

{-# INLINE_NORMAL parseBreakD #-}
parseBreakD :: Monad m =>
    PRD.Parser a m b -> Stream m a -> m (Either ParseError b, Stream m a)
parseBreakD parser strm = do
    (b, strmD) <- D.parseBreak parser (toStreamD strm)
    return $! (b, fromStreamD strmD)

-- | Parse a stream using the supplied 'Parser'.
--
-- /CPS/
--
{-# INLINE parseBreak #-}
parseBreak :: Monad m => Parser a m b -> Stream m a -> m (Either ParseError b, Stream m a)
parseBreak p strm = fmap f $ K.parseBreak (PRD.fromParserK p) (toStreamK strm)

    where

    f (b, str) = (b, fromStreamK str)

------------------------------------------------------------------------------
-- Multi-stream folds
------------------------------------------------------------------------------

-- | Returns 'True' if the first stream is the same as or a prefix of the
-- second. A stream is a prefix of itself.
--
-- >>> Stream.isPrefixOf (Stream.fromList "hello") (Stream.fromList "hello" :: Stream IO Char)
-- True
--
{-# INLINE isPrefixOf #-}
isPrefixOf :: (Monad m, Eq a) => Stream m a -> Stream m a -> m Bool
isPrefixOf m1 m2 = D.isPrefixOf (toStreamD m1) (toStreamD m2)

-- | Returns 'True' if the first stream is an infix of the second. A stream is
-- considered an infix of itself.
--
-- >>> s = Stream.fromList "hello" :: Stream IO Char
-- >>> Stream.isInfixOf s s
-- True
--
-- Space: @O(n)@ worst case where @n@ is the length of the infix.
--
-- /Pre-release/
--
-- /Requires 'Storable' constraint/
--
{-# INLINE isInfixOf #-}
isInfixOf :: (MonadIO m, Eq a, Enum a, Unbox a)
    => Stream m a -> Stream m a -> m Bool
isInfixOf infx stream = do
    arr <- fold Array.write infx
    -- XXX can use breakOnSeq instead (when available)
    r <- D.null $ D.drop 1 $ D.splitOnSeq arr Fold.drain $ toStreamD stream
    return (not r)

-- Note: isPrefixOf uses the prefix stream only once. In contrast, isSuffixOf
-- may use the suffix stream many times. To run in optimal memory we do not
-- want to buffer the suffix stream in memory therefore  we need an ability to
-- clone (or consume it multiple times) the suffix stream without any side
-- effects so that multiple potential suffix matches can proceed in parallel
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
-- >>> Stream.isSuffixOf (Stream.fromList "hello") (Stream.fromList "hello" :: Stream IO Char)
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
-- >>> Stream.isSubsequenceOf (Stream.fromList "hlo") (Stream.fromList "hello" :: Stream IO Char)
-- True
--
{-# INLINE isSubsequenceOf #-}
isSubsequenceOf :: (Monad m, Eq a) => Stream m a -> Stream m a -> m Bool
isSubsequenceOf m1 m2 = D.isSubsequenceOf (toStreamD m1) (toStreamD m2)

-- Note: If we want to return a Maybe value to know whether the
-- suffix/infix was present or not along with the stripped stream then
-- we need to buffer the whole input stream.

-- | @stripPrefix prefix input@ strips the @prefix@ stream from the @input@
-- stream if it is a prefix of input. Returns 'Nothing' if the input does not
-- start with the given prefix, stripped input otherwise. Returns @Just nil@
-- when the prefix is the same as the input stream.
--
-- Space: @O(1)@
--
{-# INLINE stripPrefix #-}
stripPrefix
    :: (Monad m, Eq a)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripPrefix m1 m2 = fmap fromStreamD <$>
    D.stripPrefix (toStreamD m1) (toStreamD m2)

-- | Drops the given suffix from a stream. Returns 'Nothing' if the stream does
-- not end with the given suffix. Returns @Just nil@ when the suffix is the
-- same as the stream.
--
-- It may be more efficient to convert the stream to an Array and use
-- stripSuffix on that especially if the elements have a Storable or Prim
-- instance.
--
-- See also "Streamly.Internal.Data.Stream.Reduce.dropSuffix".
--
-- Space: @O(n)@, buffers the entire input stream as well as the suffix
--
-- /Pre-release/
{-# INLINE stripSuffix #-}
stripSuffix
    :: (Monad m, Eq a)
    => Stream m a -> Stream m a -> m (Maybe (Stream m a))
stripSuffix m1 m2 = fmap reverse <$> stripPrefix (reverse m1) (reverse m2)
