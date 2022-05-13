-- |
-- Module      : Streamly.Internal.Data.Stream.Reduce
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Reduce streams by streams, folds or parsers.

module Streamly.Internal.Data.Stream.Reduce
    (
    -- * Reduce By Streams
      dropPrefix
    , dropInfix
    , dropSuffix

    -- * Reduce By Folds
    -- |
    -- Reduce a stream by folding or parsing chunks of the stream.  Functions
    -- generally ending in these shapes:
    --
    -- @
    -- f (Fold m a b) -> Stream m a -> Stream m b
    -- f (Parser a m b) -> Stream m a -> Stream m b
    -- @

    -- ** Generic Folding
    -- | Apply folds on a stream.
    , foldMany
    , foldManyPost
    , refoldMany
    , foldSequence
    , foldIterateM
    , refoldIterateM

    -- ** Chunking
    -- | Element unaware grouping.
    , arraysOf

    -- ** Splitting
    -- XXX Implement these as folds or parsers instead.
    , splitOnSuffixSeqAny
    , splitOnPrefix
    , splitOnAny

    -- * Reduce By Parsers
    -- ** Generic Parsing
    -- | Apply parsers on a stream.
    , parseMany
    , parseManyD
    , parseManyTill
    , parseSequence
    , parseIterate

    )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Internal.Data.Array.Type (Array)
import Streamly.Internal.Data.Fold.Type (Fold (..))
import Streamly.Internal.Data.Parser (Parser (..))
import Streamly.Internal.Data.Refold.Type (Refold (..))
import Streamly.Internal.Data.Stream.Bottom (foldManyPost)
import Streamly.Internal.Data.Stream.Type (Stream, fromStreamD, toStreamD)
import Streamly.Internal.Data.Unboxed (Unbox)

import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Parser.ParserD as ParserD
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Prelude hiding (concatMap, map)

-- $setup
-- >>> :m
-- >>> import Prelude hiding (zipWith, concatMap, concat)
-- >>> import Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Array as Array

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- | Drop prefix from the input stream if present.
--
-- Space: @O(1)@
--
-- /Unimplemented/
{-# INLINE dropPrefix #-}
dropPrefix ::
    -- (Monad m, Eq a) =>
    Stream m a -> Stream m a -> Stream m a
dropPrefix = error "Not implemented yet!"

-- | Drop all matching infix from the input stream if present. Infix stream
-- may be consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the infix.
--
-- /Unimplemented/
{-# INLINE dropInfix #-}
dropInfix ::
    -- (Monad m, Eq a) =>
    Stream m a -> Stream m a -> Stream m a
dropInfix = error "Not implemented yet!"

-- | Drop suffix from the input stream if present. Suffix stream may be
-- consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the suffix.
--
-- /Unimplemented/
{-# INLINE dropSuffix #-}
dropSuffix ::
    -- (Monad m, Eq a) =>
    Stream m a -> Stream m a -> Stream m a
dropSuffix = error "Not implemented yet!"

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

-- | Apply a 'Fold' repeatedly on a stream and emit the results in the
-- output stream. Unlike 'foldManyPost' it evaluates the fold after the stream,
-- therefore, an empty input stream results in an empty output stream.
--
-- Definition:
--
-- >>> foldMany f = Stream.parseMany (Parser.fromFold f)
--
-- Example, empty stream:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> fmany = Stream.fold Fold.toList . Stream.foldMany f
-- >>> fmany $ Stream.fromList []
-- []
--
-- Example, last fold empty:
--
-- >>> fmany $ Stream.fromList [1..4]
-- [3,7]
--
-- Example, last fold non-empty:
--
-- >>> fmany $ Stream.fromList [1..5]
-- [3,7,5]
--
-- Note that using a closed fold e.g. @Fold.take 0@, would result in an
-- infinite stream on a non-empty input stream.
--
{-# INLINE foldMany #-}
foldMany
    :: Monad m
    => Fold m a b
    -> Stream m a
    -> Stream m b
foldMany f m = fromStreamD $ D.foldMany f (toStreamD m)

-- | Like 'foldMany' but using the 'Refold' type instead of 'Fold'.
--
-- /Pre-release/
{-# INLINE refoldMany #-}
refoldMany :: Monad m =>
    Refold m c a b -> m c -> Stream m a -> Stream m b
refoldMany f action = fromStreamD . D.refoldMany f action . toStreamD

-- | Apply a stream of folds to an input stream and emit the results in the
-- output stream.
--
-- /Unimplemented/
--
{-# INLINE foldSequence #-}
foldSequence
       :: -- Monad m =>
       Stream m (Fold m a b)
    -> Stream m a
    -> Stream m b
foldSequence _f _m = undefined

-- | Iterate a fold generator on a stream. The initial value @b@ is used to
-- generate the first fold, the fold is applied on the stream and the result of
-- the fold is used to generate the next fold and so on.
--
-- >>> import Data.Monoid (Sum(..))
-- >>> f x = return (Fold.take 2 (Fold.sconcat x))
-- >>> s = fmap Sum $ Stream.fromList [1..10]
-- >>> Stream.fold Fold.toList $ fmap getSum $ Stream.foldIterateM f (pure 0) s
-- [3,10,21,36,55,55]
--
-- This is the streaming equivalent of monad like sequenced application of
-- folds where next fold is dependent on the previous fold.
--
-- /Pre-release/
--
{-# INLINE foldIterateM #-}
foldIterateM ::
       Monad m => (b -> m (Fold m a b)) -> m b -> Stream m a -> Stream m b
foldIterateM f i m = fromStreamD $ D.foldIterateM f i (toStreamD m)

-- | Like 'foldIterateM' but using the 'Refold' type instead. This could be
-- much more efficient due to stream fusion.
--
-- /Internal/
{-# INLINE refoldIterateM #-}
refoldIterateM :: Monad m =>
    Refold m b a b -> m b -> Stream m a -> Stream m b
refoldIterateM c i m = fromStreamD $ D.refoldIterateM c i (toStreamD m)

------------------------------------------------------------------------------
-- Splitting
------------------------------------------------------------------------------

-- Implement this as a fold or a parser instead.
-- This can be implemented easily using Rabin Karp
-- | Split post any one of the given patterns.
--
-- /Unimplemented/
{-# INLINE splitOnSuffixSeqAny #-}
splitOnSuffixSeqAny :: -- (Monad m, Unboxed a, Integral a) =>
    [Array a] -> Fold m a b -> Stream m a -> Stream m b
splitOnSuffixSeqAny _subseq _f _m = undefined
    -- D.fromStreamD $ D.splitPostAny f subseq (D.toStreamD m)

-- | Split on a prefixed separator element, dropping the separator.  The
-- supplied 'Fold' is applied on the split segments.
--
-- @
-- > splitOnPrefix' p xs = Stream.toList $ Stream.splitOnPrefix p (Fold.toList) (Stream.fromList xs)
-- > splitOnPrefix' (== '.') ".a.b"
-- ["a","b"]
-- @
--
-- An empty stream results in an empty output stream:
-- @
-- > splitOnPrefix' (== '.') ""
-- []
-- @
--
-- An empty segment consisting of only a prefix is folded to the default output
-- of the fold:
--
-- @
-- > splitOnPrefix' (== '.') "."
-- [""]
--
-- > splitOnPrefix' (== '.') ".a.b."
-- ["a","b",""]
--
-- > splitOnPrefix' (== '.') ".a..b"
-- ["a","","b"]
--
-- @
--
-- A prefix is optional at the beginning of the stream:
--
-- @
-- > splitOnPrefix' (== '.') "a"
-- ["a"]
--
-- > splitOnPrefix' (== '.') "a.b"
-- ["a","b"]
-- @
--
-- 'splitOnPrefix' is an inverse of 'intercalatePrefix' with a single element:
--
-- > Stream.intercalatePrefix (Stream.fromPure '.') Unfold.fromList . Stream.splitOnPrefix (== '.') Fold.toList === id
--
-- Assuming the input stream does not contain the separator:
--
-- > Stream.splitOnPrefix (== '.') Fold.toList . Stream.intercalatePrefix (Stream.fromPure '.') Unfold.fromList === id
--
-- /Unimplemented/
{-# INLINE splitOnPrefix #-}
splitOnPrefix :: -- (IsStream t, MonadCatch m) =>
    (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitOnPrefix _predicate _f = undefined
    -- parseMany (Parser.sliceBeginBy predicate f)

-- Int list examples for splitOn:
--
-- >>> splitList [] [1,2,3,3,4]
-- > [[1],[2],[3],[3],[4]]
--
-- >>> splitList [5] [1,2,3,3,4]
-- > [[1,2,3,3,4]]
--
-- >>> splitList [1] [1,2,3,3,4]
-- > [[],[2,3,3,4]]
--
-- >>> splitList [4] [1,2,3,3,4]
-- > [[1,2,3,3],[]]
--
-- >>> splitList [2] [1,2,3,3,4]
-- > [[1],[3,3,4]]
--
-- >>> splitList [3] [1,2,3,3,4]
-- > [[1,2],[],[4]]
--
-- >>> splitList [3,3] [1,2,3,3,4]
-- > [[1,2],[4]]
--
-- >>> splitList [1,2,3,3,4] [1,2,3,3,4]
-- > [[],[]]

-- This can be implemented easily using Rabin Karp
-- | Split on any one of the given patterns.
--
-- /Unimplemented/
--
{-# INLINE splitOnAny #-}
splitOnAny :: -- (Monad m, Unboxed a, Integral a) =>
    [Array a] -> Fold m a b -> Stream m a -> Stream m b
splitOnAny _subseq _f _m =
    undefined -- D.fromStreamD $ D.splitOnAny f subseq (D.toStreamD m)

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- | Apply a 'Parser' repeatedly on a stream and emit the parsed values in the
-- output stream.
--
-- This is the streaming equivalent of the 'Streamly.Internal.Data.Parser.many'
-- parse combinator.
--
-- >>> s = Stream.fromList [1..10]
-- >>> parser = Parser.takeBetween 0 2 Fold.sum
-- >>> Stream.fold Fold.toList $ Stream.parseMany parser s
-- [3,7,11,15,19]
--
-- foldMany f = Stream.parseMany (Parser.fromFold f)
--
-- Known Issues: When the parser fails there is no way to get the remaining
-- stream.
--
{-# INLINE parseMany #-}
parseMany
    :: MonadThrow m
    => Parser a m b
    -> Stream m a
    -> Stream m b
parseMany p m =
    fromStreamD $ D.parseMany (ParserD.fromParserK p) (toStreamD m)

-- | Same as parseMany but for StreamD streams.
--
-- /Internal/
--
{-# INLINE parseManyD #-}
parseManyD
    :: MonadThrow m
    => ParserD.Parser a m b
    -> Stream m a
    -> Stream m b
parseManyD p m =
    fromStreamD $ D.parseMany p (toStreamD m)

-- | Apply a stream of parsers to an input stream and emit the results in the
-- output stream.
--
-- /Unimplemented/
--
{-# INLINE parseSequence #-}
parseSequence
       :: -- Monad m =>
       Stream m (Parser a m b)
    -> Stream m a
    -> Stream m b
parseSequence _f _m = undefined

-- XXX Change the parser arguments' order

-- | @parseManyTill collect test stream@ tries the parser @test@ on the input,
-- if @test@ fails it backtracks and tries @collect@, after @collect@ succeeds
-- @test@ is tried again and so on. The parser stops when @test@ succeeds.  The
-- output of @test@ is discarded and the output of @collect@ is emitted in the
-- output stream. The parser fails if @collect@ fails.
--
-- /Unimplemented/
--
{-# INLINE parseManyTill #-}
parseManyTill ::
    -- MonadThrow m =>
       Parser a m b
    -> Parser a m x
    -> t m a
    -> t m b
parseManyTill = undefined

-- | Iterate a parser generating function on a stream. The initial value @b@ is
-- used to generate the first parser, the parser is applied on the stream and
-- the result is used to generate the next parser and so on.
--
-- >>> import Data.Monoid (Sum(..))
-- >>> s = Stream.fromList [1..10]
-- >>> Stream.fold Fold.toList $ fmap getSum $ Stream.parseIterate (\b -> Parser.takeBetween 0 2 (Fold.sconcat b)) 0 $ fmap Sum s
-- [3,10,21,36,55,55]
--
-- This is the streaming equivalent of monad like sequenced application of
-- parsers where next parser is dependent on the previous parser.
--
-- /Pre-release/
--
{-# INLINE parseIterate #-}
parseIterate
    :: MonadThrow m
    => (b -> Parser a m b)
    -> b
    -> Stream m a
    -> Stream m b
parseIterate f i m = fromStreamD $
    D.parseIterate (ParserD.fromParserK . f) i (toStreamD m)

------------------------------------------------------------------------------
-- Chunking
------------------------------------------------------------------------------

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- >>> arraysOf n = Stream.foldMany (Array.writeN n)
--
-- /Pre-release/
{-# INLINE arraysOf #-}
arraysOf :: (MonadIO m, Unbox a)
    => Int -> Stream m a -> Stream m (Array a)
arraysOf n = fromStreamD . Array.arraysOf n . toStreamD
