{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Reduce
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Reduce streams by streams, folds or parsers.

module Streamly.Internal.Data.Stream.IsStream.Reduce {-# DEPRECATED "Please use \"Streamly.Data.Stream.*\" instead." #-}
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
    -- f (Fold m a b) -> t m a -> t m b
    -- f (Parser a m b) -> t m a -> t m b
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
    , chunksOf
    , arraysOf
    , intervalsOf
    , chunksOfTimeout

    -- ** Splitting
    -- | Streams can be sliced into segments in space or in time. We use the
    -- term @chunk@ to refer to a spatial length of the stream (spatial window)
    -- and the term @session@ to refer to a length in time (time window).

    -- -- *** Using Element Separators
    , splitOn
    , splitOnSuffix
    , splitOnPrefix
    , splitOnAny

    -- , splitBy
    , splitWithSuffix
    -- , splitByPrefix

    -- -- *** Splitting By Sequences
    , splitBySeq
    , splitOnSeq
    , splitOnSuffixSeq
    -- , splitOnPrefixSeq

    -- Keeping the delimiters
    , splitWithSuffixSeq
    -- , splitByPrefixSeq
    -- , wordsBySeq

    -- Splitting using multiple sequence separators
    -- , splitOnAnySeq
    , splitOnSuffixSeqAny
    -- , splitOnAnyPrefixSeq

    -- -- *** Splitting By Streams
    -- -- | Splitting a stream using another stream as separator.

    -- ** Keyed Window Classification

    -- | Split the stream into chunks or windows by position or time. Each
    -- window can be associated with a key, all events associated with a
    -- particular key in the window can be folded to a single result.  The
    -- window termination can be dynamically controlled by the fold.
    --
    -- The term "chunk" is used for a window defined by position of elements
    -- and the term "session" is used for a time window.

    -- *** Tumbling Windows
    -- | A new window starts after the previous window is finished.

    -- , classifyChunksOf
    , classifySessionsByGeneric
    , classifySessionsBy
    , classifySessionsOf

    -- *** Keep Alive Windows
    -- | The window size is extended if an event arrives within the specified
    -- window size. This can represent sessions with idle or inactive timeout.

    -- , classifyKeepAliveChunks
    , classifyKeepAliveSessions

    {-
    -- *** Sliding Windows
    -- | A new window starts after the specified slide from the previous
    -- window. Therefore windows can overlap.
    , classifySlidingChunks
    , classifySlidingSessions
    -- *** Sliding Window Buffers
    -- , slidingChunkBuffer
    -- , slidingSessionBuffer
    -}

    -- * Reduce By Parsers
    -- ** Generic Parsing
    -- | Apply parsers on a stream.
    , parseMany
    , parseManyD
    , parseManyTill
    , parseSequence
    , parseIterate

    -- ** Grouping
    -- In imperative terms, grouped folding can be considered as a nested loop
    -- where we loop over the stream to group elements and then loop over
    -- individual groups to fold them to a single value that is yielded in the
    -- output stream.

    , wordsBy -- stripAndCompactBy
    , wordsOn
    , groups
    , groupsBy
    , groupsByRolling

    -- -- *** Searching Sequences
    -- , seqIndices -- search a sequence in the stream

    -- -- *** Searching Multiple Sequences
    -- , seqIndicesAny -- search any of the given sequence in the stream

    -- -- -- ** Searching Streams
    -- -- | Finding a stream within another stream.

    -- * Nested splitting
    , splitInnerBy
    , splitInnerBySuffix
    )
where

#include "inline.hs"

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Heap (Entry(..))
import Data.Kind (Type)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Proxy (Proxy(..))
import Foreign.Storable (Storable)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Fold (Fold (..))
import Streamly.Internal.Data.IsMap (IsMap(..))
import Streamly.Internal.Data.Refold.Type (Refold (..))
import Streamly.Internal.Data.Parser (Parser (..), ParseError)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.Data.Stream.IsStream.Common
    ( fold
    , interjectSuffix
    , intersperseM
    , map
    , scanlMAfter'
    , foldManyPost
    , splitOnSeq
    , fromPure)
import Streamly.Internal.Data.Stream.IsStream.Type
    (IsStream(..), fromStreamD, toStreamD, cons)
import Streamly.Internal.Data.Stream.Serial(toStreamK)
import Streamly.Internal.Data.Time.Units
       ( AbsTime, MilliSecond64(..), addToAbsTime, toRelTime
       , toAbsTime)
import Streamly.Data.MutByteArray (Unbox)

import qualified Data.Heap as H
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Array as A
    (chunksOf, read)
import qualified Streamly.Internal.Data.Fold as FL
    (Fold, Step(..), takeEndBy_, takeEndBy, catMaybes, take)
import qualified Streamly.Internal.Data.IsMap as IsMap
import qualified Streamly.Internal.Data.Parser as PRD (Parser(..))
import qualified Streamly.Internal.Data.Stream.IsStream.Type as IsStream
import qualified Streamly.Internal.Data.Stream as D
    ( foldMany
    , groupsOf
    , refoldMany
    , foldIterateM
    , refoldIterateM
    , parseManyD
    , parseIterateD
    , groupsBy
    , groupsRollingBy
    , wordsBy
    , splitOnSuffixSeq
    , splitInnerBy
    , splitInnerBySuffix
    )
import qualified Streamly.Internal.Data.Stream.IsStream.Expand as Expand
import qualified Streamly.Internal.Data.Stream.IsStream.Transform as Transform

import Prelude hiding (concatMap, map)

-- $setup
-- >>> :m
-- >>> :set -fno-warn-deprecations
-- >>> import Prelude hiding (zipWith, concatMap, concat)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Streamly.Internal.Data.Stream.IsStream as Stream
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
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropPrefix = error "Not implemented yet!"

-- | Drop all matching infix from the input stream if present. Infix stream
-- may be consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the infix.
--
-- /Unimplemented/
{-# INLINE dropInfix #-}
dropInfix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropInfix = error "Not implemented yet!"

-- | Drop suffix from the input stream if present. Suffix stream may be
-- consumed multiple times.
--
-- Space: @O(n)@ where n is the length of the suffix.
--
-- /Unimplemented/
{-# INLINE dropSuffix #-}
dropSuffix ::
    -- (Eq a, IsStream t, Monad m) =>
    t m a -> t m a -> t m a
dropSuffix = error "Not implemented yet!"

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

-- Splitting operations that take a predicate and a Fold can be
-- expressed using parseMany. Operations like chunksOf, intervalsOf, split*,
-- can be expressed using parseMany when used with an appropriate Parser.
--
-- XXX We need takeGE/takeBetween to implement "some" using "many".

-- | Apply a 'Fold' repeatedly on a stream and emit the fold outputs in the
-- output stream.
--
-- To sum every two contiguous elements in a stream:
--
-- >>> f = Fold.take 2 Fold.sum
-- >>> Stream.toList $ Stream.foldMany f $ Stream.fromList [1..10]
-- [3,7,11,15,19]
--
-- On an empty stream the output is empty:
--
-- >>> Stream.toList $ Stream.foldMany f $ Stream.fromList []
-- []
--
-- Note @Stream.foldMany (Fold.take 0)@ would result in an infinite loop in a
-- non-empty stream.
--
-- @since 0.8.0
--
{-# INLINE foldMany #-}
foldMany
    :: (IsStream t, Monad m)
    => Fold m a b
    -> t m a
    -> t m b
foldMany f m = fromStreamD $ D.foldMany f (toStreamD m)

-- | Like 'foldMany' but using the 'Refold' type instead of 'Fold'.
--
-- /Pre-release/
{-# INLINE refoldMany #-}
refoldMany :: (IsStream t, Monad m) =>
    Refold m c a b -> m c -> t m a -> t m b
refoldMany f action = fromStreamD . D.refoldMany f action . toStreamD

-- | Apply a stream of folds to an input stream and emit the results in the
-- output stream.
--
-- /Unimplemented/
--
{-# INLINE foldSequence #-}
foldSequence
       :: -- (IsStream t, Monad m) =>
       t m (Fold m a b)
    -> t m a
    -> t m b
foldSequence _f _m = undefined

-- | Iterate a fold generator on a stream. The initial value @b@ is used to
-- generate the first fold, the fold is applied on the stream and the result of
-- the fold is used to generate the next fold and so on.
--
-- @
-- >>> import Data.Monoid (Sum(..))
-- >>> f x = return (Fold.take 2 (Fold.sconcat x))
-- >>> s = Stream.map Sum $ Stream.fromList [1..10]
-- >>> Stream.toList $ Stream.map getSum $ Stream.foldIterateM f (pure 0) s
-- [3,10,21,36,55,55]
--
-- @
--
-- This is the streaming equivalent of monad like sequenced application of
-- folds where next fold is dependent on the previous fold.
--
-- /Pre-release/
--
{-# INLINE foldIterateM #-}
foldIterateM ::
       (IsStream t, Monad m) => (b -> m (Fold m a b)) -> m b -> t m a -> t m b
foldIterateM f i m = fromStreamD $ D.foldIterateM f i (toStreamD m)

-- | Like 'foldIterateM' but using the 'Refold' type instead. This could be
-- much more efficient due to stream fusion.
--
-- /Internal/
{-# INLINE refoldIterateM #-}
refoldIterateM :: (IsStream t, Monad m) =>
    Refold m b a b -> m b -> t m a -> t m b
refoldIterateM c i m = fromStreamD $ D.refoldIterateM c i (toStreamD m)

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

-- | Apply a 'Parser' repeatedly on a stream and emit the parsed values in the
-- output stream.
--
-- This is the streaming equivalent of the 'Streamly.Internal.Data.Parser.many'
-- parse combinator.
--
-- >>> Stream.toList $ Stream.parseMany (Parser.takeBetween 0 2 Fold.sum) $ Stream.fromList [1..10]
-- [Right 3,Right 7,Right 11,Right 15,Right 19]
--
-- @
-- > Stream.toList $ Stream.parseMany (Parser.line Fold.toList) $ Stream.fromList "hello\\nworld"
-- ["hello\\n","world"]
--
-- @
--
-- @
-- foldMany f = parseMany (fromFold f)
-- @
--
-- Known Issues: When the parser fails there is no way to get the remaining
-- stream.
--
-- /Pre-release/
--
{-# INLINE parseMany #-}
parseMany
    :: (IsStream t, Monad m)
    => Parser a m b
    -> t m a
    -> t m (Either ParseError b)
parseMany p m =
    fromStreamD $ D.parseManyD p (toStreamD m)

-- | Same as parseMany but for StreamD streams.
--
-- /Internal/
--
{-# INLINE parseManyD #-}
parseManyD
    :: (IsStream t, Monad m)
    => PRD.Parser a m b
    -> t m a
    -> t m (Either ParseError b)
parseManyD p m =
    fromStreamD $ D.parseManyD p (toStreamD m)

-- | Apply a stream of parsers to an input stream and emit the results in the
-- output stream.
--
-- /Unimplemented/
--
{-# INLINE parseSequence #-}
parseSequence
       :: -- (IsStream t, Monad m) =>
       t m (Parser a m b)
    -> t m a
    -> t m b
parseSequence _f _m = undefined

-- XXX Change the parser arguments' order
--
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
    -- (IsStream t, MonadThrow m) =>
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
-- >>> Stream.toList $ fmap getSum $ Stream.rights $ Stream.parseIterate (\b -> Parser.takeBetween 0 2 (Fold.sconcat b)) (Sum 0) $ fmap Sum $ Stream.fromList [1..10]
-- [3,10,21,36,55,55]
--
-- This is the streaming equivalent of monad like sequenced application of
-- parsers where next parser is dependent on the previous parser.
--
-- /Pre-release/
--
{-# INLINE parseIterate #-}
parseIterate
    :: (IsStream t, Monad m)
    => (b -> Parser a m b)
    -> b
    -> t m a
    -> t m (Either ParseError b)
parseIterate f i m = fromStreamD $
    D.parseIterateD f i (toStreamD m)

------------------------------------------------------------------------------
-- Grouping
------------------------------------------------------------------------------

-- | @groupsBy cmp f $ S.fromList [a,b,c,...]@ assigns the element @a@ to the
-- first group, if @b \`cmp` a@ is 'True' then @b@ is also assigned to the same
-- group.  If @c \`cmp` a@ is 'True' then @c@ is also assigned to the same
-- group and so on. When the comparison fails a new group is started. Each
-- group is folded using the fold @f@ and the result of the fold is emitted in
-- the output stream.
--
-- >>> Stream.toList $ Stream.groupsBy (>) Fold.toList $ Stream.fromList [1,3,7,0,2,5]
-- [[1,3,7],[0,2,5]]
--
-- @since 0.7.0
{-# INLINE groupsBy #-}
groupsBy
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsBy cmp f m = fromStreamD $ D.groupsBy cmp f (toStreamD m)

-- | Unlike @groupsBy@ this function performs a rolling comparison of two
-- successive elements in the input stream. @groupsByRolling cmp f $ S.fromList
-- [a,b,c,...]@ assigns the element @a@ to the first group, if @a \`cmp` b@ is
-- 'True' then @b@ is also assigned to the same group.  If @b \`cmp` c@ is
-- 'True' then @c@ is also assigned to the same group and so on. When the
-- comparison fails a new group is started. Each group is folded using the fold
-- @f@.
--
-- >>> Stream.toList $ Stream.groupsByRolling (\a b -> a + 1 == b) Fold.toList $ Stream.fromList [1,2,3,7,8,9]
-- [[1,2,3],[7,8,9]]
--
-- @since 0.7.0
{-# INLINE groupsByRolling #-}
groupsByRolling
    :: (IsStream t, Monad m)
    => (a -> a -> Bool)
    -> Fold m a b
    -> t m a
    -> t m b
groupsByRolling cmp f m =  fromStreamD $ D.groupsRollingBy cmp f (toStreamD m)

-- |
-- > groups = groupsBy (==)
-- > groups = groupsByRolling (==)
--
-- Groups contiguous spans of equal elements together in individual groups.
--
-- >>> Stream.toList $ Stream.groups Fold.toList $ Stream.fromList [1,1,2,2]
-- [[1,1],[2,2]]
--
-- @since 0.7.0
{-# INLINE groups #-}
groups :: (IsStream t, Monad m, Eq a) => Fold m a b -> t m a -> t m b
groups = groupsBy (==)

------------------------------------------------------------------------------
-- Splitting - by a predicate
------------------------------------------------------------------------------

-- In general we can use deintercalate for splitting.  Then we can also use
-- uniqBy to condense the separators.  One way to generalize splitting is to
-- output:
--
-- data Segment a b = Empty | Segment b | Separator a
--
-- XXX splitOn and splitOnSuffix have a different behavior on an empty stream,
-- is that desirable?

-- | Split on an infixed separator element, dropping the separator.  The
-- supplied 'Fold' is applied on the split segments.  Splits the stream on
-- separator elements determined by the supplied predicate, separator is
-- considered as infixed between two segments:
--
-- >>> splitOn' p xs = Stream.toList $ Stream.splitOn p Fold.toList (Stream.fromList xs)
-- >>> splitOn' (== '.') "a.b"
-- ["a","b"]
--
-- An empty stream is folded to the default value of the fold:
--
-- >>> splitOn' (== '.') ""
-- [""]
--
-- If one or both sides of the separator are missing then the empty segment on
-- that side is folded to the default output of the fold:
--
-- >>> splitOn' (== '.') "."
-- ["",""]
--
-- >>> splitOn' (== '.') ".a"
-- ["","a"]
--
-- >>> splitOn' (== '.') "a."
-- ["a",""]
--
-- >>> splitOn' (== '.') "a..b"
-- ["a","","b"]
--
-- splitOn is an inverse of intercalating single element:
--
-- > Stream.intercalate (Stream.fromPure '.') Unfold.fromList . Stream.splitOn (== '.') Fold.toList === id
--
-- Assuming the input stream does not contain the separator:
--
-- > Stream.splitOn (== '.') Fold.toList . Stream.intercalate (Stream.fromPure '.') Unfold.fromList === id
--
-- @since 0.7.0

{-# INLINE splitOn #-}
splitOn
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOn predicate f =
    -- We can express the infix splitting in terms of optional suffix split
    -- fold.  After applying a suffix split fold repeatedly if the last segment
    -- ends with a suffix then we need to return the default output of the fold
    -- after that to make it an infix split.
    --
    -- Alternately, we can also express it using an optional prefix split fold.
    -- If the first segment starts with a prefix then we need to emit the
    -- default output of the fold before that to make it an infix split, and
    -- then apply prefix split fold repeatedly.
    --
    -- Since a suffix split fold can be easily expressed using a
    -- non-backtracking fold, we use that.
    foldManyPost (FL.takeEndBy_ predicate f)

-- | Split on a suffixed separator element, dropping the separator.  The
-- supplied 'Fold' is applied on the split segments.
--
-- >>> splitOnSuffix' p xs = Stream.toList $ Stream.splitOnSuffix p Fold.toList (Stream.fromList xs)
-- >>> splitOnSuffix' (== '.') "a.b."
-- ["a","b"]
--
-- >>> splitOnSuffix' (== '.') "a."
-- ["a"]
--
-- An empty stream results in an empty output stream:
--
-- >>> splitOnSuffix' (== '.') ""
-- []
--
-- An empty segment consisting of only a suffix is folded to the default output
-- of the fold:
--
-- >>> splitOnSuffix' (== '.') "."
-- [""]
--
-- >>> splitOnSuffix' (== '.') "a..b.."
-- ["a","","b",""]
--
-- A suffix is optional at the end of the stream:
--
-- >>> splitOnSuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitOnSuffix' (== '.') ".a"
-- ["","a"]
--
-- >>> splitOnSuffix' (== '.') "a.b"
-- ["a","b"]
--
-- > lines = splitOnSuffix (== '\n')
--
-- 'splitOnSuffix' is an inverse of 'intercalateSuffix' with a single element:
--
-- > Stream.intercalateSuffix (Stream.fromPure '.') Unfold.fromList . Stream.splitOnSuffix (== '.') Fold.toList === id
--
-- Assuming the input stream does not contain the separator:
--
-- > Stream.splitOnSuffix (== '.') Fold.toList . Stream.intercalateSuffix (Stream.fromPure '.') Unfold.fromList === id
--
-- @since 0.7.0

{-# INLINE splitOnSuffix #-}
splitOnSuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOnSuffix predicate f = foldMany (FL.takeEndBy_ predicate f)

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
    (a -> Bool) -> Fold m a b -> t m a -> t m b
splitOnPrefix _predicate _f = undefined
    -- parseMany (Parser.sliceBeginBy predicate f)

-- | Like 'splitOn' after stripping leading, trailing, and repeated separators.
-- Therefore, @".a..b."@ with '.' as the separator would be parsed as
-- @["a","b"]@.  In other words, its like parsing words from whitespace
-- separated text.
--
-- >>> wordsBy' p xs = Stream.toList $ Stream.wordsBy p Fold.toList (Stream.fromList xs)
--
-- >>> wordsBy' (== ',') ""
-- []
--
-- >>> wordsBy' (== ',') ","
-- []
--
-- >>> wordsBy' (== ',') ",a,,b,"
-- ["a","b"]
--
-- > words = wordsBy isSpace
--
-- @since 0.7.0

-- It is equivalent to splitting in any of the infix/prefix/suffix styles
-- followed by removal of empty segments.
{-# INLINE wordsBy #-}
wordsBy
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
wordsBy predicate f m =
    fromStreamD $ D.wordsBy predicate f (toStreamD m)

-- | Like 'splitOnSuffix' but keeps the suffix attached to the resulting
-- splits.
--
-- >>> splitWithSuffix' p xs = Stream.toList $ splitWithSuffix p Fold.toList (Stream.fromList xs)
--
-- >>> splitWithSuffix' (== '.') ""
-- []
--
-- >>> splitWithSuffix' (== '.') "."
-- ["."]
--
-- >>> splitWithSuffix' (== '.') "a"
-- ["a"]
--
-- >>> splitWithSuffix' (== '.') ".a"
-- [".","a"]
--
-- >>> splitWithSuffix' (== '.') "a."
-- ["a."]
--
-- >>> splitWithSuffix' (== '.') "a.b"
-- ["a.","b"]
--
-- >>> splitWithSuffix' (== '.') "a.b."
-- ["a.","b."]
--
-- >>> splitWithSuffix' (== '.') "a..b.."
-- ["a.",".","b.","."]
--
-- @since 0.7.0

{-# INLINE splitWithSuffix #-}
splitWithSuffix
    :: (IsStream t, Monad m)
    => (a -> Bool) -> Fold m a b -> t m a -> t m b
splitWithSuffix predicate f = foldMany (FL.takeEndBy predicate f)

------------------------------------------------------------------------------
-- Splitting - on a delimiter sequence
------------------------------------------------------------------------------

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
splitOnAny :: -- (IsStream t, Monad m, Unbox a, Integral a) =>
    [Array a] -> Fold m a b -> t m a -> t m b
splitOnAny _subseq _f _m =
    undefined -- D.fromStreamD $ D.splitOnAny f subseq (D.toStreamD m)

-- XXX use a non-monadic intersperse to remove the MonadAsync constraint.
-- XXX Use two folds, one ring buffer fold for separator sequence and the other
-- split consumer fold. The input is fed to the ring fold first and the
-- rejected input is fed to the split fold. If the separator matches, the ring
-- fold would consume all.
--
-- | Like 'splitOnSeq' but splits the separator as well, as an infix token.
--
-- >>> splitOn'_ pat xs = Stream.toList $ Stream.splitBySeq (Array.fromList pat) Fold.toList (Stream.fromList xs)
--
-- >>> splitOn'_ "" "hello"
-- ["h","","e","","l","","l","","o"]
--
-- >>> splitOn'_ "hello" ""
-- [""]
--
-- >>> splitOn'_ "hello" "hello"
-- ["","hello",""]
--
-- >>> splitOn'_ "x" "hello"
-- ["hello"]
--
-- >>> splitOn'_ "h" "hello"
-- ["","h","ello"]
--
-- >>> splitOn'_ "o" "hello"
-- ["hell","o",""]
--
-- >>> splitOn'_ "e" "hello"
-- ["h","e","llo"]
--
-- >>> splitOn'_ "l" "hello"
-- ["he","l","","l","o"]
--
-- >>> splitOn'_ "ll" "hello"
-- ["he","ll","o"]
--
-- /Pre-release/
{-# INLINE splitBySeq #-}
splitBySeq
    :: (IsStream t, MonadAsync m, Storable a, Unbox a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitBySeq patt f m =
    intersperseM (fold f (fromStreamD $ A.read patt)) $ splitOnSeq patt f m

-- | Like 'splitSuffixBy' but the separator is a sequence of elements, instead
-- of a predicate for a single element.
--
-- >>> splitOnSuffixSeq_ pat xs = Stream.toList $ Stream.splitOnSuffixSeq (Array.fromList pat) Fold.toList (Stream.fromList xs)
--
-- >>> splitOnSuffixSeq_ "." ""
-- []
--
-- >>> splitOnSuffixSeq_ "." "."
-- [""]
--
-- >>> splitOnSuffixSeq_ "." "a"
-- ["a"]
--
-- >>> splitOnSuffixSeq_ "." ".a"
-- ["","a"]
--
-- >>> splitOnSuffixSeq_ "." "a."
-- ["a"]
--
-- >>> splitOnSuffixSeq_ "." "a.b"
-- ["a","b"]
--
-- >>> splitOnSuffixSeq_ "." "a.b."
-- ["a","b"]
--
-- >>> splitOnSuffixSeq_ "." "a..b.."
-- ["a","","b",""]
--
-- > lines = splitOnSuffixSeq "\n"
--
-- 'splitOnSuffixSeq' is an inverse of 'intercalateSuffix'. The following law
-- always holds:
--
-- > intercalateSuffix . splitOnSuffixSeq == id
--
-- The following law holds when the separator is non-empty and contains none of
-- the elements present in the input lists:
--
-- > splitSuffixOn . intercalateSuffix == id
--
-- >>> splitOnSuffixSeq pat f = Stream.foldMany (Fold.takeEndBySeq_ pat f)
--
-- /Pre-release/
{-# INLINE splitOnSuffixSeq #-}
splitOnSuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Unbox a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitOnSuffixSeq patt f m =
    fromStreamD $ D.splitOnSuffixSeq False patt f (toStreamD m)
-- XXX This may have a problem if the stream terminates and we start extracting
-- the fold and then the fold terminates before consuming all the buffered
-- input.  We have no way to supply the buffered input back to the driver.
-- splitOnSuffixSeq patt f =
--     foldMany (FL.takeEndBySeq_ patt f)

-- | Like 'splitOn' but drops any empty splits.
--
-- /Unimplemented/
{-# INLINE wordsOn #-}
wordsOn :: -- (IsStream t, Monad m, Unbox a, Eq a) =>
    Array a -> Fold m a b -> t m a -> t m b
wordsOn _subseq _f _m =
    undefined -- D.fromStreamD $ D.wordsOn f subseq (D.toStreamD m)

-- | Like 'splitOnSuffixSeq' but keeps the suffix intact in the splits.
--
-- >>> splitWithSuffixSeq' pat xs = Stream.toList $ Stream.splitWithSuffixSeq (Array.fromList pat) Fold.toList (Stream.fromList xs)
--
-- >>> splitWithSuffixSeq' "." ""
-- []
--
-- >>> splitWithSuffixSeq' "." "."
-- ["."]
--
-- >>> splitWithSuffixSeq' "." "a"
-- ["a"]
--
-- >>> splitWithSuffixSeq' "." ".a"
-- [".","a"]
--
-- >>> splitWithSuffixSeq' "." "a."
-- ["a."]
--
-- >>> splitWithSuffixSeq' "." "a.b"
-- ["a.","b"]
--
-- >>> splitWithSuffixSeq' "." "a.b."
-- ["a.","b."]
--
-- >>> splitWithSuffixSeq' "." "a..b.."
-- ["a.",".","b.","."]
--
-- >>> splitWithSuffixSeq pat f = Stream.foldMany (Fold.takeEndBySeq pat f)
--
-- /Pre-release/
{-# INLINE splitWithSuffixSeq #-}
splitWithSuffixSeq
    :: (IsStream t, MonadIO m, Storable a, Unbox a, Enum a, Eq a)
    => Array a -> Fold m a b -> t m a -> t m b
splitWithSuffixSeq patt f m =
    fromStreamD $ D.splitOnSuffixSeq True patt f (toStreamD m)
-- splitWithSuffixSeq patt f =
--     foldMany (FL.takeEndBySeq patt f)

-- This can be implemented easily using Rabin Karp
-- | Split post any one of the given patterns.
--
-- /Unimplemented/
{-# INLINE splitOnSuffixSeqAny #-}
splitOnSuffixSeqAny :: -- (IsStream t, Monad m, Unbox a, Integral a) =>
    [Array a] -> Fold m a b -> t m a -> t m b
splitOnSuffixSeqAny _subseq _f _m = undefined
    -- D.fromStreamD $ D.splitPostAny f subseq (D.toStreamD m)

------------------------------------------------------------------------------
-- Chunking
------------------------------------------------------------------------------

-- | Group the input stream into groups of @n@ elements each and then fold each
-- group using the provided fold function.
--
-- >>> Stream.toList $ Stream.chunksOf 2 Fold.sum (Stream.enumerateFromTo 1 10)
-- [3,7,11,15,19]
--
-- This can be considered as an n-fold version of 'take' where we apply
-- 'take' repeatedly on the leftover stream until the stream exhausts.
--
-- @chunksOf n f = foldMany (FL.take n f)@
--
-- @since 0.7.0
{-# INLINE chunksOf #-}
chunksOf
    :: (IsStream t, Monad m)
    => Int -> Fold m a b -> t m a -> t m b
chunksOf n f = fromStreamD . D.groupsOf n f . toStreamD

-- | @arraysOf n stream@ groups the elements in the input stream into arrays of
-- @n@ elements each.
--
-- Same as the following but may be more efficient:
--
-- > arraysOf n = Stream.foldMany (A.writeN n)
--
-- /Pre-release/
{-# INLINE arraysOf #-}
arraysOf :: (IsStream t, MonadIO m, Unbox a)
    => Int -> t m a -> t m (Array a)
arraysOf n = fromStreamD . A.chunksOf n . toStreamD

-- XXX we can implement this by repeatedly applying the 'lrunFor' fold.
-- XXX add this example after fixing the serial stream rate control
--
-- | Group the input stream into windows of @n@ second each and then fold each
-- group using the provided fold function.
--
-- >>> Stream.toList $ Stream.take 5 $ Stream.intervalsOf 1 Fold.sum $ Stream.constRate 2 $ Stream.enumerateFrom 1
-- [...,...,...,...,...]
--
-- @since 0.7.0
{-# INLINE intervalsOf #-}
intervalsOf
    :: (IsStream t, MonadAsync m)
    => Double -> Fold m a b -> t m a -> t m b
intervalsOf n f xs =
    splitWithSuffix isNothing (FL.catMaybes f)
        (interjectSuffix n (return Nothing) (map Just xs))

-- XXX This can be implemented more efficiently by sharing a Clock.Timer across
-- parallel threads and resetting it whenever a span is emitted.
--
-- | Like 'chunksOf' but if the chunk is not completed within the specified
-- time interval then emit whatever we have collected till now. The chunk
-- timeout is reset whenever a chunk is emitted. The granularity of the clock
-- is 100 ms.
--
-- >>> s = Stream.delayPost 0.3 $ Stream.fromList [1..1000]
-- >>> f = Stream.mapM_ print $ Stream.chunksOfTimeout 5 1 Fold.toList s
--
-- /Pre-release/
{-# INLINE chunksOfTimeout #-}
chunksOfTimeout :: (IsStream t, MonadAsync m, Functor (t m))
    => Int -> Double -> FL.Fold m a b -> t m a -> t m b
chunksOfTimeout n timeout f =
      map snd
    . classifySessionsBy
        0.1 False (const (return False)) timeout (FL.take n f)
    . Transform.timestamped
    . map ((),)

------------------------------------------------------------------------------
-- Windowed classification
------------------------------------------------------------------------------

-- TODO: To mark the position in space or time we can have Indexed or
-- TimeStamped types. That can make it easy to deal with the position indices
-- or timestamps.

------------------------------------------------------------------------------
-- Keyed Sliding Windows
------------------------------------------------------------------------------

{-
{-# INLINABLE classifySlidingChunks #-}
classifySlidingChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Int              -- ^ window slide
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifySlidingChunks wsize wslide (Fold step initial extract) str
    = undefined

-- XXX Another variant could be to slide the window on an event, e.g. in TCP we
-- slide the send window when an ack is received and we slide the receive
-- window when a sequence is complete. Sliding is stateful in case of TCP,
-- sliding releases the send buffer or makes data available to the user from
-- the receive buffer.
{-# INLINABLE classifySlidingSessions #-}
classifySlidingSessions
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ time window size
    -> Double         -- ^ window slide
    -> Fold m a b     -- ^ Fold to be applied to window events
    -> t m (k, a, Bool, AbsTime) -- ^ window key, data, close flag, timestamp
    -> t m (k, b)
classifySlidingSessions tick interval slide (Fold step initial extract) str
    = undefined
-}

------------------------------------------------------------------------------
-- Sliding Window Buffers
------------------------------------------------------------------------------

-- These buffered versions could be faster than concurrent incremental folds of
-- all overlapping windows as in many cases we may not need all the values to
-- compute the fold, we can just compute the result using the old value and new
-- value.  However, we may need the buffer once in a while, for example for
-- string search we usually compute the hash incrementally but when the hash
-- matches the hash of the pattern we need to compare the whole string.
--
-- XXX we should be able to implement sequence based splitting combinators
-- using this combinator.

{-
-- | Buffer n elements of the input in a ring buffer. When t new elements are
-- collected, slide the window to remove the same number of oldest elements,
-- insert the new elements, and apply an incremental fold on the sliding
-- window, supplying the outgoing elements, the new ring buffer as arguments.
slidingChunkBuffer
    :: (IsStream t, Monad m, Ord a, Unbox a)
    => Int -- window size
    -> Int -- window slide
    -> Fold m (Ring a, Array a) b
    -> t m a
    -> t m b
slidingChunkBuffer = undefined

-- Buffer n seconds worth of stream elements of the input in a radix tree.
-- Every t seconds, remove the items that are older than n seconds, and apply
-- an incremental fold on the sliding window, supplying the outgoing elements,
-- and the new radix tree buffer as arguments.
slidingSessionBuffer
    :: (IsStream t, Monad m, Ord a, Unbox a)
    => Int    -- window size
    -> Int    -- tick size
    -> Fold m (RTree a, Array a) b
    -> t m a
    -> t m b
slidingSessionBuffer = undefined
-}

------------------------------------------------------------------------------
-- Keyed Session Windows
------------------------------------------------------------------------------

{-
-- | Keyed variable size space windows. Close the window if we do not receive a
-- window event in the next "spaceout" elements.
{-# INLINABLE classifyChunksBy #-}
classifyChunksBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Bool  -- ^ reset the spaceout when a chunk window element is received
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyChunksBy spanout reset (Fold step initial extract) str = undefined

-- | Like 'classifyChunksOf' but the chunk size is reset if an element is
-- received within the chunk size window. The chunk gets closed only if no
-- element is received within the chunk window.
--
{-# INLINABLE classifyKeepAliveChunks #-}
classifyKeepAliveChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyKeepAliveChunks spanout = classifyChunksBy spanout True
-}

data SessionState t m f s b = SessionState
    { sessionCurTime :: !AbsTime  -- ^ time since last event
    , sessionEventTime :: !AbsTime -- ^ time as per last event
    -- We can use the Map size instead of maintaining a count, but if we have
    -- to switch to HashMap then it can be useful.
    , sessionCount :: !Int -- ^ total number of sessions in progress
    , sessionTimerHeap :: H.Heap (H.Entry AbsTime (Key f)) -- ^ heap for timeouts
    , sessionKeyValueMap :: f s -- ^ Stored sessions for keys
    , sessionOutputStream :: t (m :: Type -> Type) (Key f, b) -- ^ Completed sessions
    }

data SessionEntry s = LiveSession !AbsTime !s | ZombieSession

-- delete from map and output the fold accumulator
ejectEntry :: (Monad m, IsStream t, IsMap f) =>
    (acc -> m b)
    -> heap
    -> f entry
    -> t m (Key f, b)
    -> Int
    -> acc
    -> Key f
    -> m (heap, f entry, t m (Key f, b), Int)
ejectEntry extract hp mp out cnt acc key = do
    sess <- extract acc
    let out1 = (key, sess) `cons` out
    let mp1 = IsMap.mapDelete key mp
    return (hp, mp1, out1, cnt - 1)

{-# NOINLINE flush #-}
flush :: (Monad m, IsMap f, IsStream t) =>
       (s -> m b)
    -> SessionState t m f (SessionEntry s) b
    -> m (SessionState t m f (SessionEntry s) b)
flush extract session@SessionState{..} = do
    (hp', mp', out, count) <-
        ejectAll
            ( sessionTimerHeap
            , sessionKeyValueMap
            , IsStream.nil
            , sessionCount
            )
    return $ session
        { sessionCount = count
        , sessionTimerHeap = hp'
        , sessionKeyValueMap = mp'
        , sessionOutputStream = out
        }

    where

    ejectAll (hp, mp, out, !cnt) = do
        let hres = H.uncons hp
        case hres of
            Just (Entry _ key, hp1) -> do
                r <- case IsMap.mapLookup key mp of
                    Nothing -> return (hp1, mp, out, cnt)
                    Just ZombieSession ->
                        return (hp1, IsMap.mapDelete key mp, out, cnt)
                    Just (LiveSession _ acc) ->
                        ejectEntry extract hp1 mp out cnt acc key
                ejectAll r
            Nothing -> do
                assert (IsMap.mapNull mp) (return ())
                return (hp, mp, out, cnt)

{-# NOINLINE ejectOne #-}
ejectOne :: (IsMap f, IsStream t, Monad m) =>
       Bool
    -> (acc -> m b)
    -> ( H.Heap (Entry AbsTime (Key f))
       , f (SessionEntry acc)
       , t m (Key f, b)
       , Int
       )
    -> m ( H.Heap (Entry AbsTime (Key f))
         , f (SessionEntry acc)
         , t m (Key f, b), Int
         )
ejectOne reset extract = go

    where

    go (hp, mp, out, !cnt) = do
        let hres = H.uncons hp
        case hres of
            Just (Entry expiry key, hp1) ->
                case IsMap.mapLookup key mp of
                    Nothing -> go (hp1, mp, out, cnt)
                    Just ZombieSession ->
                        go (hp1, IsMap.mapDelete key mp, out, cnt)
                    Just (LiveSession expiry1 acc) -> do
                        if not reset || expiry1 <= expiry
                        then ejectEntry extract hp1 mp out cnt acc key
                        else
                            -- reset the session timeout and continue
                            let hp2 = H.insert (Entry expiry1 key) hp1
                            in go (hp2, mp, out, cnt)
            Nothing -> do
                assert (IsMap.mapNull mp) (return ())
                return (hp, mp, out, cnt)

{-# NOINLINE ejectExpired #-}
ejectExpired :: (IsMap f, IsStream t, Monad m) =>
       Bool
    -> (Int -> m Bool)
    -> (acc -> m b)
    -> SessionState t m f (SessionEntry acc) b
    -> AbsTime
    -> m (SessionState t m f (SessionEntry acc) b)
ejectExpired reset ejectPred extract session@SessionState{..} curTime = do
    (hp', mp', out, count) <-
        ejectLoop
            sessionTimerHeap sessionKeyValueMap IsStream.nil sessionCount
    return $ session
        { sessionCurTime = curTime
        , sessionCount = count
        , sessionTimerHeap = hp'
        , sessionKeyValueMap = mp'
        , sessionOutputStream = out
        }

    where

    ejectLoop hp mp out !cnt = do
        let hres = H.uncons hp
        case hres of
            Just (Entry expiry key, hp1) -> do
                (eject, force) <-
                    if curTime >= expiry
                    then return (True, False)
                    else do
                        r <- ejectPred cnt
                        return (r, r)
                if eject
                then
                    case IsMap.mapLookup key mp of
                        Nothing -> ejectLoop hp1 mp out cnt
                        Just ZombieSession ->
                            ejectLoop hp1 (IsMap.mapDelete key mp) out cnt
                        Just (LiveSession expiry1 acc) -> do
                            if expiry1 <= curTime || not reset || force
                            then do
                                (hp2,mp1,out1,cnt1) <-
                                    ejectEntry extract hp1 mp out cnt acc key
                                ejectLoop hp2 mp1 out1 cnt1
                            else
                                -- reset the session timeout and continue
                                let hp2 = H.insert (Entry expiry1 key) hp1
                                in ejectLoop hp2 mp out cnt
                else return (hp, mp, out, cnt)
            Nothing -> do
                assert (IsMap.mapNull mp) (return ())
                return (hp, mp, out, cnt)

-- XXX Use mutable IORef in accumulator
{-# INLINE classifySessionsByGeneric #-}
classifySessionsByGeneric
    :: forall t m f a b. (IsStream t, MonadAsync m, IsMap f)
    => Proxy (f :: (Type -> Type))
    -> Double         -- ^ timer tick in seconds
    -> Bool           -- ^ reset the timer when an event is received
    -> (Int -> m Bool) -- ^ predicate to eject sessions based on session count
    -> Double         -- ^ session timeout in seconds
    -> Fold m a b  -- ^ Fold to be applied to session data
    -> t m (AbsTime, (Key f, a)) -- ^ timestamp, (session key, session data)
    -> t m (Key f, b) -- ^ session key, fold result
classifySessionsByGeneric _ tick reset ejectPred tmout
    (Fold step initial extract final) input =
    Expand.unfoldMany
        (Unfold.lmap (toStreamK . sessionOutputStream) Unfold.fromStreamK)
        $ scanlMAfter' sstep (return szero) (flush final)
        $ interjectSuffix tick (return Nothing)
        $ map Just input

    where

    timeoutMs = toRelTime (round (tmout * 1000) :: MilliSecond64)
    tickMs = toRelTime (round (tick * 1000) :: MilliSecond64)
    szero = SessionState
        { sessionCurTime = toAbsTime (0 :: MilliSecond64)
        , sessionEventTime = toAbsTime (0 :: MilliSecond64)
        , sessionCount = 0
        , sessionTimerHeap = H.empty
        , sessionKeyValueMap = IsMap.mapEmpty :: f s
        , sessionOutputStream = IsStream.nil
        }

    -- We can eject sessions based on the current session count to limit
    -- memory consumption. There are two possible strategies:
    --
    -- 1) Eject old sessions or sessions beyond a certain/lower timeout
    -- threshold even before timeout, effectively reduce the timeout.
    -- 2) Drop creation of new sessions but keep accepting new events for the
    -- old ones.
    --
    -- We use the first strategy as of now.

    -- Got a new stream input element
    sstep session@SessionState{..} (Just (timestamp, (key, value))) = do
        -- XXX instead of a heap we could use a timer wheel.
        --
        -- XXX if the key is an Int, we can also use an IntMap for slightly
        -- better performance.
        --
        -- How it works:
        --
        -- Values for each key are collected in a map using the supplied fold.
        -- When we insert a key in the Map we insert an entry into the heap as
        -- well with the session expiry as the sort key.  The Map entry
        -- consists of the fold result, and the expiry time of the session. If
        -- "reset" is True the expiry time is readjusted whenever a new event
        -- is processed. If the fold terminates and a new session is started
        -- for the same key the expiry time is set to the first timestamp of
        -- the new session.
        --
        -- The heap must have at most one entry for any given key. The heap is
        -- processed periodically to remove the expired entries.  We pick up an
        -- expired entry from the top of the heap and if the session has
        -- expired based on the expiry time in the Map entry then we remove the
        -- session from the Map and yield its fold output. Otherwise, we
        -- reinsert the entry into the heap based on the current expiry in the
        -- Map entry.
        --
        -- If an entry is removed from the Map and not removed from the heap
        -- and in the meantime it is inserted again in the Map (using the same
        -- key) then how do we avoid inserting multiple entries in the heap?
        -- For this reason we maintain the invariant that the Map entry is
        -- removed only when the heap entry is removed. Even if the fold has
        -- finished we still keep a dummy Map entry (ZombieSession) until the
        -- heap entry is removed. That way if we have a Map entry we do not
        -- insert a heap entry because we know it is already there.
        -- XXX The ZombieSession mechanism does not work as expected as we
        -- ignore ZombieSession when inserting a new entry. Anyway, we can
        -- remove this mechanism as at most only two heap entries may be
        -- created and they will be ultimately cleaned up.
        --
        -- Heap processing needs the map and map processing needs the heap,
        -- therefore we cannot separate the two for modularity unless we have a
        -- way to achieve mutual recursion.
        --
        let curTime = max sessionEventTime timestamp
            mOld = IsMap.mapLookup key sessionKeyValueMap
        let done fb = do
                -- deleting a key from the heap is expensive, so we never
                -- delete a key from heap, we just purge it from the Map and it
                -- gets purged from the heap on timeout. We just need an extra
                -- lookup in the Map when the key is purged from the heap, that
                -- should not be expensive.
                --
                let (mp, cnt) = case mOld of
                        Just (LiveSession _ _) ->
                            ( IsMap.mapInsert
                                key ZombieSession sessionKeyValueMap
                            , sessionCount - 1
                            )
                        _ -> (sessionKeyValueMap, sessionCount)
                return $ session
                    { sessionCurTime = curTime
                    , sessionEventTime = curTime
                    , sessionCount = cnt
                    , sessionKeyValueMap = mp
                    , sessionOutputStream = fromPure (key, fb)
                    }
            partial fs1 = do
                let expiry = addToAbsTime timestamp timeoutMs
                (hp1, mp1, out1, cnt1) <- do
                        let vars = (sessionTimerHeap, sessionKeyValueMap,
                                           IsStream.nil, sessionCount)
                        case mOld of
                            -- inserting new entry
                            Nothing -> do
                                -- Eject a session from heap and map if needed
                                eject <- ejectPred sessionCount
                                (hp, mp, out, cnt) <-
                                    if eject
                                    then ejectOne reset extract vars
                                    else return vars

                                -- Insert the new session in heap
                                let hp' = H.insert (Entry expiry key) hp
                                 in return (hp', mp, out, cnt + 1)
                            -- updating old entry
                            Just _ -> return vars

                let acc = LiveSession expiry fs1
                    mp2 = IsMap.mapInsert key acc mp1
                return $ SessionState
                    { sessionCurTime = curTime
                    , sessionEventTime = curTime
                    , sessionCount = cnt1
                    , sessionTimerHeap = hp1
                    , sessionKeyValueMap = mp2
                    , sessionOutputStream = out1
                    }
        res0 <- do
            case mOld of
                Just (LiveSession _ acc) -> return $ FL.Partial acc
                _ -> initial
        case res0 of
            FL.Done _ ->
                error $ "classifySessionsBy: "
                    ++ "The supplied fold must consume at least one input"
            FL.Partial fs -> do
                res <- step fs value
                case res of
                    FL.Done fb -> done fb
                    FL.Partial fs1 -> partial fs1

    -- Got a timer tick event
    sstep sessionState@SessionState{..} Nothing =
        let curTime = addToAbsTime sessionCurTime tickMs
        in ejectExpired reset ejectPred extract sessionState curTime

-- | @classifySessionsBy tick keepalive predicate timeout fold stream@
-- classifies an input event @stream@ consisting of  @(timestamp, (key,
-- value))@ into sessions based on the @key@, folding all the values
-- corresponding to the same key into a session using the supplied @fold@.
--
-- When the fold terminates or a @timeout@ occurs, a tuple consisting of the
-- session key and the folded value is emitted in the output stream. The
-- timeout is measured from the first event in the session.  If the @keepalive@
-- option is set to 'True' the timeout is reset to 0 whenever an event is
-- received.
--
-- The @timestamp@ in the input stream is an absolute time from some epoch,
-- characterizing the time when the input event was generated.  The notion of
-- current time is maintained by a monotonic event time clock using the
-- timestamps seen in the input stream. The latest timestamp seen till now is
-- used as the base for the current time.  When no new events are seen, a timer
-- is started with a clock resolution of @tick@ seconds. This timer is used to
-- detect session timeouts in the absence of new events.
--
-- To ensure an upper bound on the memory used the number of sessions can be
-- limited to an upper bound. If the ejection @predicate@ returns 'True', the
-- oldest session is ejected before inserting a new session.
--
-- When the stream ends any buffered sessions are ejected immediately.
--
-- If a session key is received even after a session has finished, another
-- session is created for that key.
--
-- >>> :{
-- Stream.mapM_ print
--     $ Stream.classifySessionsBy 1 False (const (return False)) 3 (Fold.take 3 Fold.toList)
--     $ Stream.timestamped
--     $ Stream.delay 0.1
--     $ Stream.fromList ((,) <$> [1,2,3] <*> ['a','b','c'])
-- :}
-- (1,"abc")
-- (2,"abc")
-- (3,"abc")
--
-- /Pre-release/
{-# INLINE classifySessionsBy #-}
classifySessionsBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Bool           -- ^ reset the timer when an event is received
    -> (Int -> m Bool) -- ^ predicate to eject sessions based on session count
    -> Double         -- ^ session timeout in seconds
    -> Fold m a b  -- ^ Fold to be applied to session data
    -> t m (AbsTime, (k, a)) -- ^ timestamp, (session key, session data)
    -> t m (k, b) -- ^ session key, fold result
classifySessionsBy = classifySessionsByGeneric (Proxy :: Proxy (Map k))

-- | Same as 'classifySessionsBy' with a timer tick of 1 second and keepalive
-- option set to 'True'.
--
-- @
-- classifyKeepAliveSessions = classifySessionsBy 1 True
-- @
--
-- /Pre-release/
--
{-# INLINE classifyKeepAliveSessions #-}
classifyKeepAliveSessions ::
       (IsStream t, MonadAsync m, Ord k)
    => (Int -> m Bool) -- ^ predicate to eject sessions on session count
    -> Double -- ^ session inactive timeout
    -> Fold m a b -- ^ Fold to be applied to session payload data
    -> t m (AbsTime, (k, a)) -- ^ timestamp, (session key, session data)
    -> t m (k, b)
classifyKeepAliveSessions = classifySessionsBy 1 True

------------------------------------------------------------------------------
-- Keyed tumbling windows
------------------------------------------------------------------------------

-- Tumbling windows is a special case of sliding windows where the window slide
-- is the same as the window size. Or it can be a special case of session
-- windows where the reset flag is set to False.

-- XXX instead of using the early termination flag in the stream, we can use an
-- early terminating fold instead.

{-
-- | Split the stream into fixed size chunks of specified size. Within each
-- such chunk fold the elements in buckets identified by the keys. A particular
-- bucket fold can be terminated early if a closing flag is encountered in an
-- element for that key.
--
-- @since 0.7.0
{-# INLINABLE classifyChunksOf #-}
classifyChunksOf
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifyChunksOf wsize = classifyChunksBy wsize False
-}

-- | Same as 'classifySessionsBy' with a timer tick of 1 second and keepalive
-- option set to 'False'.
--
-- @
-- classifySessionsOf = classifySessionsBy 1 False
-- @
--
-- /Pre-release/
--
{-# INLINE classifySessionsOf #-}
classifySessionsOf ::
       (IsStream t, MonadAsync m, Ord k)
    => (Int -> m Bool) -- ^ predicate to eject sessions on session count
    -> Double -- ^ time window size
    -> Fold m a b -- ^ Fold to be applied to session data
    -> t m (AbsTime, (k, a)) -- ^ timestamp, (session key, session data)
    -> t m (k, b)
classifySessionsOf = classifySessionsBy 1 False

------------------------------------------------------------------------------
-- Nested Split
------------------------------------------------------------------------------

-- | @splitInnerBy splitter joiner stream@ splits the inner containers @f a@ of
-- an input stream @t m (f a)@ using the @splitter@ function. Container
-- elements @f a@ are collected until a split occurs, then all the elements
-- before the split are joined using the @joiner@ function.
--
-- For example, if we have a stream of @Array Word8@, we may want to split the
-- stream into arrays representing lines separated by '\n' byte such that the
-- resulting stream after a split would be one array for each line.
--
-- CAUTION! This is not a true streaming function as the container size after
-- the split and merge may not be bounded.
--
-- /Pre-release/
{-# INLINE splitInnerBy #-}
splitInnerBy
    :: (IsStream t, Monad m)
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> t m (f a)
    -> t m (f a)
splitInnerBy splitter joiner xs =
    fromStreamD $ D.splitInnerBy splitter joiner $ toStreamD xs

-- | Like 'splitInnerBy' but splits assuming the separator joins the segment in
-- a suffix style.
--
-- /Pre-release/
{-# INLINE splitInnerBySuffix #-}
splitInnerBySuffix
    :: (IsStream t, Monad m, Eq (f a), Monoid (f a))
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> t m (f a)
    -> t m (f a)
splitInnerBySuffix splitter joiner xs =
    fromStreamD $ D.splitInnerBySuffix (== mempty) splitter joiner $ toStreamD xs
