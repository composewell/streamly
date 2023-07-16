-- |
-- Module      : Streamly.Internal.Data.Stream.Expand
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Expand a stream by combining two or more streams or by combining streams
-- with unfolds.

module Streamly.Internal.Data.Stream.Expand
    (
    -- * Binary Combinators (Linear)
    -- | Functions ending in the shape:
    --
    -- @Stream m a -> Stream m a -> Stream m a@.
    --
    -- The functions in this section have a linear or flat n-ary combining
    -- characterstics. It means that when combined @n@ times (e.g. @a `serial`
    -- b `serial` c ...@) the resulting expression will have an @O(n)@
    -- complexity (instead O(n^2) for pair wise combinators described in the
    -- next section. These functions can be used efficiently with
    -- 'concatMapWith' et. al.  combinators that combine streams in a linear
    -- fashion (contrast with 'mergeMapWith' which combines streams as a
    -- binary tree).

      append
    -- * Binary Combinators (Pair Wise)
    -- | Like the functions in the section above these functions also combine
    -- two streams into a single stream but when used @n@ times linearly they
    -- exhibit O(n^2) complexity. They are best combined in a binary tree
    -- fashion using 'mergeMapWith' giving a @n * log n@ complexity.  Avoid
    -- using these with 'concatMapWith' when combining a large or infinite
    -- number of streams.

    -- ** Append
    , append2

    -- ** Interleave
    , interleave
    , interleave2
    , interleaveFst
    , interleaveFst2
    , interleaveFstSuffix2
    , interleaveMin
    , interleaveMin2

    -- ** Round Robin
    , roundrobin

    -- ** Merge
    , mergeBy
    , mergeByM
    , mergeByM2
    , mergeMinBy
    , mergeFstBy

    -- ** Zip
    , zipWith
    , zipWithM

    -- * Combine Streams and Unfolds
    -- |
    -- Expand a stream by repeatedly using an unfold and merging the resulting
    -- streams.  Functions generally ending in the shape:
    --
    -- @Unfold m a b -> Stream m a -> Stream m b@

    -- ** Unfold and combine streams
    -- | Unfold and flatten streams.
    , unfoldMany -- XXX Rename to unfoldAppend
    , unfoldInterleave
    , unfoldRoundRobin

    -- ** Interpose
    -- | Insert effects between streams. Like unfoldMany but intersperses an
    -- effect between the streams. A special case of gintercalate.
    , interpose
    , interposeSuffix
    -- , interposeBy

    -- ** Intercalate
    -- | Insert Streams between Streams.
    -- Like unfoldMany but intersperses streams from another source between
    -- the streams from the first source.
    , intercalate
    , intercalateSuffix
    , gintercalate
    , gintercalateSuffix

    -- * Combine Streams of Streams
    -- | Map and serially append streams. 'concatMapM' is a generalization of
    -- the binary append operation to append many streams.
    , concatMapM
    , concatMap
    , concatEffect
    , concat

    -- * ConcatMapWith
    -- | Map and flatten a stream like 'concatMap' but using a custom binary
    -- stream merging combinator instead of just appending the streams.  The
    -- merging occurs sequentially, it works efficiently for 'serial', 'async',
    -- 'ahead' like merge operations where we consume one stream before the
    -- next or in case of 'wAsync' or 'parallel' where we consume all streams
    -- simultaneously anyway.
    --
    -- However, in cases where the merging consumes streams in a round robin
    -- fashion, a pair wise merging using 'mergeMapWith' would be more
    -- efficient. These cases include operations like 'mergeBy' or 'zipWith'.

    , concatMapWith
    , bindWith
    , concatSmapMWith

    -- * MergeMapWith
    -- | See the notes about suitable merge functions in the 'concatMapWith'
    -- section.
    , mergeMapWith

    -- * Iterate
    -- | Map and flatten Trees of Streams
    , unfoldIterateDfs
    , unfoldIterateBfs
    , unfoldIterateBfsRev

    , concatIterateWith
    , mergeIterateWith

    , concatIterateDfs
    , concatIterateBfs

    -- More experimental ops
    , concatIterateBfsRev
    , concatIterateLeftsWith
    , concatIterateScanWith
    , concatIterateScan
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.Bottom
    ( concatEffect, concatMapM, concatMap, smapM, zipWith, zipWithM)
import Streamly.Internal.Data.Stream.Type
    ( Stream, fromStreamD, fromStreamK, toStreamD, toStreamK
    , bindWith, concatMapWith, cons, nil)
import Streamly.Internal.Data.Unfold.Type (Unfold)

import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K (mergeBy, mergeByM)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Prelude hiding (concat, concatMap, zipWith)

-- $setup
-- >>> :m
-- >>> import Data.Either (either)
-- >>> import Data.IORef
-- >>> import Streamly.Internal.Data.Stream (Stream)
-- >>> import Prelude hiding (zipWith, concatMap, concat)
-- >>> import qualified Streamly.Data.Array as Array
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Internal.FileSystem.Dir as Dir
--

------------------------------------------------------------------------------
-- Appending
------------------------------------------------------------------------------

infixr 6 `append2`

-- | This is fused version of 'append'. It could be up to 100x faster than
-- 'append' when combining two fusible streams. However, it slows down
-- quadratically with the number of streams being appended. Therefore, it is
-- suitable for ad-hoc append of a few streams, and should not be used with
-- 'concatMapWith' or 'mergeMapWith'.
--
-- /Fused/
--
{-# INLINE append2 #-}
append2 ::Monad m => Stream m b -> Stream m b -> Stream m b
append2 m1 m2 = fromStreamD $ D.append (toStreamD m1) (toStreamD m2)

infixr 6 `append`

-- | Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- >>> s1 = Stream.fromList [1,2]
-- >>> s2 = Stream.fromList [3,4]
-- >>> Stream.fold Fold.toList $ s1 `Stream.append` s2
-- [1,2,3,4]
--
-- This has O(n) append performance where @n@ is the number of streams. It can
-- be used to efficiently fold an infinite lazy container of streams
-- 'concatMapWith' et. al.
--
-- See 'append2' for a fusible alternative.
--
-- /CPS/
{-# INLINE append #-}
append :: Stream m a -> Stream m a -> Stream m a
append = (<>)

------------------------------------------------------------------------------
-- Interleaving
------------------------------------------------------------------------------

infixr 6 `interleave`

-- | Interleaves two streams, yielding one element from each stream
-- alternately.  When one stream stops the rest of the other stream is used in
-- the output stream.
--
-- When joining many streams in a left associative manner earlier streams will
-- get exponential priority than the ones joining later. Because of exponential
-- weighting it can be used with 'concatMapWith' even on a large number of
-- streams.
--
-- See 'interleave2' for a fusible alternative.
--
-- /CPS/
{-# INLINE interleave #-}
interleave :: Stream m a -> Stream m a -> Stream m a
interleave s1 s2 = fromStreamK $ K.interleave (toStreamK s1) (toStreamK s2)

{-# INLINE interleave2 #-}
interleave2 :: Monad m => Stream m a -> Stream m a -> Stream m a
interleave2 s1 s2 = fromStreamD $ D.interleave (toStreamD s1) (toStreamD s2)

-- | Like `interleave` but stops interleaving as soon as the first stream
-- stops.
--
-- See 'interleaveFst2' for a fusible alternative.
--
-- /CPS/
{-# INLINE interleaveFst #-}
interleaveFst :: Stream m a -> Stream m a -> Stream m a
interleaveFst s1 s2 =
    fromStreamK $ K.interleaveFst (toStreamK s1) (toStreamK s2)

-- | Like `interleave` but stops interleaving as soon as any of the two streams
-- stops.
--
-- See 'interleaveMin2' for a fusible alternative.
--
-- /CPS/
{-# INLINE interleaveMin #-}
interleaveMin :: Stream m a -> Stream m a -> Stream m a
interleaveMin s1 s2 =
    fromStreamK $ K.interleaveMin (toStreamK s1) (toStreamK s2)

{-# INLINE interleaveMin2 #-}
interleaveMin2 :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveMin2 s1 s2 =
    fromStreamD $ D.interleaveMin (toStreamD s1) (toStreamD s2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. As soon as the first stream
-- finishes, the output stops, discarding the remaining part of the second
-- stream. In this case, the last element in the resulting stream would be from
-- the second stream. If the second stream finishes early then the first stream
-- still continues to yield elements until it finishes.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveFstSuffix2 "abc" ",,,," :: Stream Identity Char
-- fromList "a,b,c,"
-- >>> Stream.interleaveFstSuffix2 "abc" "," :: Stream Identity Char
-- fromList "a,bc"
--
-- 'interleaveFstSuffix2' is a dual of 'interleaveFst2'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveFstSuffix2 #-}
interleaveFstSuffix2 :: Monad m => Stream m b -> Stream m b -> Stream m b
interleaveFstSuffix2 m1 m2 =
    fromStreamD $ D.interleaveFstSuffix (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream and ending at the first stream.
-- If the second stream is longer than the first, elements from the second
-- stream are infixed with elements from the first stream. If the first stream
-- is longer then it continues yielding elements even after the second stream
-- has finished.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveFst2 "abc" ",,,," :: Stream Identity Char
-- fromList "a,b,c"
-- >>> Stream.interleaveFst2 "abc" "," :: Stream Identity Char
-- fromList "a,bc"
--
-- 'interleaveFst2' is a dual of 'interleaveFstSuffix2'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveFst2 #-}
interleaveFst2 :: Monad m => Stream m b -> Stream m b -> Stream m b
interleaveFst2 m1 m2 =
    fromStreamD $ D.interleaveFst (toStreamD m1) (toStreamD m2)

------------------------------------------------------------------------------
-- Scheduling
------------------------------------------------------------------------------

-- | Schedule the execution of two streams in a fair round-robin manner,
-- executing each stream once, alternately. Execution of a stream may not
-- necessarily result in an output, a stream may chose to @Skip@ producing an
-- element until later giving the other stream a chance to run. Therefore, this
-- combinator fairly interleaves the execution of two streams rather than
-- fairly interleaving the output of the two streams. This can be useful in
-- co-operative multitasking without using explicit threads. This can be used
-- as an alternative to `async`.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE roundrobin #-}
roundrobin :: Monad m => Stream m b -> Stream m b -> Stream m b
roundrobin m1 m2 = fromStreamD $ D.roundRobin (toStreamD m1) (toStreamD m2)

------------------------------------------------------------------------------
-- Merging (sorted streams)
------------------------------------------------------------------------------

-- | Merge two streams using a comparison function. The head elements of both
-- the streams are compared and the smaller of the two elements is emitted, if
-- both elements are equal then the element from the first stream is used
-- first.
--
-- If the streams are sorted in ascending order, the resulting stream would
-- also remain sorted in ascending order.
--
-- >>> s1 = Stream.fromList [1,3,5]
-- >>> s2 = Stream.fromList [2,4,6,8]
-- >>> Stream.fold Fold.toList $ Stream.mergeBy compare s1 s2
-- [1,2,3,4,5,6,8]
--
-- See 'mergeByM2' for a fusible alternative.
--
-- /CPS/
{-# INLINE mergeBy #-}
mergeBy :: (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeBy f m1 m2 = fromStreamK $ K.mergeBy f (toStreamK m1) (toStreamK m2)

-- | Like 'mergeBy' but with a monadic comparison function.
--
-- Merge two streams randomly:
--
-- @
-- > randomly _ _ = randomIO >>= \x -> return $ if x then LT else GT
-- > Stream.toList $ Stream.mergeByM randomly (Stream.fromList [1,1,1,1]) (Stream.fromList [2,2,2,2])
-- [2,1,2,2,2,1,1,1]
-- @
--
-- Merge two streams in a proportion of 2:1:
--
-- >>> :{
-- do
--  let s1 = Stream.fromList [1,1,1,1,1,1]
--      s2 = Stream.fromList [2,2,2]
--  let proportionately m n = do
--       ref <- newIORef $ cycle $ Prelude.concat [Prelude.replicate m LT, Prelude.replicate n GT]
--       return $ \_ _ -> do
--          r <- readIORef ref
--          writeIORef ref $ Prelude.tail r
--          return $ Prelude.head r
--  f <- proportionately 2 1
--  xs <- Stream.fold Fold.toList $ Stream.mergeByM f s1 s2
--  print xs
-- :}
-- [1,1,2,1,1,2,1,1,2]
--
-- See 'mergeByM2' for a fusible alternative.
--
-- /CPS/
{-# INLINE mergeByM #-}
mergeByM
    :: Monad m
    => (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByM f m1 m2 = fromStreamK $ K.mergeByM f (toStreamK m1) (toStreamK m2)

-- | Like 'mergeByM' but much faster, works best when merging statically known
-- number of streams. When merging more than two streams try to merge pairs and
-- pair of pairs in a tree like structure.'mergeByM' works better with variable
-- number of streams being merged using 'mergeMapWith'.
--
-- /Internal/
{-# INLINE mergeByM2 #-}
mergeByM2
    :: Monad m
    => (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByM2 f m1 m2 =
    fromStreamD $ D.mergeByM f (toStreamD m1) (toStreamD m2)

-- | Like 'mergeByM' but stops merging as soon as any of the two streams stops.
--
-- /Unimplemented/
{-# INLINABLE mergeMinBy #-}
mergeMinBy :: -- Monad m =>
    (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeMinBy _f _m1 _m2 = undefined
    -- fromStreamD $ D.mergeMinBy f (toStreamD m1) (toStreamD m2)

-- | Like 'mergeByM' but stops merging as soon as the first stream stops.
--
-- /Unimplemented/
{-# INLINABLE mergeFstBy #-}
mergeFstBy :: -- Monad m =>
    (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeFstBy _f _m1 _m2 = undefined
    -- fromStreamK $ D.mergeFstBy f (toStreamD m1) (toStreamD m2)

------------------------------------------------------------------------------
-- Combine N Streams - unfoldMany
------------------------------------------------------------------------------

-- | Like 'concatMap' but uses an 'Unfold' for stream generation. Unlike
-- 'concatMap' this can fuse the 'Unfold' code with the inner loop and
-- therefore provide many times better performance.
--
{-# INLINE unfoldMany #-}
unfoldMany ::Monad m => Unfold m a b -> Stream m a -> Stream m b
unfoldMany u m = fromStreamD $ D.unfoldMany u (toStreamD m)

-- | This does not pair streams like mergeMapWith, instead, it goes through
-- each stream one by one and yields one element from each stream. After it
-- goes to the last stream it reverses the traversal to come back to the first
-- stream yielding elements from each stream on its way back to the first
-- stream and so on.
--
-- >>> lists = Stream.fromList [[1,1],[2,2],[3,3],[4,4],[5,5]]
-- >>> interleaved = Stream.unfoldInterleave Unfold.fromList lists
-- >>> Stream.fold Fold.toList interleaved
-- [1,2,3,4,5,5,4,3,2,1]
--
-- Note that this is order of magnitude more efficient than "mergeMapWith
-- wSerial" because of fusion.
--
-- /Fused/
{-# INLINE unfoldInterleave #-}
unfoldInterleave ::Monad m => Unfold m a b -> Stream m a -> Stream m b
unfoldInterleave u m =
    fromStreamD $ D.unfoldInterleave u (toStreamD m)

-- | 'unfoldInterleave' switches to the next stream whenever a value from a
-- stream is yielded, it does not switch on a 'Skip'. So if a stream keeps
-- skipping for long time other streams won't get a chance to run.
-- 'unfoldRoundRobin' switches on Skip as well. So it basically schedules each
-- stream fairly irrespective of whether it produces a value or not.
--
{-# INLINE unfoldRoundRobin #-}
unfoldRoundRobin ::Monad m => Unfold m a b -> Stream m a -> Stream m b
unfoldRoundRobin u m =
    fromStreamD $ D.unfoldRoundRobin u (toStreamD m)

------------------------------------------------------------------------------
-- Combine N Streams - interpose
------------------------------------------------------------------------------

-- > interpose x unf str = gintercalate unf str UF.identity (repeat x)

-- | Unfold the elements of a stream, intersperse the given element between the
-- unfolded streams and then concat them into a single stream.
--
-- >>> unwords = Stream.interpose ' '
--
-- /Pre-release/
{-# INLINE interpose #-}
interpose :: Monad m
    => c -> Unfold m b c -> Stream m b -> Stream m c
interpose x unf str =
    fromStreamD $ D.interpose x unf (toStreamD str)

-- interposeSuffix x unf str = gintercalateSuffix unf str UF.identity (repeat x)

-- | Unfold the elements of a stream, append the given element after each
-- unfolded stream and then concat them into a single stream.
--
-- >>> unlines = Stream.interposeSuffix '\n'
--
-- /Pre-release/
{-# INLINE interposeSuffix #-}
interposeSuffix :: Monad m
    => c -> Unfold m b c -> Stream m b -> Stream m c
interposeSuffix x unf str =
    fromStreamD $ D.interposeSuffix x unf (toStreamD str)

------------------------------------------------------------------------------
-- Combine N Streams - intercalate
------------------------------------------------------------------------------

-- XXX we can swap the order of arguments to gintercalate so that the
-- definition of unfoldMany becomes simpler? The first stream should be
-- infixed inside the second one. However, if we change the order in
-- "interleave" as well similarly, then that will make it a bit unintuitive.
--
-- > unfoldMany unf str =
-- >     gintercalate unf str (UF.nilM (\_ -> return ())) (repeat ())

-- | 'interleaveFst' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalate #-}
gintercalate
    :: Monad m
    => Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
gintercalate unf1 str1 unf2 str2 =
    fromStreamD $ D.gintercalate
        unf1 (toStreamD str1)
        unf2 (toStreamD str2)

-- > intercalate unf seed str = gintercalate unf str unf (repeatM seed)

-- | 'intersperse' followed by unfold and concat.
--
-- >>> intercalate u a = Stream.unfoldMany u . Stream.intersperse a
-- >>> intersperse = Stream.intercalate Unfold.identity
-- >>> unwords = Stream.intercalate Unfold.fromList " "
--
-- >>> input = Stream.fromList ["abc", "def", "ghi"]
-- >>> Stream.fold Fold.toList $ Stream.intercalate Unfold.fromList " " input
-- "abc def ghi"
--
{-# INLINE intercalate #-}
intercalate :: Monad m
    => Unfold m b c -> b -> Stream m b -> Stream m c
intercalate unf seed str = fromStreamD $
    D.unfoldMany unf $ D.intersperse seed (toStreamD str)

-- | 'interleaveFstSuffix2' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalateSuffix #-}
gintercalateSuffix
    :: Monad m
    => Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
gintercalateSuffix unf1 str1 unf2 str2 =
    fromStreamD $ D.gintercalateSuffix
        unf1 (toStreamD str1)
        unf2 (toStreamD str2)

-- > intercalateSuffix unf seed str = gintercalateSuffix unf str unf (repeatM seed)

-- | 'intersperseMSuffix' followed by unfold and concat.
--
-- >>> intercalateSuffix u a = Stream.unfoldMany u . Stream.intersperseMSuffix a
-- >>> intersperseMSuffix = Stream.intercalateSuffix Unfold.identity
-- >>> unlines = Stream.intercalateSuffix Unfold.fromList "\n"
--
-- >>> input = Stream.fromList ["abc", "def", "ghi"]
-- >>> Stream.fold Fold.toList $ Stream.intercalateSuffix Unfold.fromList "\n" input
-- "abc\ndef\nghi\n"
--
{-# INLINE intercalateSuffix #-}
intercalateSuffix :: Monad m
    => Unfold m b c -> b -> Stream m b -> Stream m c
intercalateSuffix unf seed =
    fromStreamD . D.intercalateSuffix unf seed . toStreamD

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | Flatten a stream of streams to a single stream.
--
-- >>> concat = Stream.concatMap id
--
-- /Pre-release/
{-# INLINE concat #-}
concat :: Monad m => Stream m (Stream m a) -> Stream m a
concat = concatMap id

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | Like 'concatMapWith' but carries a state which can be used to share
-- information across multiple steps of concat.
--
-- >>> concatSmapMWith combine f initial = Stream.concatMapWith combine id . Stream.smapM f initial
--
-- /Pre-release/
--
{-# INLINE concatSmapMWith #-}
concatSmapMWith
    :: Monad m
    => (Stream m b -> Stream m b -> Stream m b)
    -> (s -> a -> m (s, Stream m b))
    -> m s
    -> Stream m a
    -> Stream m b
concatSmapMWith combine f initial =
    concatMapWith combine id . smapM f initial

-- XXX Implement a StreamD version for fusion.

-- | Combine streams in pairs using a binary combinator, the resulting streams
-- are then combined again in pairs recursively until we get to a single
-- combined stream. The composition would thus form a binary tree.
--
-- For example, you can sort a stream using merge sort like this:
--
-- >>> s = Stream.fromList [5,1,7,9,2]
-- >>> generate = Stream.fromPure
-- >>> combine = Stream.mergeBy compare
-- >>> Stream.fold Fold.toList $ Stream.mergeMapWith combine generate s
-- [1,2,5,7,9]
--
-- Note that if the stream length is not a power of 2, the binary tree composed
-- by mergeMapWith would not be balanced, which may or may not be important
-- depending on what you are trying to achieve.
--
-- /Caution: the stream of streams must be finite/
--
-- /CPS/
--
-- /Pre-release/
--
{-# INLINE mergeMapWith #-}
mergeMapWith ::
       (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> Stream m a
    -> Stream m b
mergeMapWith par f m =
    fromStreamK
        $ K.mergeMapWith
            (\s1 s2 -> toStreamK $ fromStreamK s1 `par` fromStreamK s2)
            (toStreamK . f)
            (toStreamK m)

------------------------------------------------------------------------------
-- concatIterate - Map and flatten Trees of Streams
------------------------------------------------------------------------------

-- | Yield an input element in the output stream, map a stream generator on it
-- and repeat the process on the resulting stream. Resulting streams are
-- flattened using the 'concatMapWith' combinator. This can be used for a depth
-- first style (DFS) traversal of a tree like structure.
--
-- Example, list a directory tree using DFS:
--
-- >>> f = either Dir.readEitherPaths (const Stream.nil)
-- >>> input = Stream.fromPure (Left ".")
-- >>> ls = Stream.concatIterateWith Stream.append f input
--
-- Note that 'iterateM' is a special case of 'concatIterateWith':
--
-- >>> iterateM f = Stream.concatIterateWith Stream.append (Stream.fromEffect . f) . Stream.fromEffect
--
-- /CPS/
--
-- /Pre-release/
--
{-# INLINE concatIterateWith #-}
concatIterateWith ::
       (Stream m a -> Stream m a -> Stream m a)
    -> (a -> Stream m a)
    -> Stream m a
    -> Stream m a
concatIterateWith combine f = iterateStream

    where

    iterateStream = concatMapWith combine generate

    generate x = x `cons` iterateStream (f x)

-- | Traverse the stream in depth first style (DFS). Map each element in the
-- input stream to a stream and flatten, recursively map the resulting elements
-- as well to a stream and flatten until no more streams are generated.
--
-- Example, list a directory tree using DFS:
--
-- >>> f = either (Just . Dir.readEitherPaths) (const Nothing)
-- >>> input = Stream.fromPure (Left ".")
-- >>> ls = Stream.concatIterateDfs f input
--
-- This is equivalent to using @concatIterateWith Stream.append@.
--
-- /Pre-release/
{-# INLINE concatIterateDfs #-}
concatIterateDfs :: Monad m =>
       (a -> Maybe (Stream m a))
    -> Stream m a
    -> Stream m a
concatIterateDfs f stream =
    fromStreamD
        $ D.concatIterateDfs (fmap toStreamD . f ) (toStreamD stream)

-- | Similar to 'concatIterateDfs' except that it traverses the stream in
-- breadth first style (BFS). First, all the elements in the input stream are
-- emitted, and then their traversals are emitted.
--
-- Example, list a directory tree using BFS:
--
-- >>> f = either (Just . Dir.readEitherPaths) (const Nothing)
-- >>> input = Stream.fromPure (Left ".")
-- >>> ls = Stream.concatIterateBfs f input
--
-- /Pre-release/
{-# INLINE concatIterateBfs #-}
concatIterateBfs :: Monad m =>
       (a -> Maybe (Stream m a))
    -> Stream m a
    -> Stream m a
concatIterateBfs f stream =
    fromStreamD
        $ D.concatIterateBfs (fmap toStreamD . f ) (toStreamD stream)

-- | Same as 'concatIterateBfs' except that the traversal of the last
-- element on a level is emitted first and then going backwards up to the first
-- element (reversed ordering). This may be slightly faster than
-- 'concatIterateBfs'.
--
{-# INLINE concatIterateBfsRev #-}
concatIterateBfsRev :: Monad m =>
       (a -> Maybe (Stream m a))
    -> Stream m a
    -> Stream m a
concatIterateBfsRev f stream =
    fromStreamD
        $ D.concatIterateBfsRev (fmap toStreamD . f ) (toStreamD stream)

-- | Like 'concatIterateWith' but uses the pairwise flattening combinator
-- 'mergeMapWith' for flattening the resulting streams. This can be used for a
-- balanced traversal of a tree like structure.
--
-- Example, list a directory tree using balanced traversal:
--
-- >>> f = either Dir.readEitherPaths (const Stream.nil)
-- >>> input = Stream.fromPure (Left ".")
-- >>> ls = Stream.mergeIterateWith Stream.interleave f input
--
-- /CPS/
--
-- /Pre-release/
--
{-# INLINE mergeIterateWith #-}
mergeIterateWith ::
       (Stream m a -> Stream m a -> Stream m a)
    -> (a -> Stream m a)
    -> Stream m a
    -> Stream m a
mergeIterateWith combine f = iterateStream

    where

    iterateStream = mergeMapWith combine generate

    generate x = x `cons` iterateStream (f x)

-- | Same as @concatIterateDfs@ but more efficient due to stream fusion.
--
-- Example, list a directory tree using DFS:
--
-- >>> f = Unfold.liftEither Dir.eitherReaderPaths Unfold.nil
-- >>> input = Stream.fromPure (Left ".")
-- >>> ls = Stream.unfoldIterateDfs f input
--
-- /Pre-release/
{-# INLINE unfoldIterateDfs #-}
unfoldIterateDfs :: Monad m => Unfold m a a -> Stream m a -> Stream m a
unfoldIterateDfs u = fromStreamD . D.unfoldIterateDfs u . toStreamD

-- | Like 'unfoldIterateDfs' but uses breadth first style traversal.
--
-- /Pre-release/
{-# INLINE unfoldIterateBfs #-}
unfoldIterateBfs :: Monad m => Unfold m a a -> Stream m a -> Stream m a
unfoldIterateBfs u = fromStreamD . D.unfoldIterateBfs u . toStreamD

-- | Like 'unfoldIterateBfs' but processes the children in reverse order,
-- therefore, may be slightly faster.
--
-- /Pre-release/
{-# INLINE unfoldIterateBfsRev #-}
unfoldIterateBfsRev :: Monad m => Unfold m a a -> Stream m a -> Stream m a
unfoldIterateBfsRev u =
    fromStreamD . D.unfoldIterateBfsRev u . toStreamD

------------------------------------------------------------------------------
-- Flattening Graphs
------------------------------------------------------------------------------

-- To traverse graphs we need a state to be carried around in the traversal.
-- For example, we can use a hashmap to store the visited status of nodes.

-- | Like 'iterateMap' but carries a state in the stream generation function.
-- This can be used to traverse graph like structures, we can remember the
-- visited nodes in the state to avoid cycles.
--
-- Note that a combination of 'iterateMap' and 'usingState' can also be used to
-- traverse graphs. However, this function provides a more localized state
-- instead of using a global state.
--
-- See also: 'mfix'
--
-- /Pre-release/
--
{-# INLINE concatIterateScanWith #-}
concatIterateScanWith
    :: Monad m
    => (Stream m a -> Stream m a -> Stream m a)
    -> (b -> a -> m (b, Stream m a))
    -> m b
    -> Stream m a
    -> Stream m a
concatIterateScanWith combine f initial stream =
    concatEffect $ do
        b <- initial
        iterateStream (b, stream)

    where

    iterateStream (b, s) = pure $ concatMapWith combine (generate b) s

    generate b a = a `cons` feedback b a

    feedback b a = concatEffect $ f b a >>= iterateStream

-- Next stream is to be generated by the return value of the previous stream. A
-- general intuitive way of doing that could be to use an appending monad
-- instance for streams where the result of the previous stream is used to
-- generate the next one. In the first pass we can just emit the values in the
-- stream and keep building a buffered list/stream, once done we can then
-- process the buffered stream.

{-# INLINE concatIterateScan #-}
concatIterateScan
    :: Monad m
    => (b -> a -> m b)
    -> (b -> m (Maybe (b, Stream m a)))
    -> b
    -> Stream m a
concatIterateScan scanner generate initial =
    fromStreamD
        $ D.concatIterateScan
            scanner (fmap (fmap (fmap toStreamD)) . generate) initial

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- Keep concating either streams as long as rights are generated, stop as soon
-- as a left is generated and concat the left stream.
--
-- See also: 'handle'
--
-- /Unimplemented/
--
{-
concatMapEitherWith
    :: (forall x. t m x -> t m x -> t m x)
    -> (a -> t m (Either (Stream m b) b))
    -> Stream m a
    -> Stream m b
concatMapEitherWith = undefined
-}

-- XXX We should prefer using the Maybe stream returning signatures over this.
-- This API should perhaps be removed in favor of those.

-- | In an 'Either' stream iterate on 'Left's.  This is a special case of
-- 'concatIterateWith':
--
-- >>> concatIterateLeftsWith combine f = Stream.concatIterateWith combine (either f (const Stream.nil))
--
-- To traverse a directory tree:
--
-- >>> input = Stream.fromPure (Left ".")
-- >>> ls = Stream.concatIterateLeftsWith Stream.append Dir.readEither input
--
-- /Pre-release/
--
{-# INLINE concatIterateLeftsWith #-}
concatIterateLeftsWith
    :: (b ~ Either a c)
    => (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> Stream m b
    -> Stream m b
concatIterateLeftsWith combine f =
    concatIterateWith combine (either f (const nil))
