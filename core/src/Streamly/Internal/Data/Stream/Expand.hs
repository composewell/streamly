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
    -- * Binary Combinators (Pair Wise)
    -- | Like the functions in the section above these functions also combine
    -- two streams into a single stream but when used @n@ times linearly they
    -- exhibit O(n^2) complexity. They are best combined in a binary tree
    -- fashion using 'concatPairsWith' giving a @n * log n@ complexity.  Avoid
    -- using these with 'concatMapWith' when combining a large or infinite
    -- number of streams.

    -- ** Append
      append

    -- ** wSerial
    -- | 'wSerial' is a CPS based stream interleaving functions. Use
    -- 'concatPairsWith wSerial' to interleave @n@ streams uniformly. It can be
    -- used with 'concatMapWith' as well, however, the interleaving behavior of
    -- @n@ streams would be asymmetric giving exponentially more weightage to
    -- streams that come earlier in the composition.
    --
    , wSerial
    , WSerial.wSerialFst
    , WSerial.wSerialMin

    -- ** Interleave
    -- | 'interleave' is like 'wSerial'  but using a direct style
    -- implementation instead of CPS. It is faster than 'wSerial' due to stream
    -- fusion but has worse efficiency when used with 'concatMapWith' for large
    -- number of streams.
    , interleave
    , interleaveMin
    , interleaveSuffix
    , interleaveInfix

    -- ** Round Robin
    , roundrobin

    -- ** Zip
    , zipWith
    , zipWithM

    -- ** Merge
    , merge
    , mergeBy
    , mergeByM
    , mergeByMFused
    , mergeMinBy
    , mergeFstBy

    -- * Combine Streams and Unfolds
    -- |
    -- Expand a stream by repeatedly using an unfold and merging the resulting
    -- streams.  Functions generally ending in the shape:
    --
    -- @Unfold m a b -> Stream m a -> Stream m b@

    -- ** Append Many (Unfold)
    -- | Unfold and flatten streams.
    , unfoldMany
    , unfoldManyInterleave
    , unfoldManyRoundRobin

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

    -- * Append Many (concatMap)
    -- | Map and serially append streams. 'concatMapM' is a generalization of
    -- the binary append operation to append many streams.
    , concatMapM
    , concatMap
    , concatM
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
    -- fashion, a pair wise merging using 'concatPairsWith' would be more
    -- efficient. These cases include operations like 'mergeBy' or 'zipWith'.

    , concatSmapMWith

    -- * ConcatPairsWith
    -- | See the notes about suitable merge functions in the 'concatMapWith'
    -- section.
    , concatPairsWith

    -- * IterateMap
    -- | Map and flatten Trees of Streams
    , iterateMapWith
    , iterateSmapMWith
    , iterateMapLeftsWith
    , iterateUnfold
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.Bottom
    ( concatM, concatMapM, concatMap, smapM, fromPure, fromEffect
    , zipWith, zipWithM)
import Streamly.Internal.Data.Stream.Type
    ( Stream, append, fromStreamD, fromStreamK, toStreamD, toStreamK
    , concatMapWith, nil)
import Streamly.Internal.Data.Unfold.Type (Unfold)

import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K (mergeBy, mergeByM)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.WSerial as WSerial

import Prelude hiding (concat, concatMap, zipWith)

-- $setup
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import Data.IORef
-- >>> import Prelude hiding (zipWith, concatMap, concat)
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Array.Foreign as Array
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}
--

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

infixr 6 `wSerial`

-- XXX doc duplicated from Stream.Serial module.
--
-- | Interleaves two streams, yielding one element from each stream
-- alternately.  When one stream stops the rest of the other stream is used in
-- the output stream.
--
-- >>> import Streamly.Prelude (wSerial)
-- >>> stream1 = Stream.fromList [1,2]
-- >>> stream2 = Stream.fromList [3,4]
-- >>> Stream.toList $ Stream.fromWSerial $ stream1 `wSerial` stream2
-- [1,3,2,4]
--
-- Note, for singleton streams 'wSerial' and 'serial' are identical.
--
-- Note that this operation cannot be used to fold a container of infinite
-- streams but it can be used for very large streams as the state that it needs
-- to maintain is proportional to the logarithm of the number of streams.
--
-- @since 0.8.0
--
-- /Since: 0.2.0 ("Streamly")/

-- Scheduling Notes:
--
-- Note that evaluation of @a \`wSerial` b \`wSerial` c@ does not interleave
-- @a@, @b@ and @c@ with equal priority.  This expression is equivalent to @a
-- \`wSerial` (b \`wSerial` c)@, therefore, it fairly interleaves @a@ with the
-- result of @b \`wSerial` c@.  For example, @Stream.fromList [1,2] \`wSerial`
-- Stream.fromList [3,4] \`wSerial` Stream.fromList [5,6]@ would result in
-- [1,3,2,5,4,6].  In other words, the leftmost stream gets the same scheduling
-- priority as the rest of the streams taken together. The same is true for
-- each subexpression on the right.
--
{-# INLINE wSerial #-}
wSerial :: Stream m a -> Stream m a -> Stream m a
wSerial m1 m2 = fromStreamK $ WSerial.wSerialK (toStreamK m1) (toStreamK m2)

-- XXX Same as 'wSerial'. We should perhaps rename wSerial to interleave.
-- XXX Document the interleaving behavior of side effects in all the
-- interleaving combinators.
-- XXX Write time-domain equivalents of these. In the time domain we can
-- interleave two streams such that the value of second stream is always taken
-- from its last value even if no new value is being yielded, like
-- zipWithLatest. It would be something like interleaveWithLatest.
--
-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. If any of the streams finishes
-- early the other stream continues alone until it too finishes.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleave "ab" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,,,"
--
-- >>> Stream.interleave "abcd" ",," :: Stream.SerialT Identity Char
-- fromList "a,b,cd"
--
-- 'interleave' is dual to 'interleaveMin', it can be called @interleaveMax@.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleave #-}
interleave ::(Monad m) => Stream m b -> Stream m b -> Stream m b
interleave m1 m2 = fromStreamD $ D.interleave (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. As soon as the first stream
-- finishes, the output stops, discarding the remaining part of the second
-- stream. In this case, the last element in the resulting stream would be from
-- the second stream. If the second stream finishes early then the first stream
-- still continues to yield elements until it finishes.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveSuffix "abc" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,c,"
-- >>> Stream.interleaveSuffix "abc" "," :: Stream.SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveSuffix' is a dual of 'interleaveInfix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveSuffix #-}
interleaveSuffix ::(Monad m) => Stream m b -> Stream m b -> Stream m b
interleaveSuffix m1 m2 =
    fromStreamD $ D.interleaveSuffix (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream and ending at the first stream.
-- If the second stream is longer than the first, elements from the second
-- stream are infixed with elements from the first stream. If the first stream
-- is longer then it continues yielding elements even after the second stream
-- has finished.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveInfix "abc" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,c"
-- >>> Stream.interleaveInfix "abc" "," :: Stream.SerialT Identity Char
-- fromList "a,bc"
--
-- 'interleaveInfix' is a dual of 'interleaveSuffix'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveInfix #-}
interleaveInfix ::(Monad m) => Stream m b -> Stream m b -> Stream m b
interleaveInfix m1 m2 =
    fromStreamD $ D.interleaveInfix (toStreamD m1) (toStreamD m2)

-- | Interleaves the outputs of two streams, yielding elements from each stream
-- alternately, starting from the first stream. The output stops as soon as any
-- of the two streams finishes, discarding the remaining part of the other
-- stream. The last element of the resulting stream would be from the longer
-- stream.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.Functor.Identity (Identity)
-- >>> Stream.interleaveMin "ab" ",,,," :: Stream.SerialT Identity Char
-- fromList "a,b,"
-- >>> Stream.interleaveMin "abcd" ",," :: Stream.SerialT Identity Char
-- fromList "a,b,c"
--
-- 'interleaveMin' is dual to 'interleave'.
--
-- Do not use at scale in concatMapWith.
--
-- /Pre-release/
{-# INLINE interleaveMin #-}
interleaveMin ::(Monad m) => Stream m b -> Stream m b -> Stream m b
interleaveMin m1 m2 = fromStreamD $ D.interleaveMin (toStreamD m1) (toStreamD m2)

-------------------------------------------------------------------------------
-- Scheduling
-------------------------------------------------------------------------------

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
roundrobin ::(Monad m) => Stream m b -> Stream m b -> Stream m b
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
-- @
-- >>> Stream.toList $ Stream.mergeBy compare (Stream.fromList [1,3,5]) (Stream.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
--
-- @
--
-- See also: 'mergeByMFused'
--
-- @since 0.6.0
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
-- @
-- >>> :{
-- do
--  let proportionately m n = do
--       ref <- newIORef $ cycle $ Prelude.concat [Prelude.replicate m LT, Prelude.replicate n GT]
--       return $ \_ _ -> do
--          r <- readIORef ref
--          writeIORef ref $ Prelude.tail r
--          return $ Prelude.head r
--  f <- proportionately 2 1
--  xs <- Stream.toList $ Stream.mergeByM f (Stream.fromList [1,1,1,1,1,1]) (Stream.fromList [2,2,2])
--  print xs
-- :}
-- [1,1,2,1,1,2,1,1,2]
--
-- @
--
-- See also: 'mergeByMFused'
--
-- @since 0.6.0
{-# INLINE mergeByM #-}
mergeByM
    :: (Monad m)
    => (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByM f m1 m2 = fromStreamK $ K.mergeByM f (toStreamK m1) (toStreamK m2)

-- XXX Fused versions should probably go to a separate module using the same
-- names for the combinators.
--
-- | Like 'mergeByM' but much faster, works best when merging statically known
-- number of streams. When merging more than two streams try to merge pairs and
-- pair pf pairs in a tree like structure.'mergeByM' works better with variable
-- number of streams being merged using 'concatPairsWith'.
--
-- /Internal/
{-# INLINE mergeByMFused #-}
mergeByMFused
    :: (Monad m)
    => (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByMFused f m1 m2 =
    fromStreamD $ D.mergeByM f (toStreamD m1) (toStreamD m2)

-- | Like 'mergeByM' but stops merging as soon as any of the two streams stops.
--
-- /Unimplemented/
{-# INLINABLE mergeMinBy #-}
mergeMinBy :: -- (Monad m) =>
    (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeMinBy _f _m1 _m2 = undefined
    -- fromStreamD $ D.mergeMinBy f (toStreamD m1) (toStreamD m2)

-- | Like 'mergeByM' but stops merging as soon as the first stream stops.
--
-- /Unimplemented/
{-# INLINABLE mergeFstBy #-}
mergeFstBy :: -- (Monad m) =>
    (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeFstBy _f _m1 _m2 = undefined
    -- fromStreamK $ D.mergeFstBy f (toStreamD m1) (toStreamD m2)

-- XXX we may want to use the name "merge" differently
-- | Same as @'mergeBy' 'compare'@.
--
-- >>> Stream.toList $ Stream.merge (Stream.fromList [1,3,5]) (Stream.fromList [2,4,6,8])
-- [1,2,3,4,5,6,8]
--
-- /Internal/
--
{-# INLINABLE merge #-}
merge ::
       (Ord a) => Stream m a -> Stream m a -> Stream m a
merge = mergeBy compare

------------------------------------------------------------------------------
-- Combine N Streams - unfoldMany
------------------------------------------------------------------------------

-- | Like 'concatMap' but uses an 'Unfold' for stream generation. Unlike
-- 'concatMap' this can fuse the 'Unfold' code with the inner loop and
-- therefore provide many times better performance.
--
-- @since 0.8.0
{-# INLINE unfoldMany #-}
unfoldMany ::(Monad m) => Unfold m a b -> Stream m a -> Stream m b
unfoldMany u m = fromStreamD $ D.unfoldMany u (toStreamD m)

-- | Like 'unfoldMany' but interleaves the streams in the same way as
-- 'interleave' behaves instead of appending them.
--
-- /Pre-release/
{-# INLINE unfoldManyInterleave #-}
unfoldManyInterleave ::(Monad m)
    => Unfold m a b -> Stream m a -> Stream m b
unfoldManyInterleave u m =
    fromStreamD $ D.unfoldManyInterleave u (toStreamD m)

-- | Like 'unfoldMany' but executes the streams in the same way as
-- 'roundrobin'.
--
-- /Pre-release/
{-# INLINE unfoldManyRoundRobin #-}
unfoldManyRoundRobin ::(Monad m)
    => Unfold m a b -> Stream m a -> Stream m b
unfoldManyRoundRobin u m =
    fromStreamD $ D.unfoldManyRoundRobin u (toStreamD m)

------------------------------------------------------------------------------
-- Combine N Streams - interpose
------------------------------------------------------------------------------

-- > interpose x unf str = gintercalate unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, intersperse the given element between the
-- unfolded streams and then concat them into a single stream.
--
-- > unwords = S.interpose ' '
--
-- /Pre-release/
{-# INLINE interpose #-}
interpose :: (Monad m)
    => c -> Unfold m b c -> Stream m b -> Stream m c
interpose x unf str =
    fromStreamD $ D.interpose (return x) unf (toStreamD str)

-- interposeSuffix x unf str = gintercalateSuffix unf str UF.identity (repeat x)
--
-- | Unfold the elements of a stream, append the given element after each
-- unfolded stream and then concat them into a single stream.
--
-- > unlines = S.interposeSuffix '\n'
--
-- /Pre-release/
{-# INLINE interposeSuffix #-}
interposeSuffix :: (Monad m)
    => c -> Unfold m b c -> Stream m b -> Stream m c
interposeSuffix x unf str =
    fromStreamD $ D.interposeSuffix (return x) unf (toStreamD str)

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
--
-- | 'interleaveInfix' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalate #-}
gintercalate
    :: (Monad m)
    => Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
gintercalate unf1 str1 unf2 str2 =
    fromStreamD $ D.gintercalate
        unf1 (toStreamD str1)
        unf2 (toStreamD str2)

-- > intercalate unf seed str = gintercalate unf str unf (repeatM seed)
--
-- | 'intersperse' followed by unfold and concat.
--
-- > intercalate unf a str = unfoldMany unf $ intersperse a str
-- > intersperse = intercalate (Unfold.function id)
-- > unwords = intercalate Unfold.fromList " "
--
-- >>> Stream.toList $ Stream.intercalate Unfold.fromList " " $ Stream.fromList ["abc", "def", "ghi"]
-- "abc def ghi"
--
-- @since 0.8.0
{-# INLINE intercalate #-}
intercalate :: (Monad m)
    => Unfold m b c -> b -> Stream m b -> Stream m c
intercalate unf seed str = fromStreamD $
    D.unfoldMany unf $ D.intersperse seed (toStreamD str)

-- | 'interleaveSuffix' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalateSuffix #-}
gintercalateSuffix
    :: (Monad m)
    => Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
gintercalateSuffix unf1 str1 unf2 str2 =
    fromStreamD $ D.gintercalateSuffix
        unf1 (toStreamD str1)
        unf2 (toStreamD str2)

-- > intercalateSuffix unf seed str = gintercalateSuffix unf str unf (repeatM seed)
--
-- | 'intersperseSuffix' followed by unfold and concat.
--
-- > intercalateSuffix unf a str = unfoldMany unf $ intersperseSuffix a str
-- > intersperseSuffix = intercalateSuffix (Unfold.function id)
-- > unlines = intercalateSuffix Unfold.fromList "\n"
--
-- >>> Stream.toList $ Stream.intercalateSuffix Unfold.fromList "\n" $ Stream.fromList ["abc", "def", "ghi"]
-- "abc\ndef\nghi\n"
--
-- @since 0.8.0
{-# INLINE intercalateSuffix #-}
intercalateSuffix :: (Monad m)
    => Unfold m b c -> b -> Stream m b -> Stream m c
intercalateSuffix unf seed str = fromStreamD $ D.unfoldMany unf
    $ D.intersperseSuffix (return seed) (toStreamD str)

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | Flatten a stream of streams to a single stream.
--
-- @
-- concat = concatMap id
-- @
--
-- /Pre-release/
{-# INLINE concat #-}
concat :: (Monad m) => Stream m (Stream m a) -> Stream m a
concat = concatMap id

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | Like 'concatMapWith' but carries a state which can be used to share
-- information across multiple steps of concat.
--
-- @
-- concatSmapMWith combine f initial = concatMapWith combine id . smapM f initial
-- @
--
-- /Pre-release/
--
{-# INLINE concatSmapMWith #-}
concatSmapMWith
    :: (Monad m)
    => (Stream m b -> Stream m b -> Stream m b)
    -> (s -> a -> m (s, Stream m b))
    -> m s
    -> Stream m a
    -> Stream m b
concatSmapMWith combine f initial =
    concatMapWith combine id . smapM f initial

-- XXX Implement a StreamD version for fusion.
--
-- | Combine streams in pairs using a binary stream combinator, then combine
-- the resulting streams in pairs recursively until we get to a single combined
-- stream.
--
-- For example, you can sort a stream using merge sort like this:
--
-- >>> Stream.toList $ Stream.concatPairsWith (Stream.mergeBy compare) Stream.fromPure $ Stream.fromList [5,1,7,9,2]
-- [1,2,5,7,9]
--
-- /Caution: the stream of streams must be finite/
--
-- /Pre-release/
--
{-# INLINE concatPairsWith #-}
concatPairsWith ::
       (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> Stream m a
    -> Stream m b
concatPairsWith par f m =
    fromStreamK
        $ K.concatPairsWith
            (\s1 s2 -> toStreamK $ fromStreamK s1 `par` fromStreamK s2)
            (toStreamK . f)
            (toStreamK m)

------------------------------------------------------------------------------
-- IterateMap - Map and flatten Trees of Streams
------------------------------------------------------------------------------

-- | Like 'iterateM' but iterates after mapping a stream generator on the
-- output.
--
-- Yield an input element in the output stream, map a stream generator on it
-- and then do the same on the resulting stream. This can be used for a depth
-- first traversal of a tree like structure.
--
-- Note that 'iterateM' is a special case of 'iterateMapWith':
--
-- @
-- iterateM f = iterateMapWith serial (fromEffect . f) . fromEffect
-- @
--
-- It can be used to traverse a tree structure.  For example, to list a
-- directory tree:
--
-- @
-- Stream.iterateMapWith Stream.serial
--     (either Dir.toEither (const nil))
--     (fromPure (Left "tmp"))
-- @
--
-- /Pre-release/
--
{-# INLINE iterateMapWith #-}
iterateMapWith ::
       (Stream m a -> Stream m a -> Stream m a)
    -> (a -> Stream m a)
    -> Stream m a
    -> Stream m a
iterateMapWith combine f = concatMapWith combine go
    where
    go x = fromPure x `combine` concatMapWith combine go (f x)

-- | Same as @iterateMapWith Stream.serial@ but more efficient due to stream
-- fusion.
--
-- /Unimplemented/
{-# INLINE iterateUnfold #-}
iterateUnfold :: -- (MonadAsync m) =>
    Unfold m a a -> Stream m a -> Stream m a
iterateUnfold = undefined

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
{-# INLINE iterateSmapMWith #-}
iterateSmapMWith
    :: (Monad m)
    => (Stream m a -> Stream m a -> Stream m a)
    -> (b -> a -> m (b, Stream m a))
    -> m b
    -> Stream m a
    -> Stream m a
iterateSmapMWith combine f initial stream =
    concatMap
        (\b -> concatMapWith combine (go b) stream)
        (fromEffect initial)

    where

    go b a = fromPure a `combine` feedback b a

    feedback b a =
        concatMap
            (\(b1, s) -> concatMapWith combine (go b1) s)
            (fromEffect $ f b a)

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
    :: -- (MonadAsync m) =>
       (forall x. t m x -> t m x -> t m x)
    -> (a -> t m (Either (Stream m b) b))
    -> Stream m a
    -> Stream m b
concatMapEitherWith = undefined
-}

-- | In an 'Either' stream iterate on 'Left's.  This is a special case of
-- 'iterateMapWith':
--
-- @
-- iterateMapLeftsWith combine f = iterateMapWith combine (either f (const nil))
-- @
--
-- To traverse a directory tree:
--
-- @
-- iterateMapLeftsWith serial Dir.toEither (fromPure (Left "tmp"))
-- @
--
-- /Pre-release/
--
{-# INLINE iterateMapLeftsWith #-}
iterateMapLeftsWith
    :: (b ~ Either a c)
    => (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> Stream m b
    -> Stream m b
iterateMapLeftsWith combine f =
    iterateMapWith combine (either f (const nil))
