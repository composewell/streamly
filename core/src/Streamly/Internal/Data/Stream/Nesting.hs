{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.Nesting
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module contains unfolding (generational) transformations involving
-- multiple streams, unfolds or folds. There are two types of transformations
-- generational or eliminational. Generational transformations are like the
-- "Generate" module but they generate a stream by combining streams instead of
-- elements. Eliminational transformations are like the "Eliminate" module but
-- they transform a stream by eliminating parts of the stream instead of
-- eliminating the whole stream.
--
-- These combinators involve transformation, generation, elimination so can be
-- classified under any of those.
--
-- Flipped versions can be named as:
-- mapFor/forEach, concatFor, unfoldStepFor (only step function)
-- foreach would be better for streams than mapFor as map could be used for any
-- type not just containers with multiple elements.
--
-- This can be convenient for defining the outer fold step using a lambda.
--
module Streamly.Internal.Data.Stream.Nesting
    (
    -- * Generate
    -- | Combining streams to generate streams.

    -- ** Combine Two Streams
    -- | Functions ending in the shape:
    --
    -- @Stream m a -> Stream m a -> Stream m a@.

    -- *** Interleaving
    -- | Interleave elements from two streams alternately. A special case of
    -- unfoldEachInterleave. Interleave is equivalent to mergeBy with a round
    -- robin merge function.
      InterleaveState(..)
    , interleave
    , interleaveEndBy'
    , interleaveSepBy'
    , interleaveBeginBy
    , interleaveEndBy
    , interleaveSepBy

    -- *** Co-operative Scheduling
    -- | Execute streams alternately irrespective of whether they generate
    -- elements or not. Note that scheduling is affected by the Skip
    -- constructor; implementations with more skips receive proportionally less
    -- scheduling time. A more programmer controlled approach would be to emit
    -- a Maybe in a stream and use the output driven scheduling combinators
    -- instead of Skip driven, even if a stream emits Nothing, the output will
    -- force scheduling of another stream.
    --
    , roundRobin -- interleaveFair?/ParallelFair

    -- *** Merging
    -- | Interleave elements from two streams based on a condition.
    , mergeBy
    , mergeByM
    , mergeMinBy
    , mergeFstBy

    -- ** Combine N Streams
    -- | Functions generally ending in these shapes:
    --
    -- @
    -- concat: f (Stream m a) -> Stream m a
    -- concatMap: (a -> Stream m b) -> Stream m a -> Stream m b
    -- unfoldEach: Unfold m a b -> Stream m a -> Stream m b
    -- @

    -- *** unfoldEach
    -- | Generate streams by using an unfold on each element of an input
    -- stream, append the resulting streams and flatten. A special case of
    -- intercalate.
    , unfoldEachFoldBy
    , ConcatUnfoldInterleaveState (..)
    , bfsUnfoldEach
    , altBfsUnfoldEach
    , fairUnfoldEach

    -- *** unfoldEach joined by elements
    -- | Like unfoldEach but intersperses an element between the streams after
    -- unfolding. A special case of intercalate.
    , unfoldEachSepBy
    , unfoldEachSepByM
    , unfoldEachEndBy
    , unfoldEachEndByM

    -- *** unfoldEach joined by sequences
    -- | Like unfoldEach but intersperses a sequence between the unfolded
    -- streams before unfolding. A special case of intercalate.
    , unfoldEachSepBySeq
    , unfoldEachEndBySeq

    -- *** unfoldEach joined by streams
    -- | Like unfoldEach but intersperses streams between the unfolded streams.
    , intercalateSepBy
    , intercalateEndBy

    -- *** concatMap
    , fairConcatMapM
    , fairConcatMap
    , fairConcatForM
    , fairConcatFor

    -- *** unfoldSched
    -- Note appending does not make sense for sched, only bfs or diagonal.

    -- | Like unfoldEach but schedules the generated streams based on time
    -- slice instead of based on the outputs.
    , unfoldSched
    -- , altUnfoldSched -- alternating directions
    , fairUnfoldSched

    -- *** schedMap
    , schedMapM
    , schedMap
    , fairSchedMapM
    , fairSchedMap

    -- *** schedFor
    , schedForM
    , schedFor
    , fairSchedForM
    , fairSchedFor

    -- * Deprecated
    , interpose
    , interposeM
    , interposeSuffix
    , interposeSuffixM
    , gintercalate
    , gintercalateSuffix
    , intercalate
    , intercalateSuffix
    , unfoldInterleave
    , unfoldRoundRobin
    , interleaveMin
    , interleaveFst
    , interleaveFstSuffix
    )
where

#include "deprecation.h"
#include "inline.hs"
#include "ArrayMacros.h"

import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.Internal.Data.Stream.Generate as Stream
import qualified Streamly.Internal.Data.Unfold.Type as Unfold

import Streamly.Internal.Data.Stream.Transform
    (intersperse, intersperseEndByM)
import Streamly.Internal.Data.Stream.Type

import Prelude hiding (concatMap, zipWith)

#include "DocTestDataStream.hs"

------------------------------------------------------------------------------
-- Interleaving
------------------------------------------------------------------------------

data InterleaveState s1 s2 = InterleaveFirst s1 s2 | InterleaveSecond s1 s2
    | InterleaveSecondOnly s2 | InterleaveFirstOnly s1

-- XXX Ideally we should change the order of the arguments but we have the same
-- convention in append as well, we will have to change that too. Also, the
-- argument order of append makes sense for infix use.

-- | WARNING! O(n^2) time complexity wrt number of streams. Suitable for
-- statically fusing a small number of streams. Use the O(n) complexity
-- StreamK.'Streamly.Data.StreamK.interleave' otherwise.
--
-- Interleaves two streams, yielding one element from each stream alternately,
-- starting from the first stream. When one stream is exhausted, all the
-- remaining elements of the other stream are emitted in the output stream.
--
-- Both the streams are completely exhausted.
--
-- @
-- (a b c) (. . .) => a . b . c .
-- (a b c) (. .  ) => a . b . c
-- (a b  ) (. . .) => a . b .  .
-- @
--
-- Examples:
--
-- >>> f x y = Stream.toList $ Stream.interleave (Stream.fromList x) (Stream.fromList y)
-- >>> f "abc" "..."
-- "a.b.c."
-- >>> f "abc" ".."
-- "a.b.c"
-- >>> f "ab" "..."
-- "a.b.."
--
{-# INLINE_NORMAL interleave #-}
interleave :: Monad m => Stream m a -> Stream m a -> Stream m a
interleave (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveFirst s st2)
            Stop -> Skip (InterleaveSecondOnly st2)

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveSecond st1 s)
            Stop -> Skip (InterleaveFirstOnly st1)

    step gst (InterleaveFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveFirstOnly s)
            Skip s -> Skip (InterleaveFirstOnly s)
            Stop -> Stop

    step gst (InterleaveSecondOnly st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveSecondOnly s)
            Skip s -> Skip (InterleaveSecondOnly s)
            Stop -> Stop

-- XXX Check the performance of the implementation, we can write a custom one.

{-# ANN module "HLint: ignore Use zip" #-}

-- | Interleave the two streams such that the elements of the second stream are
-- ended by the elements of the first stream. If one of the streams is
-- exhausted then interleaving stops.
--
-- @
-- (. . .) (a b c) => a . b . c .
-- (. .  ) (a b c) => a . b .      -- c is discarded
-- (. . .) (a b  ) => a . b .      -- . is discarded
-- @
--
-- Examples:
--
-- >>> f x y = Stream.toList $ Stream.interleaveEndBy' (Stream.fromList x) (Stream.fromList y)
-- >>> f "..." "abc"
-- "a.b.c."
-- >>> f ".." "abc"
-- "a.b."
-- >>> f "..." "ab"
-- "a.b."
--
-- Definition:
--
-- >>> interleaveEndBy' s1 s2 = Stream.unfoldEach Unfold.fromTuple $ Stream.zipWith (,) s2 s1
--
-- Similarly, we can defined interleaveBeginBy' as:
--
-- >>> interleaveBeginBy' = flip interleaveEndBy'
--
{-# INLINE_NORMAL interleaveEndBy' #-}
interleaveEndBy' :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveEndBy' s1 s2 = unfoldEach Unfold.fromTuple $ zipWith (,) s2 s1

-- | Like `interleave` but stops interleaving as soon as any of the two streams
-- stops. The suffix 'Min' in the name determines the stop behavior.
--
-- This is the same as interleaveEndBy' but it might emit an additional element
-- at the end.
--
{-# DEPRECATED interleaveMin "Please use flip interleaveEndBy' instead." #-}
{-# INLINE_NORMAL interleaveMin #-}
interleaveMin :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveMin (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveFirst s st2)
            Stop -> Stop

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveSecond st1 s)
            Stop -> Stop

    step _ (InterleaveFirstOnly _) =  undefined
    step _ (InterleaveSecondOnly _) =  undefined

-- | Interleave the two streams such that the elements of the first stream are
-- infixed between the elements of the second stream. If one of the streams is
-- exhausted then interleaving stops.
--
-- @
-- (. . .) (a b c) => a . b . c    -- additional . is discarded
-- (. .  ) (a b c) => a . b . c
-- (.    ) (a b c) => a . b        -- c is discarded
-- @
--
-- >>> f x y = Stream.toList $ Stream.interleaveSepBy' (Stream.fromList x) (Stream.fromList y)
-- >>> f "..." "abc"
-- "a.b.c"
-- >>> f ".." "abc"
-- "a.b.c"
-- >>> f "." "abc"
-- "a.b"
--
{-# INLINE_NORMAL interleaveSepBy' #-}
interleaveSepBy' :: Monad m => Stream m a -> Stream m a -> Stream m a
-- XXX Not an efficient implementation, need to write a fused one.
interleaveSepBy' s1 s2 = concatEffect $ do
    r <- uncons s2
    case r of
        Nothing -> return Stream.nil
        Just (h, t) ->
            return $ h `Stream.cons`
                unfoldEach Unfold.fromTuple (zipWith (,) s1 t)

-- | Interleave the two streams such that the elements of the second stream are
-- prefixed by the elements of the first stream. Interleaving stops when and
-- only when the second stream is exhausted. Shortfall of the prefix stream is
-- ignored and excess is discarded.
--
-- @
-- (. . .) (a b c) => . a . b . c
-- (. . .) (a b  ) => . a . b      -- additional . is discarded
-- (. .  ) (a b c) => . a . b c    -- missing . is ignored
-- @
--
-- /Unimplemented/
--
{-# INLINE_NORMAL interleaveBeginBy #-}
interleaveBeginBy :: -- Monad m =>
    Stream m a -> Stream m a -> Stream m a
interleaveBeginBy = undefined

-- | Like 'interleaveEndBy'' but interleaving stops when and only when the
-- second stream is exhausted. Shortfall of the suffix stream is ignored and
-- excess is discarded.
--
-- @
-- (. . .) (a b c) => a . b . c .
-- (. .  ) (a b c) => a . b . c    -- missing . is ignored
-- (. . .) (a b  ) => a . b .      -- additional . is discarded
-- @
--
-- >>> f x y = Stream.toList $ Stream.interleaveEndBy (Stream.fromList x) (Stream.fromList y)
-- >>> f "..." "abc"
-- "a.b.c."
-- >>> f ".." "abc"
-- "a.b.c"
-- >>> f "..." "ab"
-- "a.b."
--
{-# INLINE_NORMAL interleaveEndBy #-}
interleaveEndBy :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveEndBy (Stream step2 state2) (Stream step1 state1) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveFirst s st2)
            Stop -> Stop

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveSecond st1 s)
            Stop -> Skip (InterleaveFirstOnly st1)

    step gst (InterleaveFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveFirstOnly s)
            Skip s -> Skip (InterleaveFirstOnly s)
            Stop -> Stop

    step _ (InterleaveSecondOnly _) =  undefined

{-# INLINE interleaveFstSuffix #-}
{-# DEPRECATED interleaveFstSuffix "Please use flip interleaveEndBy instead." #-}
interleaveFstSuffix :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveFstSuffix = flip interleaveEndBy

data InterleaveInfixState s1 s2 a
    = InterleaveInfixFirst s1 s2
    | InterleaveInfixSecondBuf s1 s2
    | InterleaveInfixSecondYield s1 s2 a
    | InterleaveInfixFirstYield s1 s2 a
    | InterleaveInfixFirstOnly s1

-- | Like 'interleaveSepBy'' but interleaving stops when and only when the
-- second stream is exhausted. Shortfall of the infix stream is ignored and
-- excess is discarded.
--
-- @
-- (. . .) (a b c) => a . b . c    -- additional . is discarded
-- (. .  ) (a b c) => a . b . c
-- (.    ) (a b c) => a . b c      -- missing . is ignored
-- @
--
-- Examples:
--
-- >>> f x y = Stream.toList $ Stream.interleaveSepBy (Stream.fromList x) (Stream.fromList y)
-- >>> f "..." "abc"
-- "a.b.c"
-- >>> f ".." "abc"
-- "a.b.c"
-- >>> f "." "abc"
-- "a.bc"
--
{-# INLINE_NORMAL interleaveSepBy #-}
interleaveSepBy :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveSepBy (Stream step2 state2) (Stream step1 state1) =
    Stream step (InterleaveInfixFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveInfixFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveInfixSecondBuf s st2)
            Skip s -> Skip (InterleaveInfixFirst s st2)
            Stop -> Stop

    step gst (InterleaveInfixSecondBuf st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Skip (InterleaveInfixSecondYield st1 s a)
            Skip s -> Skip (InterleaveInfixSecondBuf st1 s)
            Stop -> Skip (InterleaveInfixFirstOnly st1)

    step gst (InterleaveInfixSecondYield st1 st2 x) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield x (InterleaveInfixFirstYield s st2 a)
            Skip s -> Skip (InterleaveInfixSecondYield s st2 x)
            Stop -> Stop

    step _ (InterleaveInfixFirstYield st1 st2 x) = do
        return $ Yield x (InterleaveInfixSecondBuf st1 st2)

    step gst (InterleaveInfixFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveInfixFirstOnly s)
            Skip s -> Skip (InterleaveInfixFirstOnly s)
            Stop -> Stop

{-# DEPRECATED interleaveFst "Please use flip interleaveSepBy instead." #-}
{-# INLINE_NORMAL interleaveFst #-}
interleaveFst :: Monad m => Stream m a -> Stream m a -> Stream m a
interleaveFst = flip interleaveSepBy

------------------------------------------------------------------------------
-- Scheduling
------------------------------------------------------------------------------

-- | Schedule the execution of two streams in a fair round-robin manner,
-- executing each stream once, alternately. Execution of a stream may not
-- necessarily result in an output, a stream may choose to @Skip@ producing an
-- element until later giving the other stream a chance to run. Therefore, this
-- combinator fairly interleaves the execution of two streams rather than
-- fairly interleaving the output of the two streams. This can be useful in
-- co-operative multitasking without using explicit threads. This can be used
-- as an alternative to `async`.
--
-- Scheduling is affected by the Skip constructor; implementations with more
-- skips receive proportionally less scheduling time.
--
-- /Pre-release/
{-# INLINE_NORMAL roundRobin #-}
roundRobin :: Monad m => Stream m a -> Stream m a -> Stream m a
roundRobin (Stream step1 state1) (Stream step2 state2) =
    Stream step (InterleaveFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (InterleaveFirst st1 st2) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveSecond s st2)
            Skip s -> Skip (InterleaveSecond s st2)
            Stop -> Skip (InterleaveSecondOnly st2)

    step gst (InterleaveSecond st1 st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveFirst st1 s)
            Skip s -> Skip (InterleaveFirst st1 s)
            Stop -> Skip (InterleaveFirstOnly st1)

    step gst (InterleaveSecondOnly st2) = do
        r <- step2 gst st2
        return $ case r of
            Yield a s -> Yield a (InterleaveSecondOnly s)
            Skip s -> Skip (InterleaveSecondOnly s)
            Stop -> Stop

    step gst (InterleaveFirstOnly st1) = do
        r <- step1 gst st1
        return $ case r of
            Yield a s -> Yield a (InterleaveFirstOnly s)
            Skip s -> Skip (InterleaveFirstOnly s)
            Stop -> Stop

------------------------------------------------------------------------------
-- Merging
------------------------------------------------------------------------------

-- | Like 'mergeBy' but with a monadic comparison function.
--
-- Example, to merge two streams randomly:
--
-- @
-- > randomly _ _ = randomIO >>= \x -> return $ if x then LT else GT
-- > Stream.toList $ Stream.mergeByM randomly (Stream.fromList [1,1,1,1]) (Stream.fromList [2,2,2,2])
-- [2,1,2,2,2,1,1,1]
-- @
--
-- Example, merge two streams in a proportion of 2:1:
--
-- >>> :set -fno-warn-unrecognised-warning-flags
-- >>> :set -fno-warn-x-partial
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
{-# INLINE_NORMAL mergeByM #-}
mergeByM
    :: (Monad m)
    => (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByM cmp (Stream stepa ta) (Stream stepb tb) =
    Stream step (Just ta, Just tb, Nothing, Nothing)
  where
    {-# INLINE_LATE step #-}

    -- one of the values is missing, and the corresponding stream is running
    step gst (Just sa, sb, Nothing, b) = do
        r <- stepa gst sa
        return $ case r of
            Yield a sa' -> Skip (Just sa', sb, Just a, b)
            Skip sa'    -> Skip (Just sa', sb, Nothing, b)
            Stop        -> Skip (Nothing, sb, Nothing, b)

    step gst (sa, Just sb, a, Nothing) = do
        r <- stepb gst sb
        return $ case r of
            Yield b sb' -> Skip (sa, Just sb', a, Just b)
            Skip sb'    -> Skip (sa, Just sb', a, Nothing)
            Stop        -> Skip (sa, Nothing, a, Nothing)

    -- both the values are available
    step _ (sa, sb, Just a, Just b) = do
        res <- cmp a b
        return $ case res of
            GT -> Yield b (sa, sb, Just a, Nothing)
            _  -> Yield a (sa, sb, Nothing, Just b)

    -- one of the values is missing, corresponding stream is done
    step _ (Nothing, sb, Nothing, Just b) =
            return $ Yield b (Nothing, sb, Nothing, Nothing)

    step _ (sa, Nothing, Just a, Nothing) =
            return $ Yield a (sa, Nothing, Nothing, Nothing)

    step _ (Nothing, Nothing, Nothing, Nothing) = return Stop

-- | WARNING! O(n^2) time complexity wrt number of streams. Suitable for
-- statically fusing a small number of streams. Use the O(n) complexity
-- StreamK.'Streamly.Data.StreamK.mergeBy' otherwise.
--
-- Merge two streams using a comparison function. The head elements of both
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
{-# INLINE mergeBy #-}
mergeBy
    :: (Monad m)
    => (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeBy cmp = mergeByM (\a b -> return $ cmp a b)

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
-- Combine N Streams - unfoldEach
------------------------------------------------------------------------------

-- XXX If we want to have strictly N elements in each batch then we can supply a
-- Maybe input to the fold. That could be another variant of this combinator.

-- | Stream must be finite. Unfolds each element of the input stream to
-- generate streams. After generating one element from each stream fold those
-- using the supplied fold and emit the result in the output stream. Continue
-- doing this until the streams are exhausted.
--
-- /Unimplemented/
{-# INLINE_NORMAL unfoldEachFoldBy #-}
unfoldEachFoldBy :: -- Monad m =>
    Fold m b c -> Unfold m a b -> Stream m a -> Stream m c
unfoldEachFoldBy = undefined

data BfsUnfoldEachState o i =
      BfsUnfoldEachOuter o ([i] -> [i])
    | BfsUnfoldEachInner [i] ([i] -> [i])

-- XXX use arrays to store state instead of lists?
--
-- XXX In general we can use different scheduling strategies e.g. how to
-- schedule the outer vs inner loop or assigning weights to different streams
-- or outer and inner loops.

-- After a yield, switch to the next stream. Do not switch streams on Skip.
-- Yield from outer stream switches to the inner stream.
--
-- There are two choices here, (1) exhaust the outer stream first and then
-- start yielding from the inner streams, this is much simpler to implement,
-- (2) yield at least one element from an inner stream before going back to
-- outer stream and opening the next stream from it.
--
-- Ideally, we need some scheduling bias to inner streams vs outer stream.
-- Maybe we can configure the behavior.

-- | Like 'unfoldEach' but interleaves the resulting streams in a breadth first
-- manner instead of appending them. Unfolds each element in the input stream
-- to a stream and then interleave the resulting streams.
--
-- >>> lists = Stream.fromList [[1,4,7],[2,5,8],[3,6,9]]
-- >>> Stream.toList $ Stream.bfsUnfoldEach Unfold.fromList lists
-- [1,2,3,4,5,6,7,8,9]
--
-- CAUTION! Do not use on infinite streams.
--
{-# INLINE_NORMAL bfsUnfoldEach #-}
bfsUnfoldEach :: Monad m =>
    Unfold m a b -> Stream m a -> Stream m b
bfsUnfoldEach (Unfold istep inject) (Stream ostep ost) =
    Stream step (BfsUnfoldEachOuter ost id)

    where

    {-# INLINE_LATE step #-}
    step gst (BfsUnfoldEachOuter o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (BfsUnfoldEachOuter o' (ls . (i :))))
            Skip o' -> return $ Skip (BfsUnfoldEachOuter o' ls)
            Stop -> return $ Skip (BfsUnfoldEachInner (ls []) id)

    step _ (BfsUnfoldEachInner [] rs) =
        case rs [] of
            [] -> return Stop
            ls -> return $ Skip (BfsUnfoldEachInner ls id)

    step _ (BfsUnfoldEachInner (st:ls) rs) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (BfsUnfoldEachInner ls (rs . (s :)))
            Skip s    -> Skip (BfsUnfoldEachInner (s:ls) rs)
            Stop      -> Skip (BfsUnfoldEachInner ls rs)

data ConcatUnfoldInterleaveState o i =
      ConcatUnfoldInterleaveOuter o [i]
    | ConcatUnfoldInterleaveInner o [i]
    | ConcatUnfoldInterleaveInnerL [i] [i]
    | ConcatUnfoldInterleaveInnerR [i] [i]

-- | Like 'bfsUnfoldEach' but reverses the traversal direction after reaching
-- the last stream and then after reaching the first stream, thus alternating
-- the directions. This could be a little bit more efficient if the order of
-- traversal is not important.
--
-- >>> lists = Stream.fromList [[1,4,7],[2,5,8],[3,6,9]]
-- >>> Stream.toList $ Stream.altBfsUnfoldEach Unfold.fromList lists
-- [1,2,3,6,5,4,7,8,9]
--
-- CAUTION! Do not use on infinite streams.
--
{-# INLINE_NORMAL altBfsUnfoldEach #-}
altBfsUnfoldEach, unfoldInterleave :: Monad m =>
    Unfold m a b -> Stream m a -> Stream m b
altBfsUnfoldEach (Unfold istep inject) (Stream ostep ost) =
    Stream step (ConcatUnfoldInterleaveOuter ost [])

    where

    {-# INLINE_LATE step #-}
    step gst (ConcatUnfoldInterleaveOuter o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (ConcatUnfoldInterleaveInner o' (i : ls)))
            Skip o' -> return $ Skip (ConcatUnfoldInterleaveOuter o' ls)
            Stop -> return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInner _ []) = undefined
    step _ (ConcatUnfoldInterleaveInner o (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveOuter o (s:ls))
            Skip s    -> Skip (ConcatUnfoldInterleaveInner o (s:ls))
            Stop      -> Skip (ConcatUnfoldInterleaveOuter o ls)

    step _ (ConcatUnfoldInterleaveInnerL [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerL [] rs) =
        return $ Skip (ConcatUnfoldInterleaveInnerR [] rs)

    step _ (ConcatUnfoldInterleaveInnerL (st:ls) rs) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerL ls (s:rs))
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerL (s:ls) rs)
            Stop      -> Skip (ConcatUnfoldInterleaveInnerL ls rs)

    step _ (ConcatUnfoldInterleaveInnerR [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerR ls []) =
        return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInnerR ls (st:rs)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerR (s:ls) rs)
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerR ls (s:rs))
            Stop      -> Skip (ConcatUnfoldInterleaveInnerR ls rs)

RENAME(unfoldInterleave,altBfsUnfoldEach)

-- XXX In general we can use different scheduling strategies e.g. how to
-- schedule the outer vs inner loop or assigning weights to different streams
-- or outer and inner loops.
--
-- This could be inefficient if the tasks are too small.
--
-- Compared to unfoldEachInterleave this one switches streams on Skips.

-- | Similar to 'bfsUnfoldEach' but scheduling is independent of output.
--
-- This is an N-ary version of 'roundRobin'.
--
-- >>> lists = Stream.fromList [[1,4,7],[2,5,8],[3,6,9]]
-- >>> Stream.toList $ Stream.unfoldSched Unfold.fromList lists
-- [1,2,3,4,5,6,7,8,9]
--
-- Scheduling is affected by the Skip constructor; implementations with more
-- skips receive proportionally less scheduling time.
--
-- CAUTION! Do not use on infinite streams.
--
{-# INLINE_NORMAL unfoldSched #-}
unfoldSched, unfoldRoundRobin :: Monad m =>
    Unfold m a b -> Stream m a -> Stream m b
unfoldSched (Unfold istep inject) (Stream ostep ost) =
    Stream step (BfsUnfoldEachOuter ost id)

    where

    {-# INLINE_LATE step #-}
    step gst (BfsUnfoldEachOuter o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (BfsUnfoldEachOuter o' (ls . (i :))))
            Skip o' -> return $ Skip (BfsUnfoldEachOuter o' ls)
            Stop -> return $ Skip (BfsUnfoldEachInner (ls []) id)

    step _ (BfsUnfoldEachInner [] rs) =
        case rs [] of
            [] -> return Stop
            ls -> return $ Skip (BfsUnfoldEachInner ls id)

    step _ (BfsUnfoldEachInner (st:ls) rs) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (BfsUnfoldEachInner ls (rs . (s :)))
            Skip s    -> Skip (BfsUnfoldEachInner ls (rs . (s :)))
            Stop      -> Skip (BfsUnfoldEachInner ls rs)

RENAME(unfoldRoundRobin,unfoldSched)

-- | Round robin co-operative scheduling of multiple streams.
--
-- Like concatMap but schedules the generated streams in a round robin
-- fashion. Note that it does not strive to interleave the outputs of the
-- streams, just gives the streams a chance to run whether it produces an
-- output or not. Therefore, the outputs may not seem to be fairly interleaved
-- if a stream decides to skip the output.
--
-- Scheduling is affected by the Skip constructor; implementations with more
-- skips receive proportionally less scheduling time.
--
-- CAUTION! Do not use on infinite streams.
--
{-# INLINE_NORMAL schedMapM #-}
schedMapM :: Monad m => (a -> m (Stream m b)) -> Stream m a -> Stream m b
schedMapM f (Stream ostep ost) =
    Stream step (BfsUnfoldEachOuter ost id)

    where

    {-# INLINE_LATE step #-}
    step gst (BfsUnfoldEachOuter o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- f a
                return (Skip (BfsUnfoldEachOuter o' (ls . (i :))))
            Skip o' -> return $ Skip (BfsUnfoldEachOuter o' ls)
            Stop -> return $ Skip (BfsUnfoldEachInner (ls []) id)

    step _ (BfsUnfoldEachInner [] rs) =
        case rs [] of
            [] -> return Stop
            ls -> return $ Skip (BfsUnfoldEachInner ls id)

    step gst (BfsUnfoldEachInner (UnStream istep st:ls) rs) = do
        r <- istep gst st
        return $ case r of
            Yield x s -> Yield x (BfsUnfoldEachInner ls (rs . (Stream istep s :)))
            Skip s    -> Skip (BfsUnfoldEachInner ls (rs . (Stream istep s :)))
            Stop      -> Skip (BfsUnfoldEachInner ls rs)

-- | See 'SchedFor' for documentation.
--
-- Scheduling is affected by the Skip constructor; implementations with more
-- skips receive proportionally less scheduling time.
--
-- CAUTION! Do not use on infinite streams.
--
{-# INLINE schedMap #-}
schedMap :: Monad m => (a -> Stream m b) -> Stream m a -> Stream m b
schedMap f = schedMapM (return . f)

-- | See 'SchedFor' for documentation.
--
-- Scheduling is affected by the Skip constructor; implementations with more
-- skips receive proportionally less scheduling time.
--
-- CAUTION! Do not use on infinite streams.
--
{-# INLINE schedForM #-}
schedForM :: Monad m => Stream m a -> (a -> m (Stream m b)) -> Stream m b
schedForM = flip schedMapM

-- | Similar to 'bfsConcatFor' but scheduling is independent of output.
--
-- >>> lists = Stream.fromList [[1,4,7],[2,5,8],[3,6,9]]
-- >>> Stream.toList $ Stream.schedFor lists $ \xs -> Stream.fromList xs
-- [1,2,3,4,5,6,7,8,9]
--
-- Scheduling is affected by the Skip constructor; implementations with more
-- skips receive proportionally less scheduling time.
--
-- CAUTION! Do not use on infinite streams.
--
{-# INLINE schedFor #-}
schedFor :: Monad m => Stream m a -> (a -> Stream m b) -> Stream m b
schedFor = flip schedMap

-- | Similar to 'fairUnfoldEach' but scheduling is independent of the output.
--
-- >>> :{
-- outerLoop = Stream.fromList [1,2,3]
-- innerLoop = Unfold.carry $ Unfold.lmap (const [4,5,6]) Unfold.fromList
-- :}
--
-- >>> Stream.toList $ Stream.fairUnfoldSched innerLoop outerLoop
-- [(1,4),(1,5),(2,4),(1,6),(2,5),(3,4),(2,6),(3,5),(3,6)]
--
-- Scheduling is affected by the Skip constructor; implementations with more
-- skips receive proportionally less scheduling time.
--
{-# INLINE_NORMAL fairUnfoldSched #-}
fairUnfoldSched :: Monad m =>
    Unfold m a b -> Stream m a -> Stream m b
fairUnfoldSched (Unfold istep inject) (Stream ostep ost) =
    Stream step (FairUnfoldInit ost id)

    where

    {-# INLINE_LATE step #-}
    step gst (FairUnfoldInit o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (FairUnfoldNext o' id (ls [i])))
            Skip o' -> return $ Skip (FairUnfoldNext o' id (ls []))
            Stop -> return $ Skip (FairUnfoldDrain id (ls []))

    step _ (FairUnfoldNext o ys []) =
            return $ Skip (FairUnfoldInit o ys)

    step _ (FairUnfoldNext o ys (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (FairUnfoldNext o (ys . (s :)) ls)
            Skip s    -> Skip (FairUnfoldNext o (ys . (s :)) ls)
            Stop      -> Skip (FairUnfoldNext o ys ls)

    step _ (FairUnfoldDrain ys []) =
        case ys [] of
            [] -> return Stop
            xs -> return $ Skip (FairUnfoldDrain id xs)

    step _ (FairUnfoldDrain ys (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (FairUnfoldDrain (ys . (s :)) ls)
            Skip s    -> Skip (FairUnfoldDrain (ys . (s :)) ls)
            Stop      -> Skip (FairUnfoldDrain ys ls)

-- | See 'fairConcatFor' for more details. This is similar except that this
-- uses unfolds, therefore, it is much faster due to fusion.
--
-- >>> :{
-- outerLoop = Stream.fromList [1,2,3]
-- innerLoop = Unfold.carry $ Unfold.lmap (const [4,5,6]) Unfold.fromList
-- :}
--
-- >>> Stream.toList $ Stream.fairUnfoldEach innerLoop outerLoop
-- [(1,4),(1,5),(2,4),(1,6),(2,5),(3,4),(2,6),(3,5),(3,6)]
--
{-# INLINE_NORMAL fairUnfoldEach #-}
fairUnfoldEach :: Monad m =>
    Unfold m a b -> Stream m a -> Stream m b
fairUnfoldEach (Unfold istep inject) (Stream ostep ost) =
    Stream step (FairUnfoldInit ost id)

    where

    {-# INLINE_LATE step #-}
    step gst (FairUnfoldInit o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (FairUnfoldNext o' id (ls [i])))
            Skip o' -> return $ Skip (FairUnfoldInit o' ls)
            Stop -> return $ Skip (FairUnfoldDrain id (ls []))

    step _ (FairUnfoldNext o ys []) =
            return $ Skip (FairUnfoldInit o ys)

    step _ (FairUnfoldNext o ys (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (FairUnfoldNext o (ys . (s :)) ls)
            Skip s    -> Skip (FairUnfoldNext o ys (s : ls))
            Stop      -> Skip (FairUnfoldNext o ys ls)

    step _ (FairUnfoldDrain ys []) =
        case ys [] of
            [] -> return Stop
            xs -> return $ Skip (FairUnfoldDrain id xs)

    step _ (FairUnfoldDrain ys (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (FairUnfoldDrain (ys . (s :)) ls)
            Skip s    -> Skip (FairUnfoldDrain ys (s : ls))
            Stop      -> Skip (FairUnfoldDrain ys ls)

-- | See 'fairSchedFor' for documentation.
--
-- Scheduling is affected by the Skip constructor; implementations with more
-- skips receive proportionally less scheduling time.
--
{-# INLINE_NORMAL fairSchedMapM #-}
fairSchedMapM :: Monad m =>
    (a -> m (Stream m b)) -> Stream m a -> Stream m b
fairSchedMapM f (Stream ostep ost) =
    Stream step (FairUnfoldInit ost id)

    where

    {-# INLINE_LATE step #-}
    step gst (FairUnfoldInit o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- f a
                i `seq` return (Skip (FairUnfoldNext o' id (ls [i])))
            Skip o' -> return $ Skip (FairUnfoldNext o' id (ls []))
            Stop -> return $ Skip (FairUnfoldDrain id (ls []))

    step _ (FairUnfoldNext o ys []) =
            return $ Skip (FairUnfoldInit o ys)

    step gst (FairUnfoldNext o ys (UnStream istep st:ls)) = do
        r <- istep gst st
        return $ case r of
            Yield x s -> Yield x (FairUnfoldNext o (ys . (Stream istep s :)) ls)
            Skip s    -> Skip (FairUnfoldNext o (ys . (Stream istep s :)) ls)
            Stop      -> Skip (FairUnfoldNext o ys ls)

    step _ (FairUnfoldDrain ys []) =
        case ys [] of
            [] -> return Stop
            xs -> return $ Skip (FairUnfoldDrain id xs)

    step gst (FairUnfoldDrain ys (UnStream istep st:ls)) = do
        r <- istep gst st
        return $ case r of
            Yield x s -> Yield x (FairUnfoldDrain (ys . (Stream istep s :)) ls)
            Skip s    -> Skip (FairUnfoldDrain (ys . (Stream istep s :)) ls)
            Stop      -> Skip (FairUnfoldDrain ys ls)

-- | See 'fairSchedFor' for documentation.
--
-- Scheduling is affected by the Skip constructor; implementations with more
-- skips receive proportionally less scheduling time.
--
{-# INLINE fairSchedMap #-}
fairSchedMap :: Monad m => (a -> Stream m b) -> Stream m a -> Stream m b
fairSchedMap f = fairSchedMapM (return . f)

-- | See 'fairSchedFor' for documentation.
--
-- Scheduling is affected by the Skip constructor; implementations with more
-- skips receive proportionally less scheduling time.
--
{-# INLINE fairSchedForM #-}
fairSchedForM :: Monad m => Stream m a -> (a -> m (Stream m b)) -> Stream m b
fairSchedForM = flip fairSchedMapM

-- | 'fairSchedFor' is just like 'fairConcatFor', it traverses the depth and
-- breadth of nesting equally. It maintains fairness among different levels of
-- loop iterations.  Therefore, the outer and the inner loops in a nested loop
-- get equal priority. It can be used to nest infinite streams without starving
-- outer streams due to inner ones.
--
-- There is one crucial difference, while 'fairConcatFor' necessarily produces
-- an output from one stream before it schedules the next, 'fairSchedFor'
-- schedules the next stream even if a stream did not produce an output. Thus
-- it interleaves the CPU rather than the outputs of the streams. Thus even if
-- an infinite stream does not produce an output it can not block all other
-- streams.
--
-- Note that the order of emitting the output from different streams may not be
-- predictable, it depends on the skip points inside the stream. Scheduling is
-- affected by the Skip constructor; implementations with more skips receive
-- proportionally less scheduling time.
--
-- == Non-Productive Streams
--
-- Unlike in 'fairConcatFor', if one of the two interleaved streams does not
-- produce an output at all and continues forever then the other stream will
-- still get scheduled. The following program will hang forever for
-- 'fairConcatFor' but will work fine with 'fairSchedFor'.
--
-- >>> :{
-- oddsIf x = Stream.fromList (if x then [1,3..] else [2,4..])
-- filterEven x = if even x then Stream.fromPure x else Stream.nil
-- :}
--
-- >>> :{
-- evens =
--     Stream.fairSchedFor (Stream.fromList [True,False]) $ \r ->
--      Stream.fairSchedFor (oddsIf r) filterEven
-- :}
--
-- >>> Stream.toList $ Stream.take 3 $ evens
-- [2,4,6]
--
-- When @r@ is True, the nested 'fairSchedFor' is a non-productive infinite
-- loop, but still the outer loop gets a chance to generate the @False@ value,
-- and the @evens@ function can produce output. The same code won't terminate
-- if we use 'fairConcatFor' instead of 'fairSchedFor'. Thus even without
-- explicit concurrency we can schedule multiple streams on the same CPU.
--
-- == Logic Programming
--
-- When exploring large streams in logic programming, 'fairSchedFor' can be
-- used as a safe alternative to 'fairConcatFor' as it cannot block due to
-- non-productive infinite streams.
--
{-# INLINE fairSchedFor #-}
fairSchedFor :: Monad m => Stream m a -> (a -> Stream m b) -> Stream m b
fairSchedFor = flip fairSchedMap

-- | See 'fairConcatFor' for documentation.
{-# INLINE_NORMAL fairConcatMapM #-}
fairConcatMapM :: Monad m =>
    (a -> m (Stream m b)) -> Stream m a -> Stream m b
fairConcatMapM f (Stream ostep ost) =
    Stream step (FairUnfoldInit ost id)

    where

    {-# INLINE_LATE step #-}
    step gst (FairUnfoldInit o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- f a
                i `seq` return (Skip (FairUnfoldNext o' id (ls [i])))
            Skip o' -> return $ Skip (FairUnfoldInit o' ls)
            Stop -> return $ Skip (FairUnfoldDrain id (ls []))

    step _ (FairUnfoldNext o ys []) =
            return $ Skip (FairUnfoldInit o ys)

    step gst (FairUnfoldNext o ys (UnStream istep st:ls)) = do
        r <- istep gst st
        return $ case r of
            Yield x s -> Yield x (FairUnfoldNext o (ys . (Stream istep s :)) ls)
            Skip s    -> Skip (FairUnfoldNext o ys (UnStream istep s:ls))
            Stop      -> Skip (FairUnfoldNext o ys ls)

    step _ (FairUnfoldDrain ys []) =
        case ys [] of
            [] -> return Stop
            xs -> return $ Skip (FairUnfoldDrain id xs)

    step gst (FairUnfoldDrain ys (UnStream istep st:ls)) = do
        r <- istep gst st
        return $ case r of
            Yield x s -> Yield x (FairUnfoldDrain (ys . (Stream istep s :)) ls)
            Skip s    -> Skip (FairUnfoldDrain ys (Stream istep s : ls))
            Stop      -> Skip (FairUnfoldDrain ys ls)

-- | See 'fairConcatFor' for documentation.
{-# INLINE fairConcatMap #-}
fairConcatMap :: Monad m => (a -> Stream m b) -> Stream m a -> Stream m b
fairConcatMap f = fairConcatMapM (return . f)

-- | See 'fairConcatFor' for documentation.
{-# INLINE fairConcatForM #-}
fairConcatForM :: Monad m => Stream m a -> (a -> m (Stream m b)) -> Stream m b
fairConcatForM = flip fairConcatMapM

-- | 'fairConcatFor' is like 'concatFor' but traverses the depth and breadth of
-- nesting equally. Therefore, the outer and the inner loops in a nested loop
-- get equal priority. It can be used to nest infinite streams without starving
-- outer streams due to inner ones.
--
-- Given a stream of three streams:
--
-- @
-- 1. [1,2,3]
-- 2. [4,5,6]
-- 3. [7,8,9]
-- @
--
-- Here, outer loop is the stream of streams and the inner loops are the
-- individual streams. The traversal sweeps the diagonals in the above grid to
-- give equal chance to outer and inner loops. The resulting stream is
-- @(1),(2,4),(3,5,7),(6,8),(9)@, diagonals are parenthesized for emphasis.
--
-- == Looping
--
-- A single stream case is equivalent to 'concatFor':
--
-- >>> Stream.toList $ Stream.fairConcatFor (Stream.fromList [1,2]) $ \x -> Stream.fromPure x
-- [1,2]
--
-- == Fair Nested Looping
--
-- Multiple streams nest like @for@ loops. The result is a cross product of the
-- streams. However, the ordering of the results of the cross product is such
-- that each stream gets consumed equally. In other words, inner iterations of
-- a nested loop get the same priority as the outer iterations. Inner
-- iterations do not finish completely before the outer iterations start.
--
-- >>> :{
-- Stream.toList $ do
--     Stream.fairConcatFor (Stream.fromList [1,2,3]) $ \x ->
--      Stream.fairConcatFor (Stream.fromList [4,5,6]) $ \y ->
--       Stream.fromPure (x, y)
-- :}
-- [(1,4),(1,5),(2,4),(1,6),(2,5),(3,4),(2,6),(3,5),(3,6)]
--
-- == Nesting Infinite Streams
--
-- Example with infinite streams. Print all pairs in the cross product with sum
-- less than a specified number.
--
-- >>> :{
-- Stream.toList
--  $ Stream.takeWhile (\(x,y) -> x + y < 6)
--  $ Stream.fairConcatFor (Stream.fromList [1..]) $ \x ->
--     Stream.fairConcatFor (Stream.fromList [1..]) $ \y ->
--      Stream.fromPure (x, y)
-- :}
-- [(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1)]
--
-- == How the nesting works?
--
-- If we look at the cross product of [1,2,3], [4,5,6], the streams being
-- combined using 'fairConcatFor' are the following sequential loop iterations:
--
-- @
-- (1,4) (1,5) (1,6) -- first iteration of the outer loop
-- (2,4) (2,5) (2,6) -- second iteration of the outer loop
-- (3,4) (3,5) (3,6) -- third iteration of the outer loop
-- @
--
-- The result is a triangular or diagonal traversal of these iterations:
--
-- @
-- [(1,4),(1,5),(2,4),(1,6),(2,5),(3,4),(2,6),(3,5),(3,6)]
-- @
--
-- == Non-Termination Cases
--
-- If one of the two interleaved streams does not produce an output at all and
-- continues forever then the other stream will never get scheduled. This is
-- because a stream is unscheduled only after it produces an output. This can
-- lead to non-terminating programs, an example is provided below.
--
-- >>> :{
-- oddsIf x = Stream.fromList (if x then [1,3..] else [2,4..])
-- filterEven x = if even x then Stream.fromPure x else Stream.nil
-- :}
--
-- >>> :{
-- evens =
--     Stream.fairConcatFor (Stream.fromList [True,False]) $ \r ->
--      Stream.concatFor (oddsIf r) filterEven
-- :}
--
-- The @evens@ function does not terminate because, when r is True, the nested
-- 'concatFor' is a non-productive infinite loop, therefore, the outer loop
-- never gets a chance to generate the @False@ value.
--
-- But the following refactoring of the above code works as expected:
--
-- >>> :{
-- mixed =
--      Stream.fairConcatFor (Stream.fromList [True,False]) $ \r ->
--          Stream.concatFor (oddsIf r) Stream.fromPure
-- :}
--
-- >>> evens = Stream.fairConcatFor mixed filterEven
-- >>> Stream.toList $ Stream.take 3 $ evens
-- [2,4,6]
--
-- This works because in @mixed@ both the streams being interleaved are
-- productive.
--
-- Care should be taken how you write your program, keep in mind the scheduling
-- implications. To avoid such scheduling problems in serial interleaving, you
-- can use 'fairSchedFor' or concurrent scheduling i.e. parFairConcatFor. Due
-- to concurrent scheduling the other branch will make progress even if one is
-- an infinite loop producing nothing.
--
-- == Logic Programming
--
-- Streamly provides all operations for logic programming. It provides
-- functionality equivalent to 'LogicT' type from the 'logict' package.
-- The @MonadLogic@ operations can be implemented using the available stream
-- operations. For example, 'uncons' is @msplit@, 'interleave' corresponds to
-- the @interleave@ operation of MonadLogic, 'fairConcatFor' is the
-- fair bind (@>>-@) operation. 'fairSchedFor' is an even better alternative
-- for fair bind, it guarantees that non-productive infinite streams cannot
-- block progress.
--
-- == Related Operations
--
-- See also "Streamly.Internal.Data.StreamK.fairConcatFor".
--
{-# INLINE fairConcatFor #-}
fairConcatFor :: Monad m => Stream m a -> (a -> Stream m b) -> Stream m b
fairConcatFor = flip fairConcatMap

------------------------------------------------------------------------------
-- Combine N Streams - interpose
------------------------------------------------------------------------------

{-# ANN type InterposeSuffixState Fuse #-}
data InterposeSuffixState s1 i1 =
      InterposeSuffixFirst s1
    -- | InterposeSuffixFirstYield s1 i1
    | InterposeSuffixFirstInner s1 i1
    | InterposeSuffixSecond s1

-- XXX Note that if an unfolded layer turns out to be nil we still emit the
-- separator effect. An alternate behavior could be to emit the separator
-- effect only if at least one element has been yielded by the unfolding.
-- However, that becomes a bit complicated, so we have chosen the former
-- behavior for now.

-- | Monadic variant of 'unfoldEachEndBy'.
--
-- Definition:
--
-- >>> unfoldEachEndByM x = Stream.intercalateEndBy Unfold.identity (Stream.repeatM x)
--
{-# INLINE_NORMAL unfoldEachEndByM #-}
unfoldEachEndByM, interposeSuffixM :: Monad m =>
    m c -> Unfold m b c -> Stream m b -> Stream m c
unfoldEachEndByM
    action
    (Unfold istep1 inject1) (Stream step1 state1) =
    Stream step (InterposeSuffixFirst state1)

    where

    {-# INLINE_LATE step #-}
    step gst (InterposeSuffixFirst s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (InterposeSuffixFirstInner s i))
                -- i `seq` return (Skip (InterposeSuffixFirstYield s i))
            Skip s -> return $ Skip (InterposeSuffixFirst s)
            Stop -> return Stop

    {-
    step _ (InterposeSuffixFirstYield s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (InterposeSuffixFirstInner s1 i')
            Skip i'    -> Skip (InterposeSuffixFirstYield s1 i')
            Stop       -> Skip (InterposeSuffixFirst s1)
    -}

    step _ (InterposeSuffixFirstInner s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (InterposeSuffixFirstInner s1 i')
            Skip i'    -> Skip (InterposeSuffixFirstInner s1 i')
            Stop       -> Skip (InterposeSuffixSecond s1)

    step _ (InterposeSuffixSecond s1) = do
        r <- action
        return $ Yield r (InterposeSuffixFirst s1)

-- | Unfold the elements of a stream, append the given element after each
-- unfolded stream and then concat them into a single stream.
--
-- Definition:
--
-- >>> unfoldEachEndBy x = Stream.intercalateEndBy Unfold.identity (Stream.repeat x)
--
-- Usage:
--
-- >>> unlines = Stream.unfoldEachEndBy '\n'
--
-- /Pre-release/
{-# INLINE unfoldEachEndBy #-}
unfoldEachEndBy, interposeSuffix :: Monad m
    => c -> Unfold m b c -> Stream m b -> Stream m c
unfoldEachEndBy x = unfoldEachEndByM (return x)

RENAME(interposeSuffix,unfoldEachEndBy)
RENAME(interposeSuffixM,unfoldEachEndByM)

{-# ANN type InterposeState Fuse #-}
data InterposeState s1 i1 a =
      InterposeFirst s1
    -- | InterposeFirstYield s1 i1
    | InterposeFirstInner s1 i1
    | InterposeFirstInject s1
    -- | InterposeFirstBuf s1 i1
    | InterposeSecondYield s1 i1
    -- -- | InterposeSecondYield s1 i1 a
    -- -- | InterposeFirstResume s1 i1 a

-- Note that this only interposes the pure values, we may run many effects to
-- generate those values as some effects may not generate anything (Skip).

-- | Monadic variant of 'unfoldEachSepBy'.
--
-- Definition:
--
-- >>> unfoldEachSepByM x = Stream.intercalateSepBy Unfold.identity (Stream.repeatM x)
--
{-# INLINE_NORMAL unfoldEachSepByM #-}
unfoldEachSepByM, interposeM :: Monad m =>
    m c -> Unfold m b c -> Stream m b -> Stream m c
unfoldEachSepByM
    action
    (Unfold istep1 inject1) (Stream step1 state1) =
    Stream step (InterposeFirst state1)

    where

    {-# INLINE_LATE step #-}
    step gst (InterposeFirst s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (InterposeFirstInner s i))
                -- i `seq` return (Skip (InterposeFirstYield s i))
            Skip s -> return $ Skip (InterposeFirst s)
            Stop -> return Stop

    {-
    step _ (InterposeFirstYield s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (InterposeFirstInner s1 i')
            Skip i'    -> Skip (InterposeFirstYield s1 i')
            Stop       -> Skip (InterposeFirst s1)
    -}

    step _ (InterposeFirstInner s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (InterposeFirstInner s1 i')
            Skip i'    -> Skip (InterposeFirstInner s1 i')
            Stop       -> Skip (InterposeFirstInject s1)

    step gst (InterposeFirstInject s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                -- i `seq` return (Skip (InterposeFirstBuf s i))
                i `seq` return (Skip (InterposeSecondYield s i))
            Skip s -> return $ Skip (InterposeFirstInject s)
            Stop -> return Stop

    {-
    step _ (InterposeFirstBuf s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Skip (InterposeSecondYield s1 i' x)
            Skip i'    -> Skip (InterposeFirstBuf s1 i')
            Stop       -> Stop
    -}

    {-
    step _ (InterposeSecondYield s1 i1 v) = do
        r <- action
        return $ Yield r (InterposeFirstResume s1 i1 v)
    -}
    step _ (InterposeSecondYield s1 i1) = do
        r <- action
        return $ Yield r (InterposeFirstInner s1 i1)

    {-
    step _ (InterposeFirstResume s1 i1 v) = do
        return $ Yield v (InterposeFirstInner s1 i1)
    -}

-- | Unfold the elements of a stream, intersperse the given element between the
-- unfolded streams and then concat them into a single stream.
--
-- Definition:
--
-- >>> unfoldEachSepBy x = Stream.unfoldEachSepByM (return x)
-- >>> unfoldEachSepBy x = Stream.intercalateSepBy Unfold.identity (Stream.repeat x)
--
-- Usage:
--
-- >>> unwords = Stream.unfoldEachSepBy ' '
--
-- /Pre-release/
{-# INLINE unfoldEachSepBy #-}
unfoldEachSepBy, interpose :: Monad m
    => c -> Unfold m b c -> Stream m b -> Stream m c
unfoldEachSepBy x = unfoldEachSepByM (return x)

RENAME(interposeM,unfoldEachSepByM)
RENAME(interpose,unfoldEachSepBy)

------------------------------------------------------------------------------
-- Combine N Streams - intercalate
------------------------------------------------------------------------------

data ICUState s1 s2 i1 i2 =
      ICUFirst s1 s2
    | ICUSecond s1 s2
    | ICUSecondOnly s2
    | ICUFirstOnly s1
    | ICUFirstInner s1 s2 i1
    | ICUSecondInner s1 s2 i2
    | ICUFirstOnlyInner s1 i1
    | ICUSecondOnlyInner s2 i2

-- | See 'intercalateSepBy' for detailed documentation.
--
-- You can think of this as 'interleaveEndBy' on the stream of streams followed
-- by concat. Same as the following but more efficient:
--
-- >>> intercalateEndBy u1 s1 u2 s2 = Stream.concat $ Stream.interleaveEndBy (fmap (Stream.unfold u1) s1) (fmap (Stream.unfold u2) s2)
--
-- /Pre-release/
{-# INLINE_NORMAL intercalateEndBy #-}
intercalateEndBy :: Monad m =>
       Unfold m a c -> Stream m a
    -> Unfold m b c -> Stream m b
    -> Stream m c
intercalateEndBy
    (Unfold istep2 inject2) (Stream step2 state2)
    (Unfold istep1 inject1) (Stream step1 state1) =
    Stream step (ICUFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (ICUFirst s1 s2) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICUFirstInner s s2 i))
            Skip s -> return $ Skip (ICUFirst s s2)
            Stop -> return Stop

    step gst (ICUFirstOnly s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICUFirstOnlyInner s i))
            Skip s -> return $ Skip (ICUFirstOnly s)
            Stop -> return Stop

    step _ (ICUFirstInner s1 s2 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICUFirstInner s1 s2 i')
            Skip i'    -> Skip (ICUFirstInner s1 s2 i')
            Stop       -> Skip (ICUSecond s1 s2)

    step _ (ICUFirstOnlyInner s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICUFirstOnlyInner s1 i')
            Skip i'    -> Skip (ICUFirstOnlyInner s1 i')
            Stop       -> Skip (ICUFirstOnly s1)

    step gst (ICUSecond s1 s2) = do
        r <- step2 (adaptState gst) s2
        case r of
            Yield a s -> do
                i <- inject2 a
                i `seq` return (Skip (ICUSecondInner s1 s i))
            Skip s -> return $ Skip (ICUSecond s1 s)
            Stop -> return $ Skip (ICUFirstOnly s1)

    step _ (ICUSecondInner s1 s2 i2) = do
        r <- istep2 i2
        return $ case r of
            Yield x i' -> Yield x (ICUSecondInner s1 s2 i')
            Skip i'    -> Skip (ICUSecondInner s1 s2 i')
            Stop       -> Skip (ICUFirst s1 s2)

    step _ (ICUSecondOnly _s2) = undefined
    step _ (ICUSecondOnlyInner _s2 _i2) = undefined

-- |
--
-- >>> gintercalateSuffix u1 s1 u2 s2 = Stream.intercalateEndBy u2 s2 u1 s1
--
{-# DEPRECATED gintercalateSuffix "Please use intercalateEndBy instead. Note the change in argument order." #-}
{-# INLINE gintercalateSuffix #-}
gintercalateSuffix
    :: Monad m
    => Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
gintercalateSuffix u1 s1 u2 s2 = intercalateEndBy u2 s2 u1 s1

data ICALState s1 s2 i1 i2 a =
      ICALFirst s1 s2
    -- | ICALFirstYield s1 s2 i1
    | ICALFirstInner s1 s2 i1
    | ICALFirstOnly s1
    | ICALFirstOnlyInner s1 i1
    | ICALSecondInject s1 s2
    | ICALFirstInject s1 s2 i2
    -- | ICALFirstBuf s1 s2 i1 i2
    | ICALSecondInner s1 s2 i1 i2
    -- -- | ICALSecondInner s1 s2 i1 i2 a
    -- -- | ICALFirstResume s1 s2 i1 i2 a

-- | The first stream @Stream m b@ is turned into a stream of streams by
-- unfolding each element using the first unfold, similarly @Stream m a@ is
-- also turned into a stream of streams.  The second stream of streams is
-- interspersed with the streams from the first stream in an infix manner and
-- then the resulting stream is flattened.
--
-- You can think of this as 'interleaveSepBy' on the stream of streams followed
-- by concat. Same as the following but more efficient:
--
-- >>> intercalateSepBy u1 s1 u2 s2 = Stream.concat $ Stream.interleaveSepBy (fmap (Stream.unfold u1) s1) (fmap (Stream.unfold u2) s2)
--
-- If the separator stream consists of nil streams then it becomes equivalent
-- to 'unfoldEach':
--
-- >>> unfoldEach = Stream.intercalateSepBy (Unfold.nilM (const (return ()))) (Stream.repeat ())
--
-- /Pre-release/
{-# INLINE_NORMAL intercalateSepBy #-}
intercalateSepBy
    :: Monad m
    => Unfold m b c -> Stream m b
    -> Unfold m a c -> Stream m a
    -> Stream m c
{-
intercalateSepBy u1 s1 u2 s2 =
    Stream.concat $ interleaveSepBy (fmap (unfold u1) s1) (fmap (unfold u2) s2)
-}
intercalateSepBy
    (Unfold istep2 inject2) (Stream step2 state2)
    (Unfold istep1 inject1) (Stream step1 state1) =
    Stream step (ICALFirst state1 state2)

    where

    {-# INLINE_LATE step #-}
    step gst (ICALFirst s1 s2) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICALFirstInner s s2 i))
                -- i `seq` return (Skip (ICALFirstYield s s2 i))
            Skip s -> return $ Skip (ICALFirst s s2)
            Stop -> return Stop

    {-
    step _ (ICALFirstYield s1 s2 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICALFirstInner s1 s2 i')
            Skip i'    -> Skip (ICALFirstYield s1 s2 i')
            Stop       -> Skip (ICALFirst s1 s2)
    -}

    step _ (ICALFirstInner s1 s2 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICALFirstInner s1 s2 i')
            Skip i'    -> Skip (ICALFirstInner s1 s2 i')
            Stop       -> Skip (ICALSecondInject s1 s2)

    step gst (ICALFirstOnly s1) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICALFirstOnlyInner s i))
            Skip s -> return $ Skip (ICALFirstOnly s)
            Stop -> return Stop

    step _ (ICALFirstOnlyInner s1 i1) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Yield x (ICALFirstOnlyInner s1 i')
            Skip i'    -> Skip (ICALFirstOnlyInner s1 i')
            Stop       -> Skip (ICALFirstOnly s1)

    -- We inject the second stream even before checking if the first stream
    -- would yield any more elements. There is no clear choice whether we
    -- should do this before or after that. Doing it after may make the state
    -- machine a bit simpler though.
    step gst (ICALSecondInject s1 s2) = do
        r <- step2 (adaptState gst) s2
        case r of
            Yield a s -> do
                i <- inject2 a
                i `seq` return (Skip (ICALFirstInject s1 s i))
            Skip s -> return $ Skip (ICALSecondInject s1 s)
            Stop -> return $ Skip (ICALFirstOnly s1)

    step gst (ICALFirstInject s1 s2 i2) = do
        r <- step1 (adaptState gst) s1
        case r of
            Yield a s -> do
                i <- inject1 a
                i `seq` return (Skip (ICALSecondInner s s2 i i2))
                -- i `seq` return (Skip (ICALFirstBuf s s2 i i2))
            Skip s -> return $ Skip (ICALFirstInject s s2 i2)
            Stop -> return Stop

    {-
    step _ (ICALFirstBuf s1 s2 i1 i2) = do
        r <- istep1 i1
        return $ case r of
            Yield x i' -> Skip (ICALSecondInner s1 s2 i' i2 x)
            Skip i'    -> Skip (ICALFirstBuf s1 s2 i' i2)
            Stop       -> Stop

    step _ (ICALSecondInner s1 s2 i1 i2 v) = do
        r <- istep2 i2
        return $ case r of
            Yield x i' -> Yield x (ICALSecondInner s1 s2 i1 i' v)
            Skip i'    -> Skip (ICALSecondInner s1 s2 i1 i' v)
            Stop       -> Skip (ICALFirstResume s1 s2 i1 i2 v)
    -}

    step _ (ICALSecondInner s1 s2 i1 i2) = do
        r <- istep2 i2
        return $ case r of
            Yield x i' -> Yield x (ICALSecondInner s1 s2 i1 i')
            Skip i'    -> Skip (ICALSecondInner s1 s2 i1 i')
            Stop       -> Skip (ICALFirstInner s1 s2 i1)
            -- Stop       -> Skip (ICALFirstResume s1 s2 i1 i2)

    {-
    step _ (ICALFirstResume s1 s2 i1 i2 x) = do
        return $ Yield x (ICALFirstInner s1 s2 i1 i2)
    -}

-- |
--
-- >>> gintercalate u1 s1 u2 s2 = Stream.intercalateSepBy u2 s2 u1 s1
--
{-# DEPRECATED gintercalate "Please use intercalateSepBy instead." #-}
{-# INLINE gintercalate #-}
gintercalate :: Monad m =>
    Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
gintercalate u1 s1 u2 s2 = intercalateSepBy u2 s2 u1 s1

-- | Unfold each element of the stream, end each unfold by a sequence generated
-- by unfolding the supplied value.
--
-- Definition:
--
-- >>> unfoldEachEndBySeq a u = Stream.unfoldEach u . Stream.intersperseEndByM a
-- >>> unfoldEachEndBySeq a u = Stream.intercalateEndBy u (Stream.repeat a) u
--
-- Idioms:
--
-- >>> intersperseEndByM x = Stream.unfoldEachEndBySeq x Unfold.identity
-- >>> unlines = Stream.unfoldEachEndBySeq "\n" Unfold.fromList
--
-- Usage:
--
-- >>> input = Stream.fromList ["abc", "def", "ghi"]
-- >>> Stream.toList $ Stream.unfoldEachEndBySeq "\n" Unfold.fromList input
-- "abc\ndef\nghi\n"
--
{-# INLINE unfoldEachEndBySeq #-}
unfoldEachEndBySeq :: Monad m
    => b -> Unfold m b c -> Stream m b -> Stream m c
unfoldEachEndBySeq seed unf = unfoldEach unf . intersperseEndByM (return seed)

{-# DEPRECATED intercalateSuffix "Please use unfoldEachEndBySeq instead." #-}
{-# INLINE intercalateSuffix #-}
intercalateSuffix :: Monad m
    => Unfold m b c -> b -> Stream m b -> Stream m c
intercalateSuffix u x = unfoldEachEndBySeq x u

-- | Unfold each element of the stream, separate the successive unfolds by a
-- sequence generated by unfolding the supplied value.
--
-- Definition:
--
-- >>> unfoldEachSepBySeq a u = Stream.unfoldEach u . Stream.intersperse a
-- >>> unfoldEachSepBySeq a u = Stream.intercalateSepBy u (Stream.repeat a) u
--
-- Idioms:
--
-- >>> intersperse x = Stream.unfoldEachSepBySeq x Unfold.identity
-- >>> unwords = Stream.unfoldEachSepBySeq " " Unfold.fromList
--
-- Usage:
--
-- >>> input = Stream.fromList ["abc", "def", "ghi"]
-- >>> Stream.toList $ Stream.unfoldEachSepBySeq " " Unfold.fromList input
-- "abc def ghi"
--
{-# INLINE unfoldEachSepBySeq #-}
unfoldEachSepBySeq :: Monad m
    => b -> Unfold m b c -> Stream m b -> Stream m c
unfoldEachSepBySeq seed unf str = unfoldEach unf $ intersperse seed str

{-# DEPRECATED intercalate "Please use unfoldEachSepBySeq instead." #-}
{-# INLINE intercalate #-}
intercalate :: Monad m
    => Unfold m b c -> b -> Stream m b -> Stream m c
intercalate u x = unfoldEachSepBySeq x u
