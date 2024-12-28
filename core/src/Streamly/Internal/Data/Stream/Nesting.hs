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
-- This module contains transformations involving multiple streams, unfolds or
-- folds. There are two types of transformations generational or eliminational.
-- Generational transformations are like the "Generate" module but they
-- generate a stream by combining streams instead of elements. Eliminational
-- transformations are like the "Eliminate" module but they transform a stream
-- by eliminating parts of the stream instead of eliminating the whole stream.
--
-- These combinators involve transformation, generation, elimination so can be
-- classified under any of those.

-- The zipWithM combinator in this module has been adapted from the vector
-- package (c) Roman Leshchinskiy.
--
module Streamly.Internal.Data.Stream.Nesting
    (
    -- * Generate
    -- | Combining streams to generate streams.

    -- ** Combine Two Streams
    -- | Functions ending in the shape:
    --
    -- @Stream m a -> Stream m a -> Stream m a@.

    -- *** Appending
    -- | Append a stream after another. A special case of concatMap or
    -- unfoldEach Note, appending more than two streams is called @concat@
    -- which could be called appendMany or appendAll in append terminology and
    -- is equivalent to @concatMap id@. Append is equivalent to @mergeBy fst@.
      AppendState(..)
    , append

    -- *** Interleaving
    -- | Interleave elements from two streams alternately. A special case of
    -- unfoldEachInterleave. Interleave is equivalent to mergeBy with a round
    -- robin merge function.
    , InterleaveState(..)
    , interleave
    , interleaveEndBy'
    , interleaveSepBy'
    , interleaveBeginBy
    , interleaveEndBy
    , interleaveSepBy

    -- *** Scheduling
    -- | Execute streams alternately irrespective of whether they generate
    -- elements or not. Note 'interleave' would execute a stream until it
    -- yields an element. A special case of unfoldEachRoundRobin.
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
    , unfoldEachInterleave
    , unfoldEachInterleaveRev
    , unfoldEachRoundRobin

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

    -- * Eliminate
    -- | Folding and Parsing chunks of streams to eliminate nested streams.
    -- Functions generally ending in these shapes:
    --
    -- @
    -- f (Fold m a b) -> t m a -> t m b
    -- f (Parser a m b) -> t m a -> t m b
    -- @

    -- ** Folding
    -- | Apply folds on a stream.
    , foldSequence
    , foldIterateM

    -- ** Parsing
    -- | Parsing is opposite to flattening. 'parseMany' is dual to concatMap or
    -- unfoldEach concatMap generates a stream from single values in a
    -- stream and flattens, parseMany does the opposite of flattening by
    -- splitting the stream and then folds each such split to single value in
    -- the output stream.
    , parseMany
    , parseSequence
    , parseManyTill
    , parseIterate

    -- ** Grouping
    -- | Group segments of a stream and fold. Special case of parsing.
    , groupsWhile
    , groupsRollingBy

    -- ** Splitting
    -- | A special case of parsing.
    , takeEndBySeq
    , takeEndBySeq_
    , wordsBy
    , splitSepBySeq_
    , splitEndBySeq
    , splitEndBySeq_
    , splitOnSuffixSeq -- internal

    , splitBeginBy_
    , splitEndBySeqOneOf
    , splitSepBySeqOneOf

    -- * Transform (Nested Containers)
    -- | Opposite to compact in ArrayStream
    , splitInnerBy -- XXX innerSplitOn
    , splitInnerBySuffix -- XXX innerSplitOnSuffix

    -- * Reduce By Streams
    , dropPrefix
    , dropInfix
    , dropSuffix

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
    , parseManyD
    , parseIterateD
    , groupsBy
    , splitOnSeq
    )
where

#include "deprecation.h"
#include "inline.hs"
#include "ArrayMacros.h"

import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Proxy (Proxy(..))
import Data.Word (Word32)
import Fusion.Plugin.Types (Fuse(..))
import GHC.Types (SPEC(..))

import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.MutArray.Type (MutArray(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.RingArray (RingArray(..))
import Streamly.Internal.Data.SVar.Type (adaptState)
import Streamly.Internal.Data.Unbox (Unbox(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))

import qualified Streamly.Internal.Data.Array.Type as A
import qualified Streamly.Internal.Data.MutArray.Type as MutArray
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser as PRD
import qualified Streamly.Internal.Data.RingArray as RB
import qualified Streamly.Internal.Data.Stream.Generate as Stream
import qualified Streamly.Internal.Data.Unfold.Type as Unfold

import Streamly.Internal.Data.Stream.Transform
    (intersperse, intersperseEndByM)
import Streamly.Internal.Data.Stream.Type

import Prelude hiding (concatMap, mapM, zipWith)

#include "DocTestDataStream.hs"

------------------------------------------------------------------------------
-- Appending
------------------------------------------------------------------------------

data AppendState s1 s2 = AppendFirst s1 | AppendSecond s2

-- Performance Note: From an implementation perspective,
-- StreamK.'Streamly.Data.StreamK.append' translates into a function call
-- whereas Stream.'append' translates into a conditional branch (jump).
-- However, the overhead of the function call in StreamK.append is incurred
-- only once, while the overhead of the conditional branch in fused append is
-- incurred for each element in the stream. As a result, StreamK.append has a
-- linear time complexity of O(n), while fused append has a quadratic time
-- complexity of O(n^2), where @n@ represents the number of 'append's used.

-- | WARNING! O(n^2) time complexity wrt number of streams. Suitable for
-- statically fusing a small number of streams. Use the O(n) complexity
-- StreamK.'Streamly.Data.StreamK.append' otherwise.
--
-- Fuses two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- >>> s1 = Stream.fromList [1,2]
-- >>> s2 = Stream.fromList [3,4]
-- >>> Stream.fold Fold.toList $ s1 `Stream.append` s2
-- [1,2,3,4]
--
{-# INLINE_NORMAL append #-}
append :: Monad m => Stream m a -> Stream m a -> Stream m a
append (Stream step1 state1) (Stream step2 state2) =
    Stream step (AppendFirst state1)

    where

    {-# INLINE_LATE step #-}
    step gst (AppendFirst st) = do
        r <- step1 gst st
        return $ case r of
            Yield a s -> Yield a (AppendFirst s)
            Skip s -> Skip (AppendFirst s)
            Stop -> Skip (AppendSecond state2)

    step gst (AppendSecond st) = do
        r <- step2 gst st
        return $ case r of
            Yield a s -> Yield a (AppendSecond s)
            Skip s -> Skip (AppendSecond s)
            Stop -> Stop

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
-- Do not use dynamically.
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

data ConcatUnfoldInterleaveState o i =
      ConcatUnfoldInterleaveOuter o [i]
    | ConcatUnfoldInterleaveInner o [i]
    | ConcatUnfoldInterleaveInnerL [i] [i]
    | ConcatUnfoldInterleaveInnerR [i] [i]

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

-- XXX Instead of using "reverse" build the list in the correct order to begin
-- with.

-- | Like 'unfoldEach' but interleaves the resulting streams instead of
-- appending them. Unfolds each element in the input stream to a stream and
-- then interleave the resulting streams.
--
-- >>> lists = Stream.fromList [[1,4,7],[2,5,8],[3,6,9]]
-- >>> Stream.toList $ Stream.unfoldEachInterleave Unfold.fromList lists
-- [1,2,3,4,5,6,7,8,9]
--
-- This is similar to 'mergeMapWith' using 'Streamly.Data.StreamK.interleave'
-- but an order of magnitude more efficient due to fusion.
--
-- See also 'mergeMapWith'.
--
{-# INLINE_NORMAL unfoldEachInterleave #-}
unfoldEachInterleave :: Monad m =>
    Unfold m a b -> Stream m a -> Stream m b
unfoldEachInterleave (Unfold istep inject) (Stream ostep ost) =
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
            Stop -> return $ Skip (ConcatUnfoldInterleaveInnerL (reverse ls) [])

    step _ (ConcatUnfoldInterleaveInner _ []) = undefined
    step _ (ConcatUnfoldInterleaveInner o (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveOuter o (s:ls))
            Skip s    -> Skip (ConcatUnfoldInterleaveInner o (s:ls))
            Stop      -> Skip (ConcatUnfoldInterleaveOuter o ls)

    step _ (ConcatUnfoldInterleaveInnerL [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerL [] rs) =
        return $ Skip (ConcatUnfoldInterleaveInnerR [] (reverse rs))

    step _ (ConcatUnfoldInterleaveInnerL (st:ls) rs) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerL ls (s:rs))
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerL (s:ls) rs)
            Stop      -> Skip (ConcatUnfoldInterleaveInnerL ls rs)

    step _ (ConcatUnfoldInterleaveInnerR [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerR ls []) =
        return $ Skip (ConcatUnfoldInterleaveInnerL (reverse ls) [])

    step _ (ConcatUnfoldInterleaveInnerR ls (st:rs)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerR (s:ls) rs)
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerR ls (s:rs))
            Stop      -> Skip (ConcatUnfoldInterleaveInnerR ls rs)

-- | Like 'unfoldEachInterleave' but reverses the traversal direction after
-- reaching the last stream. This could be little bit more efficient if the
-- order of traversal is not important.
--
-- >>> lists = Stream.fromList [[1,4,7],[2,5,8],[3,6,9]]
-- >>> Stream.toList $ Stream.unfoldEachInterleaveRev Unfold.fromList lists
-- [1,2,3,6,5,4,7,8,9]
--
{-# INLINE_NORMAL unfoldEachInterleaveRev #-}
unfoldEachInterleaveRev, unfoldInterleave :: Monad m =>
    Unfold m a b -> Stream m a -> Stream m b
unfoldEachInterleaveRev (Unfold istep inject) (Stream ostep ost) =
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

RENAME(unfoldInterleave,unfoldEachInterleaveRev)

-- XXX In general we can use different scheduling strategies e.g. how to
-- schedule the outer vs inner loop or assigning weights to different streams
-- or outer and inner loops.
--
-- This could be inefficient if the tasks are too small.
--
-- Compared to unfoldEachInterleave this one switches streams on Skips.

-- | 'unfoldEachInterleave' switches to the next stream whenever a value from a
-- stream is yielded, it does not switch on a 'Skip'. So if a stream keeps
-- skipping for long time other streams won't get a chance to run.
-- 'unfoldEachRoundRobin' switches on Skip as well. So it basically schedules each
-- stream fairly irrespective of whether it produces a value or not.
--
{-# INLINE_NORMAL unfoldEachRoundRobin #-}
unfoldEachRoundRobin, unfoldRoundRobin :: Monad m =>
    Unfold m a b -> Stream m a -> Stream m b
unfoldEachRoundRobin (Unfold istep inject) (Stream ostep ost) =
    Stream step (ConcatUnfoldInterleaveOuter ost [])
  where
    {-# INLINE_LATE step #-}
    step gst (ConcatUnfoldInterleaveOuter o ls) = do
        r <- ostep (adaptState gst) o
        case r of
            Yield a o' -> do
                i <- inject a
                i `seq` return (Skip (ConcatUnfoldInterleaveInner o' (i : ls)))
            Skip o' -> return $ Skip (ConcatUnfoldInterleaveInner o' ls)
            Stop -> return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInner o []) =
            return $ Skip (ConcatUnfoldInterleaveOuter o [])

    step _ (ConcatUnfoldInterleaveInner o (st:ls)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveOuter o (s:ls))
            Skip s    -> Skip (ConcatUnfoldInterleaveOuter o (s:ls))
            Stop      -> Skip (ConcatUnfoldInterleaveOuter o ls)

    step _ (ConcatUnfoldInterleaveInnerL [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerL [] rs) =
        return $ Skip (ConcatUnfoldInterleaveInnerR [] rs)

    step _ (ConcatUnfoldInterleaveInnerL (st:ls) rs) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerL ls (s:rs))
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerL ls (s:rs))
            Stop      -> Skip (ConcatUnfoldInterleaveInnerL ls rs)

    step _ (ConcatUnfoldInterleaveInnerR [] []) = return Stop
    step _ (ConcatUnfoldInterleaveInnerR ls []) =
        return $ Skip (ConcatUnfoldInterleaveInnerL ls [])

    step _ (ConcatUnfoldInterleaveInnerR ls (st:rs)) = do
        r <- istep st
        return $ case r of
            Yield x s -> Yield x (ConcatUnfoldInterleaveInnerR (s:ls) rs)
            Skip s    -> Skip (ConcatUnfoldInterleaveInnerR (s:ls) rs)
            Stop      -> Skip (ConcatUnfoldInterleaveInnerR ls rs)

RENAME(unfoldRoundRobin,unfoldEachRoundRobin)

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

------------------------------------------------------------------------------
-- Folding
------------------------------------------------------------------------------

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

{-# ANN type FIterState Fuse #-}
data FIterState s f m a b
    = FIterInit s f
    | forall fs. FIterStream s (fs -> a -> m (FL.Step fs b)) fs (fs -> m b)
        (fs -> m b)
    | FIterYield b (FIterState s f m a b)
    | FIterStop

-- | Iterate a fold generator on a stream. The initial value @b@ is used to
-- generate the first fold, the fold is applied on the stream and the result of
-- the fold is used to generate the next fold and so on.
--
-- Usage:
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
{-# INLINE_NORMAL foldIterateM #-}
foldIterateM ::
       Monad m => (b -> m (FL.Fold m a b)) -> m b -> Stream m a -> Stream m b
foldIterateM func seed0 (Stream step state) =
    Stream stepOuter (FIterInit state seed0)

    where

    {-# INLINE iterStep #-}
    iterStep from st fstep extract final = do
        res <- from
        return
            $ Skip
            $ case res of
                  FL.Partial fs -> FIterStream st fstep fs extract final
                  FL.Done fb -> FIterYield fb $ FIterInit st (return fb)

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ (FIterInit st seed) = do
        (FL.Fold fstep initial extract final) <- seed >>= func
        iterStep initial st fstep extract final
    stepOuter gst (FIterStream st fstep fs extract final) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                iterStep (fstep fs x) s fstep extract final
            Skip s -> return $ Skip $ FIterStream s fstep fs extract final
            Stop -> do
                b <- final fs
                return $ Skip $ FIterYield b FIterStop
    stepOuter _ (FIterYield a next) = return $ Yield a next
    stepOuter _ FIterStop = return Stop

------------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------------

{-# ANN type ParseChunksState Fuse #-}
data ParseChunksState x inpBuf st pst =
      ParseChunksInit inpBuf st
    | ParseChunksInitBuf inpBuf
    | ParseChunksInitLeftOver inpBuf
    | ParseChunksStream st inpBuf !pst
    | ParseChunksStop inpBuf !pst
    | ParseChunksBuf inpBuf st inpBuf !pst
    | ParseChunksExtract inpBuf inpBuf !pst
    | ParseChunksYield x (ParseChunksState x inpBuf st pst)

-- XXX return the remaining stream as part of the error.

-- | Apply a 'Parser' repeatedly on a stream and emit the parsed values in the
-- output stream.
--
-- Usage:
--
-- >>> s = Stream.fromList [1..10]
-- >>> parser = Parser.takeBetween 0 2 Fold.sum
-- >>> Stream.toList $ Stream.parseMany parser s
-- [Right 3,Right 7,Right 11,Right 15,Right 19]
--
-- This is the streaming equivalent of the 'Streamly.Data.Parser.many' parse
-- combinator.
--
-- Known Issues: When the parser fails there is no way to get the remaining
-- stream.
--
{-# INLINE_NORMAL parseMany #-}
parseMany
    :: Monad m
    => PRD.Parser a m b
    -> Stream m a
    -> Stream m (Either ParseError b)
parseMany (PRD.Parser pstep initial extract) (Stream step state) =
    Stream stepOuter (ParseChunksInit [] state)

    where

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, get the first element from the stream, initialize the
    -- fold and then go to stream processing loop.
    stepOuter gst (ParseChunksInit [] st) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                res <- initial
                case res of
                    PRD.IPartial ps ->
                        return $ Skip $ ParseChunksBuf [x] s [] ps
                    PRD.IDone pb ->
                        let next = ParseChunksInit [x] s
                         in return $ Skip $ ParseChunksYield (Right pb) next
                    PRD.IError err ->
                        return
                            $ Skip
                            $ ParseChunksYield
                                (Left (ParseError err))
                                (ParseChunksInitLeftOver [])
            Skip s -> return $ Skip $ ParseChunksInit [] s
            Stop   -> return Stop

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ParseChunksInit src st) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ Skip $ ParseChunksBuf src st [] ps
            PRD.IDone pb ->
                let next = ParseChunksInit src st
                 in return $ Skip $ ParseChunksYield (Right pb) next
            PRD.IError err ->
                return
                    $ Skip
                    $ ParseChunksYield
                        (Left (ParseError err))
                        (ParseChunksInitLeftOver [])

    -- This is simplified ParseChunksInit
    stepOuter _ (ParseChunksInitBuf src) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ Skip $ ParseChunksExtract src [] ps
            PRD.IDone pb ->
                let next = ParseChunksInitBuf src
                 in return $ Skip $ ParseChunksYield (Right pb) next
            PRD.IError err ->
                return
                    $ Skip
                    $ ParseChunksYield
                        (Left (ParseError err))
                        (ParseChunksInitLeftOver [])

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ParseChunksInitLeftOver _) = return Stop

    -- Buffer is empty, process elements from the stream
    stepOuter gst (ParseChunksStream st buf pst) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 ->
                        return $ Skip $ ParseChunksStream s [] pst1
                    PR.Partial n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ParseChunksBuf src s [] pst1
                    PR.Continue 0 pst1 ->
                        return $ Skip $ ParseChunksStream s (x:buf) pst1
                    PR.Continue n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ParseChunksBuf src s buf1 pst1
                    PR.Done 0 b -> do
                        return $ Skip $
                            ParseChunksYield (Right b) (ParseChunksInit [] s)
                    PR.Done n b -> do
                        assert (n <= length (x:buf)) (return ())
                        let src = Prelude.reverse (Prelude.take n (x:buf))
                        return $ Skip $
                            ParseChunksYield (Right b) (ParseChunksInit src s)
                    PR.Error err ->
                        return
                            $ Skip
                            $ ParseChunksYield
                                (Left (ParseError err))
                                (ParseChunksInitLeftOver [])
            Skip s -> return $ Skip $ ParseChunksStream s buf pst
            Stop -> return $ Skip $ ParseChunksStop buf pst

    -- go back to stream processing mode
    stepOuter _ (ParseChunksBuf [] s buf pst) =
        return $ Skip $ ParseChunksStream s buf pst

    -- buffered processing loop
    stepOuter _ (ParseChunksBuf (x:xs) s buf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ Skip $ ParseChunksBuf xs s [] pst1
            PR.Partial n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksBuf src s [] pst1
            PR.Continue 0 pst1 ->
                return $ Skip $ ParseChunksBuf xs s (x:buf) pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksBuf src s buf1 pst1
            PR.Done 0 b ->
                return
                    $ Skip
                    $ ParseChunksYield (Right b) (ParseChunksInit xs s)
            PR.Done n b -> do
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return $ Skip
                    $ ParseChunksYield (Right b) (ParseChunksInit src s)
            PR.Error err ->
                return
                    $ Skip
                    $ ParseChunksYield
                        (Left (ParseError err))
                        (ParseChunksInitLeftOver [])

    -- This is simplified ParseChunksBuf
    stepOuter _ (ParseChunksExtract [] buf pst) =
        return $ Skip $ ParseChunksStop buf pst

    stepOuter _ (ParseChunksExtract (x:xs) buf pst) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ Skip $ ParseChunksExtract xs [] pst1
            PR.Partial n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksExtract src [] pst1
            PR.Continue 0 pst1 ->
                return $ Skip $ ParseChunksExtract xs (x:buf) pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ParseChunksExtract src buf1 pst1
            PR.Done 0 b ->
                return
                    $ Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitBuf xs)
            PR.Done n b -> do
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return
                    $ Skip
                    $ ParseChunksYield (Right b) (ParseChunksInitBuf src)
            PR.Error err ->
                return
                    $ Skip
                    $ ParseChunksYield
                        (Left (ParseError err))
                        (ParseChunksInitLeftOver [])

    -- This is simplified ParseChunksExtract
    stepOuter _ (ParseChunksStop buf pst) = do
        pRes <- extract pst
        case pRes of
            PR.Partial _ _ -> error "Bug: parseMany: Partial in extract"
            PR.Continue 0 pst1 ->
                return $ Skip $ ParseChunksStop buf pst1
            PR.Continue n pst1 -> do
                assert (n <= length buf) (return ())
                let (src0, buf1) = splitAt n buf
                    src  = Prelude.reverse src0
                return $ Skip $ ParseChunksExtract src buf1 pst1
            PR.Done 0 b -> do
                return $ Skip $
                    ParseChunksYield (Right b) (ParseChunksInitLeftOver [])
            PR.Done n b -> do
                assert (n <= length buf) (return ())
                let src = Prelude.reverse (Prelude.take n buf)
                return $ Skip $
                    ParseChunksYield (Right b) (ParseChunksInitBuf src)
            PR.Error err ->
                return
                    $ Skip
                    $ ParseChunksYield
                        (Left (ParseError err))
                        (ParseChunksInitLeftOver [])

    stepOuter _ (ParseChunksYield a next) = return $ Yield a next

{-# DEPRECATED parseManyD "Please use parseMany instead." #-}
{-# INLINE parseManyD #-}
parseManyD
    :: Monad m
    => PR.Parser a m b
    -> Stream m a
    -> Stream m (Either ParseError b)
parseManyD = parseMany

-- | Apply a stream of parsers to an input stream and emit the results in the
-- output stream.
--
-- /Unimplemented/
--
{-# INLINE parseSequence #-}
parseSequence
       :: -- Monad m =>
       Stream m (PR.Parser a m b)
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
       PR.Parser a m b
    -> PR.Parser a m x
    -> Stream m a
    -> Stream m b
parseManyTill = undefined

{-# ANN type ConcatParseState Fuse #-}
data ConcatParseState c b inpBuf st p m a =
      ConcatParseInit inpBuf st p
    | ConcatParseInitBuf inpBuf p
    | ConcatParseInitLeftOver inpBuf
    | forall s. ConcatParseStop
        inpBuf (s -> a -> m (PRD.Step s b)) s (s -> m (PRD.Step s b))
    | forall s. ConcatParseStream
        st inpBuf (s -> a -> m (PRD.Step s b)) s (s -> m (PRD.Step s b))
    | forall s. ConcatParseBuf
        inpBuf st inpBuf (s -> a -> m (PRD.Step s b)) s (s -> m (PRD.Step s b))
    | forall s. ConcatParseExtract
        inpBuf inpBuf (s -> a -> m (PRD.Step s b)) s (s -> m (PRD.Step s b))
    | ConcatParseYield c (ConcatParseState c b inpBuf st p m a)

-- | Iterate a parser generating function on a stream. The initial value @b@ is
-- used to generate the first parser, the parser is applied on the stream and
-- the result is used to generate the next parser and so on.
--
-- Example:
--
-- >>> import Data.Monoid (Sum(..))
-- >>> s = Stream.fromList [1..10]
-- >>> Stream.toList $ fmap getSum $ Stream.catRights $ Stream.parseIterate (\b -> Parser.takeBetween 0 2 (Fold.sconcat b)) (Sum 0) $ fmap Sum s
-- [3,10,21,36,55,55]
--
-- This is the streaming equivalent of monad like sequenced application of
-- parsers where next parser is dependent on the previous parser.
--
-- /Pre-release/
--
{-# INLINE_NORMAL parseIterate #-}
parseIterate
    :: Monad m
    => (b -> PRD.Parser a m b)
    -> b
    -> Stream m a
    -> Stream m (Either ParseError b)
parseIterate func seed (Stream step state) =
    Stream stepOuter (ConcatParseInit [] state (func seed))

    where

    {-# INLINE_LATE stepOuter #-}
    -- Buffer is empty, go to stream processing loop
    stepOuter _ (ConcatParseInit [] st (PRD.Parser pstep initial extract)) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ Skip $ ConcatParseStream st [] pstep ps extract
            PRD.IDone pb ->
                let next = ConcatParseInit [] st (func pb)
                 in return $ Skip $ ConcatParseYield (Right pb) next
            PRD.IError err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (ParseError err))
                        (ConcatParseInitLeftOver [])

    -- Buffer is not empty, go to buffered processing loop
    stepOuter _ (ConcatParseInit src st
                    (PRD.Parser pstep initial extract)) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ Skip $ ConcatParseBuf src st [] pstep ps extract
            PRD.IDone pb ->
                let next = ConcatParseInit src st (func pb)
                 in return $ Skip $ ConcatParseYield (Right pb) next
            PRD.IError err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (ParseError err))
                        (ConcatParseInitLeftOver [])

    -- This is simplified ConcatParseInit
    stepOuter _ (ConcatParseInitBuf src
                    (PRD.Parser pstep initial extract)) = do
        res <- initial
        case res of
            PRD.IPartial ps ->
                return $ Skip $ ConcatParseExtract src [] pstep ps extract
            PRD.IDone pb ->
                let next = ConcatParseInitBuf src (func pb)
                 in return $ Skip $ ConcatParseYield (Right pb) next
            PRD.IError err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (ParseError err))
                        (ConcatParseInitLeftOver [])

    -- XXX we just discard any leftover input at the end
    stepOuter _ (ConcatParseInitLeftOver _) = return Stop

    -- Buffer is empty process elements from the stream
    stepOuter gst (ConcatParseStream st buf pstep pst extract) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                pRes <- pstep pst x
                case pRes of
                    PR.Partial 0 pst1 ->
                        return $ Skip $ ConcatParseStream s [] pstep pst1 extract
                    PR.Partial n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let src0 = Prelude.take n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ConcatParseBuf src s [] pstep pst1 extract
                    -- PR.Continue 0 pst1 ->
                    --     return $ Skip $ ConcatParseStream s (x:buf) pst1
                    PR.Continue n pst1 -> do
                        assert (n <= length (x:buf)) (return ())
                        let (src0, buf1) = splitAt n (x:buf)
                            src  = Prelude.reverse src0
                        return $ Skip $ ConcatParseBuf src s buf1 pstep pst1 extract
                    -- XXX Specialize for Stop 0 common case?
                    PR.Done n b -> do
                        assert (n <= length (x:buf)) (return ())
                        let src = Prelude.reverse (Prelude.take n (x:buf))
                        return $ Skip $
                            ConcatParseYield (Right b) (ConcatParseInit src s (func b))
                    PR.Error err ->
                        return
                            $ Skip
                            $ ConcatParseYield
                                (Left (ParseError err))
                                (ConcatParseInitLeftOver [])
            Skip s -> return $ Skip $ ConcatParseStream s buf pstep pst extract
            Stop -> return $ Skip $ ConcatParseStop buf pstep pst extract

    -- go back to stream processing mode
    stepOuter _ (ConcatParseBuf [] s buf pstep ps extract) =
        return $ Skip $ ConcatParseStream s buf pstep ps extract

    -- buffered processing loop
    stepOuter _ (ConcatParseBuf (x:xs) s buf pstep pst extract) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ Skip $ ConcatParseBuf xs s [] pstep pst1 extract
            PR.Partial n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ConcatParseBuf src s [] pstep pst1 extract
         -- PR.Continue 0 pst1 -> return $ Skip $ ConcatParseBuf xs s (x:buf) pst1
            PR.Continue n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ConcatParseBuf src s buf1 pstep pst1 extract
            -- XXX Specialize for Stop 0 common case?
            PR.Done n b -> do
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return $ Skip $ ConcatParseYield (Right b)
                                    (ConcatParseInit src s (func b))
            PR.Error err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (ParseError err))
                        (ConcatParseInitLeftOver [])

    -- This is simplified ConcatParseBuf
    stepOuter _ (ConcatParseExtract [] buf pstep pst extract) =
        return $ Skip $ ConcatParseStop buf pstep pst extract

    stepOuter _ (ConcatParseExtract (x:xs) buf pstep pst extract) = do
        pRes <- pstep pst x
        case pRes of
            PR.Partial 0 pst1 ->
                return $ Skip $ ConcatParseExtract xs [] pstep pst1 extract
            PR.Partial n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let src0 = Prelude.take n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ConcatParseExtract src [] pstep pst1 extract
            PR.Continue 0 pst1 ->
                return $ Skip $ ConcatParseExtract xs (x:buf) pstep pst1 extract
            PR.Continue n pst1 -> do
                assert (n <= length (x:buf)) (return ())
                let (src0, buf1) = splitAt n (x:buf)
                    src  = Prelude.reverse src0 ++ xs
                return $ Skip $ ConcatParseExtract src buf1 pstep pst1 extract
            PR.Done 0 b ->
                 return $ Skip $ ConcatParseYield (Right b) (ConcatParseInitBuf xs (func b))
            PR.Done n b -> do
                assert (n <= length (x:buf)) (return ())
                let src = Prelude.reverse (Prelude.take n (x:buf)) ++ xs
                return $ Skip $ ConcatParseYield (Right b) (ConcatParseInitBuf src (func b))
            PR.Error err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (ParseError err))
                        (ConcatParseInitLeftOver [])

    -- This is simplified ConcatParseExtract
    stepOuter _ (ConcatParseStop buf pstep pst extract) = do
        pRes <- extract pst
        case pRes of
            PR.Partial _ _ -> error "Bug: parseIterate: Partial in extract"
            PR.Continue 0 pst1 ->
                return $ Skip $ ConcatParseStop buf pstep pst1 extract
            PR.Continue n pst1 -> do
                assert (n <= length buf) (return ())
                let (src0, buf1) = splitAt n buf
                    src  = Prelude.reverse src0
                return $ Skip $ ConcatParseExtract src buf1 pstep pst1 extract
            PR.Done 0 b -> do
                return $ Skip $
                    ConcatParseYield (Right b) (ConcatParseInitLeftOver [])
            PR.Done n b -> do
                assert (n <= length buf) (return ())
                let src = Prelude.reverse (Prelude.take n buf)
                return $ Skip $
                    ConcatParseYield (Right b) (ConcatParseInitBuf src (func b))
            PR.Error err ->
                return
                    $ Skip
                    $ ConcatParseYield
                        (Left (ParseError err))
                        (ConcatParseInitLeftOver [])

    stepOuter _ (ConcatParseYield a next) = return $ Yield a next

{-# DEPRECATED parseIterateD "Please use parseIterate instead." #-}
{-# INLINE parseIterateD #-}
parseIterateD
    :: Monad m
    => (b -> PR.Parser a m b)
    -> b
    -> Stream m a
    -> Stream m (Either ParseError b)
parseIterateD = parseIterate

------------------------------------------------------------------------------
-- Grouping
------------------------------------------------------------------------------

data GroupByState st fs a b
    = GroupingInit st
    | GroupingDo st !fs
    | GroupingInitWith st !a
    | GroupingDoWith st !fs !a
    | GroupingYield !b (GroupByState st fs a b)
    | GroupingDone

-- | Keep collecting items in a group as long as the comparison function
-- returns true. The comparison function is @cmp old new@ where @old@ is the
-- first item in the group and @new@ is the incoming item being tested for
-- membership of the group. The collected items are folded by the supplied
-- fold.
--
-- Definition:
--
-- >>> groupsWhile cmp f = Stream.parseMany (Parser.groupBy cmp f)
{-# INLINE_NORMAL groupsWhile #-}
groupsWhile :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
{-
groupsWhile eq fld = parseMany (PRD.groupBy eq fld)
-}
groupsWhile cmp (Fold fstep initial _ final) (Stream step state) =
    Stream stepOuter (GroupingInit state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ (GroupingInit st) = do
        -- XXX Note that if the stream stops without yielding a single element
        -- in the group we discard the "initial" effect.
        res <- initial
        return
            $ case res of
                  FL.Partial s -> Skip $ GroupingDo st s
                  FL.Done b -> Yield b $ GroupingInit st
    stepOuter gst (GroupingDo st fs) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                r <- fstep fs x
                case r of
                    FL.Partial fs1 -> go SPEC x s fs1
                    FL.Done b -> return $ Yield b (GroupingInit s)
            Skip s -> return $ Skip $ GroupingDo s fs
            Stop -> final fs >> return Stop

        where

        go !_ prev stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp prev x
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC prev s fs1
                            FL.Done b -> return $ Yield b (GroupingInit s)
                    else do
                        r <- final acc
                        return $ Yield r (GroupingInitWith s x)
                Skip s -> go SPEC prev s acc
                Stop -> do
                    r <- final acc
                    return $ Yield r GroupingDone
    stepOuter _ (GroupingInitWith st x) = do
        res <- initial
        return
            $ case res of
                  FL.Partial s -> Skip $ GroupingDoWith st s x
                  FL.Done b -> Yield b $ GroupingInitWith st x
    stepOuter gst (GroupingDoWith st fs prev) = do
        res <- fstep fs prev
        case res of
            FL.Partial fs1 -> go SPEC st fs1
            FL.Done b -> return $ Yield b (GroupingInit st)

        where

        -- XXX code duplicated from the previous equation
        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp prev x
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC s fs1
                            FL.Done b -> return $ Yield b (GroupingInit s)
                    else do
                        r <- final acc
                        return $ Yield r (GroupingInitWith s x)
                Skip s -> go SPEC s acc
                Stop -> do
                    r <- final acc
                    return $ Yield r GroupingDone
    stepOuter _ (GroupingYield _ _) = error "groupsWhile: Unreachable"
    stepOuter _ GroupingDone = return Stop

-- | The argument order of the comparison function in `groupsWhile` is
-- different than that of `groupsBy`.
--
-- In `groupsBy` the comparison function takes the next element as the first
-- argument and the previous element as the second argument. In `groupsWhile`
-- the first argument is the previous element and second argument is the next
-- element.
{-# DEPRECATED groupsBy "Please use groupsWhile instead. Please note the change in the argument order of the comparison function." #-}
{-# INLINE_NORMAL groupsBy #-}
groupsBy :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
groupsBy cmp = groupsWhile (flip cmp)

-- |
--
-- Definition:
--
-- >>> groupsRollingBy cmp f = Stream.parseMany (Parser.groupByRolling cmp f)
--
{-# INLINE_NORMAL groupsRollingBy #-}
groupsRollingBy :: Monad m
    => (a -> a -> Bool)
    -> Fold m a b
    -> Stream m a
    -> Stream m b
{-
groupsRollingBy eq fld = parseMany (PRD.groupByRolling eq fld)
-}
groupsRollingBy cmp (Fold fstep initial _ final) (Stream step state) =
    Stream stepOuter (GroupingInit state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ (GroupingInit st) = do
        -- XXX Note that if the stream stops without yielding a single element
        -- in the group we discard the "initial" effect.
        res <- initial
        return
            $ case res of
                  FL.Partial fs -> Skip $ GroupingDo st fs
                  FL.Done fb -> Yield fb $ GroupingInit st
    stepOuter gst (GroupingDo st fs) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                r <- fstep fs x
                case r of
                    FL.Partial fs1 -> go SPEC x s fs1
                    FL.Done fb -> return $ Yield fb (GroupingInit s)
            Skip s -> return $ Skip $ GroupingDo s fs
            Stop -> final fs >> return Stop

        where

        go !_ prev stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp prev x
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC x s fs1
                            FL.Done b -> return $ Yield b (GroupingInit s)
                    else do
                        r <- final acc
                        return $ Yield r (GroupingInitWith s x)
                Skip s -> go SPEC prev s acc
                Stop -> do
                    r <- final acc
                    return $ Yield r GroupingDone
    stepOuter _ (GroupingInitWith st x) = do
        res <- initial
        return
            $ case res of
                  FL.Partial s -> Skip $ GroupingDoWith st s x
                  FL.Done b -> Yield b $ GroupingInitWith st x
    stepOuter gst (GroupingDoWith st fs previous) = do
        res <- fstep fs previous
        case res of
            FL.Partial s -> go SPEC previous st s
            FL.Done b -> return $ Yield b (GroupingInit st)

        where

        -- XXX GHC: groupsWhile has one less parameter in this go loop and it
        -- fuses. However, groupsRollingBy does not fuse, removing the prev
        -- parameter makes it fuse. Something needs to be fixed in GHC. The
        -- workaround for this is noted in the comments below.
        go !_ prev !stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if cmp prev x
                    then do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC x s fs1
                            FL.Done b -> return $ Yield b (GroupingInit st)
                    else do
                        {-
                        r <- final acc
                        return $ Yield r (GroupingInitWith s x)
                        -}
                        -- The code above does not let groupBy fuse. We use the
                        -- alternative code below instead.  Instead of jumping
                        -- to GroupingInitWith state, we unroll the code of
                        -- GroupingInitWith state here to help GHC with stream
                        -- fusion.
                        result <- initial
                        r <- final acc
                        return
                            $ Yield r
                            $ case result of
                                  FL.Partial fsi -> GroupingDoWith s fsi x
                                  FL.Done b -> GroupingYield b (GroupingInit s)
                Skip s -> go SPEC prev s acc
                Stop -> do
                    r <- final acc
                    return $ Yield r GroupingDone
    stepOuter _ (GroupingYield r next) = return $ Yield r next
    stepOuter _ GroupingDone = return Stop

------------------------------------------------------------------------------
-- Splitting - by a predicate
------------------------------------------------------------------------------

data WordsByState st fs b
    = WordsByInit st
    | WordsByDo st !fs
    | WordsByDone
    | WordsByYield !b (WordsByState st fs b)

-- | Split the stream after stripping leading, trailing, and repeated
-- separators determined by the predicate supplied. The tokens after splitting
-- are collected by the supplied fold. In other words, the tokens are parsed in
-- the same way as words are parsed from whitespace separated text.
--
-- >>> f x = Stream.toList $ Stream.wordsBy (== '.') Fold.toList $ Stream.fromList x
-- >>> f "a.b"
-- ["a","b"]
-- >>> f "a..b"
-- ["a","b"]
-- >>> f ".a..b."
-- ["a","b"]
--
{-# INLINE_NORMAL wordsBy #-}
wordsBy :: Monad m => (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
wordsBy predicate (Fold fstep initial _ final) (Stream step state) =
    Stream stepOuter (WordsByInit state)

    where

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ (WordsByInit st) = do
        res <- initial
        return
            $ case res of
                  FL.Partial s -> Skip $ WordsByDo st s
                  FL.Done b -> Yield b (WordsByInit st)

    stepOuter gst (WordsByDo st fs) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                if predicate x
                then do
                    resi <- initial
                    return
                        $ case resi of
                              FL.Partial fs1 -> Skip $ WordsByDo s fs1
                              FL.Done b -> Yield b (WordsByInit s)
                else do
                    r <- fstep fs x
                    case r of
                        FL.Partial fs1 -> go SPEC s fs1
                        FL.Done b -> return $ Yield b (WordsByInit s)
            Skip s    -> return $ Skip $ WordsByDo s fs
            Stop      -> final fs >> return Stop

        where

        go !_ stt !acc = do
            res <- step (adaptState gst) stt
            case res of
                Yield x s -> do
                    if predicate x
                    then do
                        {-
                        r <- final acc
                        return $ Yield r (WordsByInit s)
                        -}
                        -- The above code does not fuse well. Need to check why
                        -- GHC is not able to simplify it well.  Using the code
                        -- below, instead of jumping through the WordsByInit
                        -- state always, we directly go to WordsByDo state in
                        -- the common case of Partial.
                        resi <- initial
                        r <- final acc
                        return
                            $ Yield r
                            $ case resi of
                                  FL.Partial fs1 -> WordsByDo s fs1
                                  FL.Done b -> WordsByYield b (WordsByInit s)
                    else do
                        r <- fstep acc x
                        case r of
                            FL.Partial fs1 -> go SPEC s fs1
                            FL.Done b -> return $ Yield b (WordsByInit s)
                Skip s -> go SPEC s acc
                Stop -> do
                    r <- final acc
                    return $ Yield r WordsByDone

    stepOuter _ WordsByDone = return Stop

    stepOuter _ (WordsByYield b next) = return $ Yield b next

------------------------------------------------------------------------------
-- Splitting on a sequence
------------------------------------------------------------------------------

-- String search algorithms:
-- http://www-igm.univ-mlv.fr/~lecroq/string/index.html

-- XXX Can GHC find a way to modularise this? Can we write different cases
-- i.e.g single element, word hash, karp-rabin as different functions and then
-- be able to combine them into a single state machine?

{-# ANN type TakeEndBySeqState Fuse #-}
data TakeEndBySeqState mba rb rh ck w s b x =
      TakeEndBySeqInit
    | TakeEndBySeqYield !b (TakeEndBySeqState mba rb rh ck w s b x)
    | TakeEndBySeqDone

    | TakeEndBySeqSingle s x

    | TakeEndBySeqWordInit !Int !w s
    | TakeEndBySeqWordLoop !w s
    | TakeEndBySeqWordDone !Int !w

    | TakeEndBySeqKRInit s mba
    | TakeEndBySeqKRInit1 s mba !Int
    | TakeEndBySeqKRLoop s mba !rh !ck
    | TakeEndBySeqKRCheck s mba !rh
    | TakeEndBySeqKRDone !Int rb

-- | If the pattern is empty the output stream is empty.
{-# INLINE_NORMAL takeEndBySeqWith #-}
takeEndBySeqWith
    :: forall m a. (MonadIO m, Unbox a, Enum a, Eq a)
    => Bool
    -> Array a
    -> Stream m a
    -> Stream m a
takeEndBySeqWith withSep patArr (Stream step state) =
    Stream stepOuter TakeEndBySeqInit

    where

    patLen = A.length patArr
    patBytes = A.byteLength patArr
    maxIndex = patLen - 1
    maxOffset = patBytes - SIZE_OF(a)
    elemBits = SIZE_OF(a) * 8

    -- For word pattern case
    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. A.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    -- For Rabin-Karp search
    k = 2891336453 :: Word32
    coeff = k ^ patLen

    addCksum cksum a = cksum * k + fromIntegral (fromEnum a)

    deltaCksum cksum old new =
        addCksum cksum new - coeff * fromIntegral (fromEnum old)

    -- XXX shall we use a random starting hash or 1 instead of 0?
    patHash = A.foldl' addCksum 0 patArr

    skip = return . Skip

    {-# INLINE yield #-}
    yield x !s = skip $ TakeEndBySeqYield x s

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ TakeEndBySeqInit = do
        -- XXX When we statically specify the method compiler is able to
        -- simplify the code better and removes the handling of other states.
        -- When it is determined dynamically, the code is less efficient. For
        -- example, the single element search degrades by 80% if the handling
        -- of other cases is present. We need to investigate this further but
        -- until then we can guide the compiler statically where we can. If we
        -- want to use single element search statically then we can use
        -- takeEndBy instead.
        --
        -- XXX Is there a way for GHC to statically determine patLen when we
        -- use an array created from a static string as pattern e.g. "\n".
        case () of
            _ | patLen == 0 -> return Stop
              | patLen == 1 -> do
                    pat <- liftIO $ A.unsafeGetIndexIO 0 patArr
                    return $ Skip $ TakeEndBySeqSingle state pat
              | SIZE_OF(a) * patLen <= sizeOf (Proxy :: Proxy Word) ->
                    return $ Skip $ TakeEndBySeqWordInit 0 0 state
              | otherwise -> do
                    (MutArray mba _ _ _) :: MutArray a <-
                        liftIO $ MutArray.emptyOf patLen
                    skip $ TakeEndBySeqKRInit state mba

    ---------------------
    -- Single yield point
    ---------------------

    stepOuter _ (TakeEndBySeqYield x next) = return $ Yield x next

    -----------------
    -- Done
    -----------------

    stepOuter _ TakeEndBySeqDone = return Stop

    -----------------
    -- Single Pattern
    -----------------

    stepOuter gst (TakeEndBySeqSingle st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s ->
                if pat /= x
                then yield x (TakeEndBySeqSingle s pat)
                else do
                    if withSep
                    then yield x TakeEndBySeqDone
                    else return Stop
            Skip s -> skip $ TakeEndBySeqSingle s pat
            Stop -> return Stop

    ---------------------------
    -- Short Pattern - Shift Or
    ---------------------------

    -- Note: Karp-Rabin is roughly 15% slower than word hash for a 2 element
    -- pattern. This may be useful for common cases like splitting lines using
    -- "\r\n".
    stepOuter _ (TakeEndBySeqWordDone 0 _) = do
        return Stop
    stepOuter _ (TakeEndBySeqWordDone n wrd) = do
        let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
         in yield
                (toEnum $ fromIntegral old)
                (TakeEndBySeqWordDone (n - 1) wrd)

    -- XXX If we remove this init state for perf experiment the time taken
    -- reduces to half, there may be some optimization opportunity here.
    stepOuter gst (TakeEndBySeqWordInit idx wrd st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                let wrd1 = addToWord wrd x
                    next
                      | idx /= maxIndex =
                            TakeEndBySeqWordInit (idx + 1) wrd1 s
                      | wrd1 .&. wordMask /= wordPat =
                            TakeEndBySeqWordLoop wrd1 s
                      | otherwise = TakeEndBySeqDone
                if withSep
                then yield x next
                else skip next
            Skip s -> skip $ TakeEndBySeqWordInit idx wrd s
            Stop ->
                if withSep
                then return Stop
                else skip $ TakeEndBySeqWordDone idx wrd

    stepOuter gst (TakeEndBySeqWordLoop wrd st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                -- XXX Never use a lazy expression as state, that causes issues
                -- in simplification because the state argument of Yield is
                -- lazy, maybe we can make that strict.
                let wrd1 = addToWord wrd x
                    old = (wordMask .&. wrd)
                            `shiftR` (elemBits * (patLen - 1))
                    !y =
                            if withSep
                            then x
                            else toEnum $ fromIntegral old
                -- Note: changing the nesting order of if and yield makes a
                -- difference in performance.
                if wrd1 .&. wordMask /= wordPat
                then yield y (TakeEndBySeqWordLoop wrd1 s)
                else yield y TakeEndBySeqDone
            Skip s -> skip $ TakeEndBySeqWordLoop wrd s
            Stop ->
                 if withSep
                 then return Stop
                 else skip $ TakeEndBySeqWordDone patLen wrd

    -------------------------------
    -- General Pattern - Karp Rabin
    -------------------------------

    stepOuter gst (TakeEndBySeqKRInit st0 mba) = do
        res <- step (adaptState gst) st0
        case res of
            Yield x s -> do
                liftIO $ pokeAt 0 mba x
                if withSep
                then yield x (TakeEndBySeqKRInit1 s mba (SIZE_OF(a)))
                else skip $ TakeEndBySeqKRInit1 s mba (SIZE_OF(a))
            Skip s -> skip $ TakeEndBySeqKRInit s mba
            Stop -> return Stop

    stepOuter gst (TakeEndBySeqKRInit1 st mba offset) = do
        res <- step (adaptState gst) st
        let arr :: Array a = Array
                    { arrContents = mba
                    , arrStart = 0
                    , arrEnd = patBytes
                    }
        case res of
            Yield x s -> do
                liftIO $ pokeAt offset mba x
                let next =
                        if offset /= maxOffset
                        then TakeEndBySeqKRInit1 s mba (offset + SIZE_OF(a))
                        else
                            let ringHash = A.foldl' addCksum 0 arr
                             in if ringHash == patHash
                                then TakeEndBySeqKRCheck s mba 0
                                else TakeEndBySeqKRLoop s mba 0 ringHash
                if withSep
                then yield x next
                else skip next
            Skip s -> skip $ TakeEndBySeqKRInit1 s mba offset
            Stop -> do
                if withSep
                then return Stop
                else do
                    let rb = RingArray
                            { ringContents = mba
                            , ringSize = offset
                            , ringHead = 0
                            }
                     in skip $ TakeEndBySeqKRDone offset rb

    stepOuter gst (TakeEndBySeqKRLoop st mba rh cksum) = do
        res <- step (adaptState gst) st
        let rb = RingArray
                { ringContents = mba
                , ringSize = patBytes
                , ringHead = rh
                }
        case res of
            Yield x s -> do
                (rb1, old) <- liftIO (RB.replace rb x)
                let cksum1 = deltaCksum cksum old x
                let rh1 = ringHead rb1
                    next =
                        if cksum1 /= patHash
                        then TakeEndBySeqKRLoop s mba rh1 cksum1
                        else TakeEndBySeqKRCheck s mba rh1
                if withSep
                then yield x next
                else yield old next
            Skip s -> skip $ TakeEndBySeqKRLoop s mba rh cksum
            Stop -> do
                if withSep
                then return Stop
                else skip $ TakeEndBySeqKRDone patBytes rb

    stepOuter _ (TakeEndBySeqKRCheck st mba rh) = do
        let rb = RingArray
                    { ringContents = mba
                    , ringSize = patBytes
                    , ringHead = rh
                    }
        matches <- liftIO $ RB.eqArray rb patArr
        if matches
        then return Stop
        else skip $ TakeEndBySeqKRLoop st mba rh patHash

    stepOuter _ (TakeEndBySeqKRDone 0 _) = return Stop
    stepOuter _ (TakeEndBySeqKRDone len rb) = do
        assert (len >= 0) (return ())
        old <- RB.unsafeGetHead rb
        let rb1 = RB.moveForward rb
        yield old $ TakeEndBySeqKRDone (len - SIZE_OF(a)) rb1

-- | Take the stream until the supplied sequence is encountered. Take the
-- sequence as well and stop.
--
-- Usage:
--
-- >>> f pat xs = Stream.toList $ Stream.takeEndBySeq (Array.fromList pat) $ Stream.fromList xs
-- >>> f "fgh" "abcdefghijk"
-- "abcdefgh"
-- >>> f "lmn" "abcdefghijk"
-- "abcdefghijk"
-- >>> f "" "abcdefghijk"
-- ""
--
{-# INLINE takeEndBySeq #-}
takeEndBySeq
    :: forall m a. (MonadIO m, Unbox a, Enum a, Eq a)
    => Array a
    -> Stream m a
    -> Stream m a
takeEndBySeq = takeEndBySeqWith True

-- | Take the stream until the supplied sequence is encountered. Do not take
-- the sequence.
--
-- Usage:
--
-- >>> f pat xs = Stream.toList $ Stream.takeEndBySeq_ (Array.fromList pat) $ Stream.fromList xs
-- >>> f "fgh" "abcdefghijk"
-- "abcde"
-- >>> f "lmn" "abcdefghijk"
-- "abcdefghijk"
-- >>> f "" "abcdefghijk"
-- ""
--
{-# INLINE takeEndBySeq_ #-}
takeEndBySeq_
    :: forall m a. (MonadIO m, Unbox a, Enum a, Eq a)
    => Array a
    -> Stream m a
    -> Stream m a
takeEndBySeq_ = takeEndBySeqWith False

{-
-- TODO can we unify the splitting operations using a splitting configuration
-- like in the split package.
--
data SplitStyle = Infix | Suffix | Prefix deriving (Eq, Show)
data SplitOptions = SplitOptions
    { style    :: SplitStyle
    , withSep  :: Bool  -- ^ keep the separators in output
    -- , compact  :: Bool  -- ^ treat multiple consecutive separators as one
    -- , trimHead :: Bool  -- ^ drop blank at head
    -- , trimTail :: Bool  -- ^ drop blank at tail
    }
-}

-- XXX using "fs" as the last arg in Constructors may simplify the code a bit,
-- because we can use the constructor directly without having to create "jump"
-- functions.
{-# ANN type SplitOnSeqState Fuse #-}
data SplitOnSeqState mba rb rh ck w fs s b x =
      SplitOnSeqInit
    | SplitOnSeqYield b (SplitOnSeqState mba rb rh ck w fs s b x)
    | SplitOnSeqDone

    | SplitOnSeqEmpty !fs s

    | SplitOnSeqSingle !fs s x

    | SplitOnSeqWordInit !fs s
    | SplitOnSeqWordLoop !w s !fs
    | SplitOnSeqWordDone Int !fs !w

    | SplitOnSeqKRInit Int !fs s mba
    | SplitOnSeqKRLoop fs s mba !rh !ck
    | SplitOnSeqKRCheck fs s mba !rh
    | SplitOnSeqKRDone Int !fs rb

    | SplitOnSeqReinit (fs -> SplitOnSeqState mba rb rh ck w fs s b x)

-- XXX Need to fix empty stream split behavior

-- | Like 'splitOn' but splits the stream on a sequence of elements rather than
-- a single element. Parses a sequence of tokens separated by an infixed
-- separator e.g. @a;b;c@ is parsed as @a@, @b@, @c@. If the pattern is empty
-- then each element is a match, thus the fold is finalized on each element.
--
-- Equivalent to the following:
--
-- >>> splitSepBySeq_ pat f = Stream.foldManyPost (Fold.takeEndBySeq_ pat f)
--
-- Uses Rabin-Karp algorithm for substring search.
--
{-# INLINE_NORMAL splitSepBySeq_ #-}
splitSepBySeq_, splitOnSeq
    :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitSepBySeq_ patArr (Fold fstep initial _ final) (Stream step state) =
    Stream stepOuter SplitOnSeqInit

    where

    patLen = A.length patArr
    patBytes = A.byteLength patArr
    maxIndex = patLen - 1
    maxOffset = patBytes - SIZE_OF(a)
    elemBits = SIZE_OF(a) * 8

    -- For word pattern case
    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. A.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    -- For Rabin-Karp search
    k = 2891336453 :: Word32
    coeff = k ^ patLen

    addCksum cksum a = cksum * k + fromIntegral (fromEnum a)

    deltaCksum cksum old new =
        addCksum cksum new - coeff * fromIntegral (fromEnum old)

    -- XXX shall we use a random starting hash or 1 instead of 0?
    patHash = A.foldl' addCksum 0 patArr

    skip = return . Skip

    nextAfterInit nextGen stepRes =
        case stepRes of
            FL.Partial s -> nextGen s
            FL.Done b -> SplitOnSeqYield b (SplitOnSeqReinit nextGen)

    {-# INLINE yieldReinit #-}
    yieldReinit nextGen fs =
        initial >>= skip . SplitOnSeqYield fs . nextAfterInit nextGen

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ SplitOnSeqInit = do
        res <- initial
        case res of
            FL.Partial acc
                | patLen == 0 ->
                    return $ Skip $ SplitOnSeqEmpty acc state
                | patLen == 1 -> do
                    pat <- liftIO $ A.unsafeGetIndexIO 0 patArr
                    return $ Skip $ SplitOnSeqSingle acc state pat
                | SIZE_OF(a) * patLen <= sizeOf (Proxy :: Proxy Word) ->
                    return $ Skip $ SplitOnSeqWordInit acc state
                | otherwise -> do
                    (MutArray mba _ _ _) :: MutArray a <-
                        liftIO $ MutArray.emptyOf patLen
                    skip $ SplitOnSeqKRInit 0 acc state mba
            FL.Done b -> skip $ SplitOnSeqYield b SplitOnSeqInit

    stepOuter _ (SplitOnSeqYield x next) = return $ Yield x next

    ---------------------------
    -- Checkpoint
    ---------------------------

    stepOuter _ (SplitOnSeqReinit nextGen) =
        initial >>= skip . nextAfterInit nextGen

    ---------------------------
    -- Empty pattern
    ---------------------------

    stepOuter gst (SplitOnSeqEmpty acc st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                r <- fstep acc x
                b1 <-
                    case r of
                        FL.Partial acc1 -> final acc1
                        FL.Done b -> return b
                let jump c = SplitOnSeqEmpty c s
                 in yieldReinit jump b1
            Skip s -> skip (SplitOnSeqEmpty acc s)
            Stop -> final acc >> return Stop

    -----------------
    -- Done
    -----------------

    stepOuter _ SplitOnSeqDone = return Stop

    -----------------
    -- Single Pattern
    -----------------

    stepOuter gst (SplitOnSeqSingle fs0 st0 pat) = do
        go SPEC fs0 st0

        where

        -- The local loop increases allocations by 6% but improves CPU
        -- performance by 14%.
        go !_ !fs !st = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let jump c = SplitOnSeqSingle c s pat
                    if pat == x
                    then final fs >>= yieldReinit jump
                    else do
                        r <- fstep fs x
                        case r of
                            FL.Partial fs1 -> go SPEC fs1 s
                            FL.Done b -> yieldReinit jump b
                Skip s -> go SPEC fs s
                Stop -> do
                    r <- final fs
                    return $ Skip $ SplitOnSeqYield r SplitOnSeqDone

    ---------------------------
    -- Short Pattern - Shift Or
    ---------------------------

    -- Note: We fill the matching buffer before we emit anything, in case it
    -- matches and we have to drop it. Though we could be more eager in
    -- emitting as soon as we know that the pattern cannot match. But still the
    -- worst case will remain the same, in case a match is going to happen we
    -- will have to delay until the very end.

    stepOuter _ (SplitOnSeqWordDone 0 fs _) = do
        r <- final fs
        skip $ SplitOnSeqYield r SplitOnSeqDone
    stepOuter _ (SplitOnSeqWordDone n fs wrd) = do
        let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
        r <- fstep fs (toEnum $ fromIntegral old)
        case r of
            FL.Partial fs1 -> skip $ SplitOnSeqWordDone (n - 1) fs1 wrd
            FL.Done b -> do
                 let jump c = SplitOnSeqWordDone (n - 1) c wrd
                 yieldReinit jump b

    stepOuter gst (SplitOnSeqWordInit fs st0) =
        go SPEC 0 0 st0

        where

        {-# INLINE go #-}
        go !_ !idx !wrd !st = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let wrd1 = addToWord wrd x
                    if idx == maxIndex
                    then do
                        if wrd1 .&. wordMask == wordPat
                        then do
                            let jump c = SplitOnSeqWordInit c s
                            final fs >>= yieldReinit jump
                        else skip $ SplitOnSeqWordLoop wrd1 s fs
                    else go SPEC (idx + 1) wrd1 s
                Skip s -> go SPEC idx wrd s
                Stop -> do
                    if idx /= 0
                    then skip $ SplitOnSeqWordDone idx fs wrd
                    else do
                        r <- final fs
                        skip $ SplitOnSeqYield r SplitOnSeqDone

    stepOuter gst (SplitOnSeqWordLoop wrd0 st0 fs0) =
        go SPEC wrd0 st0 fs0

        where

        -- This loop does not affect allocations but it improves the CPU
        -- performance signifcantly compared to looping using state.
        {-# INLINE go #-}
        go !_ !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let jump c = SplitOnSeqWordInit c s
                        wrd1 = addToWord wrd x
                        old = (wordMask .&. wrd)
                                `shiftR` (elemBits * (patLen - 1))
                    r <- fstep fs (toEnum $ fromIntegral old)
                    case r of
                        FL.Partial fs1 -> do
                            if wrd1 .&. wordMask == wordPat
                            then final fs1 >>= yieldReinit jump
                            else go SPEC wrd1 s fs1
                        FL.Done b -> yieldReinit jump b
                Skip s -> go SPEC wrd s fs
                Stop -> skip $ SplitOnSeqWordDone patLen fs wrd

    -------------------------------
    -- General Pattern - Karp Rabin
    -------------------------------

    -- XXX Document this pattern for writing efficient code. Loop around only
    -- required elements in the recursive loop, build the structures being
    -- manipulated locally e.g. we are passing only mba, here and build an
    -- array using patLen and arrStart from the surrounding context.

    stepOuter gst (SplitOnSeqKRInit offset fs st mba) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                liftIO $ pokeAt offset mba x
                if offset == maxOffset
                then do
                    let arr :: Array a = Array
                                { arrContents = mba
                                , arrStart = 0
                                , arrEnd = patBytes
                                }
                    let ringHash = A.foldl' addCksum 0 arr
                    if ringHash == patHash && A.byteEq arr patArr
                    then skip $ SplitOnSeqKRCheck fs s mba 0
                    else skip $ SplitOnSeqKRLoop fs s mba 0 ringHash
                else skip $ SplitOnSeqKRInit (offset + SIZE_OF(a)) fs s mba
            Skip s -> skip $ SplitOnSeqKRInit offset fs s mba
            Stop -> do
                let rb = RingArray
                        { ringContents = mba
                        , ringSize = offset
                        , ringHead = 0
                        }
                skip $ SplitOnSeqKRDone offset fs rb

    -- XXX The recursive "go" is more efficient than the state based recursion
    -- code commented out below. Perhaps its more efficient because of
    -- factoring out "mba" outside the loop.
    --
    stepOuter gst (SplitOnSeqKRLoop fs0 st0 mba rh0 cksum0) =
        go SPEC fs0 st0 rh0 cksum0

        where

        go !_ !fs !st !rh !cksum = do
            res <- step (adaptState gst) st
            let rb = RingArray
                    { ringContents = mba
                    , ringSize = patBytes
                    , ringHead = rh
                    }
            case res of
                Yield x s -> do
                    (rb1, old) <- liftIO (RB.replace rb x)
                    r <- fstep fs old
                    case r of
                        FL.Partial fs1 -> do
                            let cksum1 = deltaCksum cksum old x
                            let rh1 = ringHead rb1
                            if cksum1 == patHash
                            then skip $ SplitOnSeqKRCheck fs1 s mba rh1
                            else go SPEC fs1 s rh1 cksum1
                        FL.Done b -> do
                            -- XXX the old code looks wrong as we are resetting
                            -- the ring head but the ring still has old
                            -- elements as we are not resetting the size.
                            let jump c = SplitOnSeqKRInit 0 c s mba
                            yieldReinit jump b
                Skip s -> go SPEC fs s rh cksum
                Stop -> skip $ SplitOnSeqKRDone patBytes fs rb

    -- XXX The following code is 5 times slower compared to the recursive loop
    -- based code above. Need to investigate why. One possibility is that the
    -- go loop above does not thread around the ring buffer (rb). This code may
    -- be causing the state to bloat and getting allocated on each iteration.
    -- We can check the cmm/asm code to confirm.  If so a good GHC solution to
    -- such problem is needed. One way to avoid this could be to use unboxed
    -- mutable state?
    {-
    stepOuter gst (SplitOnSeqKRLoop fs st rb rh cksum) = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    old <- liftIO $ peek rh
                    let cksum1 = deltaCksum cksum old x
                    fs1 <- fstep fs old
                    if (cksum1 == patHash)
                    then do
                        r <- done fs1
                        skip $ SplitOnSeqYield r $ SplitOnSeqKRInit 0 s rb rh
                    else do
                        rh1 <- liftIO (RB.unsafeInsert rb rh x)
                        skip $ SplitOnSeqKRLoop fs1 s rb rh1 cksum1
                Skip s -> skip $ SplitOnSeqKRLoop fs s rb rh cksum
                Stop -> skip $ SplitOnSeqKRDone patLen fs rb rh
    -}

    stepOuter _ (SplitOnSeqKRCheck fs st mba rh) = do
        let rb = RingArray
                    { ringContents = mba
                    , ringSize = patBytes
                    , ringHead = rh
                    }
        res <- liftIO $ RB.eqArray rb patArr
        if res
        then do
            r <- final fs
            let jump c = SplitOnSeqKRInit 0 c st mba
            yieldReinit jump r
        else skip $ SplitOnSeqKRLoop fs st mba rh patHash

    stepOuter _ (SplitOnSeqKRDone 0 fs _) = do
        r <- final fs
        skip $ SplitOnSeqYield r SplitOnSeqDone
    stepOuter _ (SplitOnSeqKRDone len fs rb) = do
        assert (len >= 0) (return ())
        old <- RB.unsafeGetHead rb
        let rb1 = RB.moveForward rb
        r <- fstep fs old
        case r of
            FL.Partial fs1 -> skip $ SplitOnSeqKRDone (len - SIZE_OF(a)) fs1 rb1
            FL.Done b -> do
                 let jump c = SplitOnSeqKRDone (len - SIZE_OF(a)) c rb1
                 yieldReinit jump b

RENAME(splitOnSeq,splitSepBySeq_)

{-# ANN type SplitOnSuffixSeqState Fuse #-}
data SplitOnSuffixSeqState mba rb rh ck w fs s b x =
      SplitOnSuffixSeqInit
    | SplitOnSuffixSeqYield b (SplitOnSuffixSeqState mba rb rh ck w fs s b x)
    | SplitOnSuffixSeqDone

    | SplitOnSuffixSeqEmpty !fs s

    | SplitOnSuffixSeqSingleInit !fs s x
    | SplitOnSuffixSeqSingle !fs s x

    | SplitOnSuffixSeqWordInit !fs s
    | SplitOnSuffixSeqWordLoop !w s !fs
    | SplitOnSuffixSeqWordDone Int !fs !w

    | SplitOnSuffixSeqKRInit !fs s mba
    | SplitOnSuffixSeqKRInit1 !fs s mba
    | SplitOnSuffixSeqKRLoop fs s mba !rh !ck
    | SplitOnSuffixSeqKRCheck fs s mba !rh
    | SplitOnSuffixSeqKRDone Int !fs rb

    | SplitOnSuffixSeqReinit
          (fs -> SplitOnSuffixSeqState mba rb rh ck w fs s b x)

-- | @splitOnSuffixSeq withSep pat fld input@ splits the input using @pat@ as a
-- suffixed separator, the resulting split segments are fed to the fold @fld@.
-- If @withSep@ is True then the separator sequence is also suffixed with the
-- split segments.
--
-- /Internal/
{-# INLINE_NORMAL splitOnSuffixSeq #-}
splitOnSuffixSeq
    :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a)
    => Bool
    -> Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitOnSuffixSeq withSep patArr (Fold fstep initial _ final) (Stream step state) =
    Stream stepOuter SplitOnSuffixSeqInit

    where

    patLen = A.length patArr
    patBytes = A.byteLength patArr
    maxIndex = patLen - 1
    maxOffset = patBytes - SIZE_OF(a)
    elemBits = SIZE_OF(a) * 8

    -- For word pattern case
    wordMask :: Word
    wordMask = (1 `shiftL` (elemBits * patLen)) - 1

    elemMask :: Word
    elemMask = (1 `shiftL` elemBits) - 1

    wordPat :: Word
    wordPat = wordMask .&. A.foldl' addToWord 0 patArr

    addToWord wd a = (wd `shiftL` elemBits) .|. fromIntegral (fromEnum a)

    nextAfterInit nextGen stepRes =
        case stepRes of
            FL.Partial s -> nextGen s
            FL.Done b ->
                SplitOnSuffixSeqYield b (SplitOnSuffixSeqReinit nextGen)

    {-# INLINE yieldReinit #-}
    yieldReinit nextGen fs =
        initial >>= skip . SplitOnSuffixSeqYield fs . nextAfterInit nextGen

    -- For single element pattern case
    {-# INLINE processYieldSingle #-}
    processYieldSingle pat x s fs = do
        let jump c = SplitOnSuffixSeqSingleInit c s pat
        if pat == x
        then do
            r <- if withSep then fstep fs x else return $ FL.Partial fs
            b1 <-
                case r of
                    FL.Partial fs1 -> final fs1
                    FL.Done b -> return b
            yieldReinit jump b1
        else do
            r <- fstep fs x
            case r of
                FL.Partial fs1 -> skip $ SplitOnSuffixSeqSingle fs1 s pat
                FL.Done b -> yieldReinit jump b

    -- For Rabin-Karp search
    k = 2891336453 :: Word32
    coeff = k ^ patLen

    addCksum cksum a = cksum * k + fromIntegral (fromEnum a)

    deltaCksum cksum old new =
        addCksum cksum new - coeff * fromIntegral (fromEnum old)

    -- XXX shall we use a random starting hash or 1 instead of 0?
    patHash = A.foldl' addCksum 0 patArr

    skip = return . Skip

    {-# INLINE_LATE stepOuter #-}
    stepOuter _ SplitOnSuffixSeqInit = do
        res <- initial
        case res of
            FL.Partial fs
                | patLen == 0 ->
                    skip $ SplitOnSuffixSeqEmpty fs state
                | patLen == 1 -> do
                    pat <- liftIO $ A.unsafeGetIndexIO 0 patArr
                    skip $ SplitOnSuffixSeqSingleInit fs state pat
                | SIZE_OF(a) * patLen <= sizeOf (Proxy :: Proxy Word) ->
                    skip $ SplitOnSuffixSeqWordInit fs state
                | otherwise -> do
                    (MutArray mba _ _ _) :: MutArray a <-
                        liftIO $ MutArray.emptyOf patLen
                    skip $ SplitOnSuffixSeqKRInit fs state mba
            FL.Done fb -> skip $ SplitOnSuffixSeqYield fb SplitOnSuffixSeqInit

    stepOuter _ (SplitOnSuffixSeqYield x next) = return $ Yield x next

    ---------------------------
    -- Reinit
    ---------------------------

    stepOuter _ (SplitOnSuffixSeqReinit nextGen) =
        initial >>= skip . nextAfterInit nextGen

    ---------------------------
    -- Empty pattern
    ---------------------------

    stepOuter gst (SplitOnSuffixSeqEmpty acc st) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> do
                let jump c = SplitOnSuffixSeqEmpty c s
                r <- fstep acc x
                b1 <-
                    case r of
                        FL.Partial fs -> final fs
                        FL.Done b -> return b
                yieldReinit jump b1
            Skip s -> skip (SplitOnSuffixSeqEmpty acc s)
            Stop -> final acc >> return Stop

    -----------------
    -- Done
    -----------------

    stepOuter _ SplitOnSuffixSeqDone = return Stop

    -----------------
    -- Single Pattern
    -----------------

    stepOuter gst (SplitOnSuffixSeqSingleInit fs st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> processYieldSingle pat x s fs
            Skip s -> skip $ SplitOnSuffixSeqSingleInit fs s pat
            Stop -> final fs >> return Stop

    stepOuter gst (SplitOnSuffixSeqSingle fs st pat) = do
        res <- step (adaptState gst) st
        case res of
            Yield x s -> processYieldSingle pat x s fs
            Skip s -> skip $ SplitOnSuffixSeqSingle fs s pat
            Stop -> do
                r <- final fs
                skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone

    ---------------------------
    -- Short Pattern - Shift Or
    ---------------------------

    stepOuter _ (SplitOnSuffixSeqWordDone 0 fs _) = do
        r <- final fs
        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
    stepOuter _ (SplitOnSuffixSeqWordDone n fs wrd) = do
        let old = elemMask .&. (wrd `shiftR` (elemBits * (n - 1)))
        r <- fstep fs (toEnum $ fromIntegral old)
        case r of
            FL.Partial fs1 -> skip $ SplitOnSuffixSeqWordDone (n - 1) fs1 wrd
            FL.Done b -> do
                let jump c = SplitOnSuffixSeqWordDone (n - 1) c wrd
                yieldReinit jump b

    stepOuter gst (SplitOnSuffixSeqWordInit fs0 st0) = do
        res <- step (adaptState gst) st0
        case res of
            Yield x s -> do
                let wrd = addToWord 0 x
                r <- if withSep then fstep fs0 x else return $ FL.Partial fs0
                case r of
                    FL.Partial fs1 -> go SPEC 1 wrd s fs1
                    FL.Done b -> do
                        let jump c = SplitOnSuffixSeqWordInit c s
                        yieldReinit jump b
            Skip s -> skip (SplitOnSuffixSeqWordInit fs0 s)
            Stop -> final fs0 >> return Stop

        where

        {-# INLINE go #-}
        go !_ !idx !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let jump c = SplitOnSuffixSeqWordInit c s
                    let wrd1 = addToWord wrd x
                    r <- if withSep then fstep fs x else return $ FL.Partial fs
                    case r of
                        FL.Partial fs1
                            | idx /= maxIndex ->
                                go SPEC (idx + 1) wrd1 s fs1
                            | wrd1 .&. wordMask /= wordPat ->
                                skip $ SplitOnSuffixSeqWordLoop wrd1 s fs1
                            | otherwise ->
                                final fs1 >>= yieldReinit jump
                        FL.Done b -> yieldReinit jump b
                Skip s -> go SPEC idx wrd s fs
                Stop ->
                    if withSep
                    then do
                        r <- final fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else skip $ SplitOnSuffixSeqWordDone idx fs wrd

    stepOuter gst (SplitOnSuffixSeqWordLoop wrd0 st0 fs0) =
        go SPEC wrd0 st0 fs0

        where

        {-# INLINE go #-}
        go !_ !wrd !st !fs = do
            res <- step (adaptState gst) st
            case res of
                Yield x s -> do
                    let jump c = SplitOnSuffixSeqWordInit c s
                        wrd1 = addToWord wrd x
                        old = (wordMask .&. wrd)
                                `shiftR` (elemBits * (patLen - 1))
                    r <-
                        if withSep
                        then fstep fs x
                        else fstep fs (toEnum $ fromIntegral old)
                    case r of
                        FL.Partial fs1 ->
                            if wrd1 .&. wordMask == wordPat
                            then final fs1 >>= yieldReinit jump
                            else go SPEC wrd1 s fs1
                        FL.Done b -> yieldReinit jump b
                Skip s -> go SPEC wrd s fs
                Stop ->
                    if withSep
                    then do
                        r <- final fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else skip $ SplitOnSuffixSeqWordDone patLen fs wrd

    -------------------------------
    -- General Pattern - Karp Rabin
    -------------------------------

    stepOuter gst (SplitOnSuffixSeqKRInit fs st0 mba) = do
        res <- step (adaptState gst) st0
        case res of
            Yield x s -> do
                liftIO $ pokeAt 0 mba x
                r <- if withSep then fstep fs x else return $ FL.Partial fs
                case r of
                    FL.Partial fs1 ->
                        skip $ SplitOnSuffixSeqKRInit1 fs1 s mba
                    FL.Done b -> do
                        let jump c = SplitOnSuffixSeqKRInit c s mba
                        yieldReinit jump b
            Skip s -> skip $ SplitOnSuffixSeqKRInit fs s mba
            Stop -> final fs >> return Stop

    stepOuter gst (SplitOnSuffixSeqKRInit1 fs0 st0 mba) = do
        go SPEC (SIZE_OF(a)) st0 fs0

        where

        go !_ !offset st !fs = do
            res <- step (adaptState gst) st
            let arr :: Array a = Array
                        { arrContents = mba
                        , arrStart = 0
                        , arrEnd = patBytes
                        }
            case res of
                Yield x s -> do
                    liftIO $ pokeAt offset mba x
                    r <- if withSep then fstep fs x else return $ FL.Partial fs
                    let ringHash = A.foldl' addCksum 0 arr
                    case r of
                        FL.Partial fs1
                            | offset /= maxOffset ->
                                go SPEC (offset + SIZE_OF(a)) s fs1
                            | ringHash == patHash ->
                                skip $ SplitOnSuffixSeqKRCheck fs1 s mba 0
                            | otherwise ->
                                skip $ SplitOnSuffixSeqKRLoop
                                    fs1 s mba 0 ringHash
                        FL.Done b -> do
                            let jump c = SplitOnSuffixSeqKRInit c s mba
                            yieldReinit jump b
                Skip s -> go SPEC offset s fs
                Stop -> do
                    -- do not issue a blank segment when we end at pattern
                    if offset == maxOffset && A.byteEq arr patArr
                    then final fs >> return Stop
                    else if withSep
                    then do
                        r <- final fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else do
                        let rb = RingArray
                                { ringContents = mba
                                , ringSize = offset
                                , ringHead = 0
                                }
                         in skip $ SplitOnSuffixSeqKRDone offset fs rb

    stepOuter gst (SplitOnSuffixSeqKRLoop fs0 st0 mba rh0 cksum0) =
        go SPEC fs0 st0 rh0 cksum0

        where

        go !_ !fs !st !rh !cksum = do
            res <- step (adaptState gst) st
            let rb = RingArray
                    { ringContents = mba
                    , ringSize = patBytes
                    , ringHead = rh
                    }
            case res of
                Yield x s -> do
                    (rb1, old) <- liftIO (RB.replace rb x)
                    let cksum1 = deltaCksum cksum old x
                    let rh1 = ringHead rb1
                    r <- if withSep then fstep fs x else fstep fs old
                    case r of
                        FL.Partial fs1 ->
                            if cksum1 /= patHash
                            then go SPEC fs1 s rh1 cksum1
                            else skip $ SplitOnSuffixSeqKRCheck fs1 s mba rh1
                        FL.Done b -> do
                            let jump c = SplitOnSuffixSeqKRInit c s mba
                            yieldReinit jump b
                Skip s -> go SPEC fs s rh cksum
                Stop -> do
                    if withSep
                    then do
                        r <- final fs
                        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
                    else skip $ SplitOnSuffixSeqKRDone patBytes fs rb

    stepOuter _ (SplitOnSuffixSeqKRCheck fs st mba rh) = do
        let rb = RingArray
                    { ringContents = mba
                    , ringSize = patBytes
                    , ringHead = rh
                    }
        matches <- liftIO $ RB.eqArray rb patArr
        if matches
        then do
            r <- final fs
            let jump c = SplitOnSuffixSeqKRInit c st mba
            yieldReinit jump r
        else skip $ SplitOnSuffixSeqKRLoop fs st mba rh patHash

    stepOuter _ (SplitOnSuffixSeqKRDone 0 fs _) = do
        r <- final fs
        skip $ SplitOnSuffixSeqYield r SplitOnSuffixSeqDone
    stepOuter _ (SplitOnSuffixSeqKRDone len fs rb) = do
        assert (len >= 0) (return ())
        old <- RB.unsafeGetHead rb
        let rb1 = RB.moveForward rb
        r <- fstep fs old
        case r of
            FL.Partial fs1 ->
                skip $ SplitOnSuffixSeqKRDone (len - SIZE_OF(a)) fs1 rb1
            FL.Done b -> do
                let jump c = SplitOnSuffixSeqKRDone (len - SIZE_OF(a)) c rb1
                yieldReinit jump b

-- | Parses a sequence of tokens suffixed by a separator e.g. @a;b;c;@ is
-- parsed as @a;@, @b;@, @c;@. If the pattern is empty the input stream is
-- returned as it is.
--
-- Equivalent to the following:
--
-- >>> splitEndBySeq pat f = Stream.foldMany (Fold.takeEndBySeq pat f)
--
-- Uses Rabin-Karp algorithm for substring search.
--
{-# INLINE_NORMAL splitEndBySeq #-}
splitEndBySeq
    :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitEndBySeq = splitOnSuffixSeq True

-- | Like 'splitEndBySeq' but drops the separators and returns only the tokens.
--
-- Equivalent to the following:
--
-- >>> splitEndBySeq_ pat f = Stream.foldMany (Fold.takeEndBySeq_ pat f)
--
-- Uses Rabin-Karp algorithm for substring search.
--
{-# INLINE_NORMAL splitEndBySeq_ #-}
splitEndBySeq_
    :: forall m a b. (MonadIO m, Unbox a, Enum a, Eq a)
    => Array a
    -> Fold m a b
    -> Stream m a
    -> Stream m b
splitEndBySeq_ = splitOnSuffixSeq False

-- Implement this as a fold or a parser instead.
-- This can be implemented easily using Rabin Karp

-- | Split post any one of the given patterns.
--
-- /Unimplemented/
{-# INLINE splitEndBySeqOneOf #-}
splitEndBySeqOneOf :: -- (Monad m, Unboxed a, Integral a) =>
    [Array a] -> Fold m a b -> Stream m a -> Stream m b
splitEndBySeqOneOf _subseq _f _m = undefined

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
{-# INLINE splitBeginBy_ #-}
splitBeginBy_ :: -- (MonadCatch m) =>
    (a -> Bool) -> Fold m a b -> Stream m a -> Stream m b
splitBeginBy_ _predicate _f = undefined
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
{-# INLINE splitSepBySeqOneOf #-}
splitSepBySeqOneOf :: -- (Monad m, Unboxed a, Integral a) =>
    [Array a] -> Fold m a b -> Stream m a -> Stream m b
splitSepBySeqOneOf _subseq _f _m =
    undefined -- D.fromStreamD $ D.splitOnAny f subseq (D.toStreamD m)

------------------------------------------------------------------------------
-- Nested Container Transformation
------------------------------------------------------------------------------

{-# ANN type SplitState Fuse #-}
data SplitState s arr
    = SplitInitial s
    | SplitBuffering s arr
    | SplitSplitting s arr
    | SplitYielding arr (SplitState s arr)
    | SplitFinishing

-- XXX An alternative approach would be to use a partial fold (Fold m a b) to
-- split using a splitBy like combinator. The Fold would consume upto the
-- separator and return any leftover which can then be fed to the next fold.
--
-- We can revisit this once we have partial folds/parsers.
--
-- | Performs infix separator style splitting.
{-# INLINE_NORMAL splitInnerBy #-}
splitInnerBy
    :: Monad m
    => (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> Stream m (f a)
    -> Stream m (f a)
splitInnerBy splitter joiner (Stream step1 state1) =
    Stream step (SplitInitial state1)

    where

    {-# INLINE_LATE step #-}
    step gst (SplitInitial st) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s x1)
                    Just x2 -> Skip (SplitYielding x1 (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitInitial s)
            Stop -> return Stop

    step gst (SplitBuffering st buf) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                buf' <- joiner buf x1
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s buf')
                    Just x2 -> Skip (SplitYielding buf' (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitBuffering s buf)
            Stop -> return $ Skip (SplitYielding buf SplitFinishing)

    step _ (SplitSplitting st buf) = do
        (x1, mx2) <- splitter buf
        return $ case mx2 of
                Nothing -> Skip $ SplitBuffering st x1
                Just x2 -> Skip $ SplitYielding x1 (SplitSplitting st x2)

    step _ (SplitYielding x next) = return $ Yield x next
    step _ SplitFinishing = return Stop

-- | Performs infix separator style splitting.
{-# INLINE_NORMAL splitInnerBySuffix #-}
splitInnerBySuffix
    :: Monad m
    => (f a -> Bool)                  -- isEmpty?
    -> (f a -> m (f a, Maybe (f a)))  -- splitter
    -> (f a -> f a -> m (f a))        -- joiner
    -> Stream m (f a)
    -> Stream m (f a)
splitInnerBySuffix isEmpty splitter joiner (Stream step1 state1) =
    Stream step (SplitInitial state1)

    where

    {-# INLINE_LATE step #-}
    step gst (SplitInitial st) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s x1)
                    Just x2 -> Skip (SplitYielding x1 (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitInitial s)
            Stop -> return Stop

    step gst (SplitBuffering st buf) = do
        r <- step1 gst st
        case r of
            Yield x s -> do
                (x1, mx2) <- splitter x
                buf' <- joiner buf x1
                return $ case mx2 of
                    Nothing -> Skip (SplitBuffering s buf')
                    Just x2 -> Skip (SplitYielding buf' (SplitSplitting s x2))
            Skip s -> return $ Skip (SplitBuffering s buf)
            Stop ->
                return $
                    if isEmpty buf
                    then Stop
                    else Skip (SplitYielding buf SplitFinishing)

    step _ (SplitSplitting st buf) = do
        (x1, mx2) <- splitter buf
        return $ case mx2 of
                Nothing -> Skip $ SplitBuffering st x1
                Just x2 -> Skip $ SplitYielding x1 (SplitSplitting st x2)

    step _ (SplitYielding x next) = return $ Yield x next
    step _ SplitFinishing = return Stop

------------------------------------------------------------------------------
-- Trimming
------------------------------------------------------------------------------

-- | Drop prefix from the input stream if present.
--
-- Space: @O(1)@
--
-- See also stripPrefix.
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
-- See also stripInfix.
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
-- See also stripSuffix.
--
-- /Unimplemented/
{-# INLINE dropSuffix #-}
dropSuffix ::
    -- (Monad m, Eq a) =>
    Stream m a -> Stream m a -> Stream m a
dropSuffix = error "Not implemented yet!"
