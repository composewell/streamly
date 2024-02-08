-- |
-- Module      : Streamly.Internal.Data.Scan
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This is an experimental module.
--
-- Scans represent stateful transformations with filtering and termination.
-- Scans are simple pipes that have an input end and an output end. These pipes
-- can be connected in series or in parallel. A Scan can be used on the output
-- of a stream or on the input of a fold. We can split a pipe into multiple
-- pipes which apply different stateful transformations and then merge the
-- transformed streams back into a single stream.

-- Design notes:
--
-- Till now we have been using folds as terminal consumers as well as for
-- scanning. Folds can split the input stream so that it can be folded by
-- multiple different folds but we cannot merge it back. An arbitrary
-- combination of series and parallel transformations cannot be composed using
-- just streams and folds. Also there are cases where the terminal consumer
-- nature of folds conflicts with the transformation nature of scanning e.g.
-- parEval does not implement "extract", similarly, splitWith does not
-- implement extract. Due to these issues we need to remove "extract" from
-- folds and move the scanning functionality into a separate "Scan"
-- abstraction.
--
-- A Stream:
--
-- data Step s a =
--       Yield a s
--     | Skip s
--     | Stop
-- initial :: s
-- step :: s -> m (Step s a)
-- final :: s -> m (Step s a)
--
-- If the fold stops before the stream, we may need a function "final s" to
-- drain or finalize the stream.
--
-- The Skip constructor allows introducing states without producing output.
-- Skip enables filtering. Even though filtering can be done by composing a
-- "Scan" with the stream, "Skip' is required to compose such filtering scans
-- with the stream i.e. for Stream + Scan = Stream. If we do not allow such
-- composition then we can avoid "Skip" in stream. In that case instead of
-- joining Stream + Fold in a pipeline we will always have to join Stream +
-- Scan + Fold.
--
-- A Fold:
--
-- data Step s b =
--       Partial s
--     | Repeat s
--     | Done b
-- initial :: m (Step s b)
-- step :: s -> a -> m (Step s b)
-- final :: s -> m b
--
-- We need "Done b" in folds instead of "Stop" to know the termination without
-- having to push the next input. Other alternative is to use a "Done s" and
-- then use "final" to extract the final value of the fold. If the input stream
-- stops before the fold, we use "final" to extract the value of the fold.
--
-- When we return a "Done", the input may be consumed (e.g. take) or may remain
-- unconsumed (e.g. takewhile). An alternative design is to always return
-- "Done" only when we have an unconsumed input. Even if the fold knows that it
-- is done when it consumes an input it can wait for the next input to say
-- "Done". That way we can represent both take and takeWhile without having to
-- introduce one more constructor. Note that this is similar to the "Stop"
-- behavior on the stream side, where we don't know about the termination until
-- we try to pull the next element.
--
-- The Repeat constructor asks the driver to repeat the previous input. Repeat
-- enables expanding the output stream in a Scan. It is not needed in a fold,
-- but if we allow Scan + Fold = Fold type composition then we need it.
--
-- A Scan uses a step function which takes an input and produces an output. In
-- addition to Skipping output it can also repeat the input.
--
-- data Step s b =
--       Yield b s
--     | Skip s
--     | Stop
--     | SkipRep s  -- Skip output, repeat input
--     | YieldRep b s -- Emit output, repeat input
-- step :: s -> a -> m (Step s b)
--
-- A stream is a scan with () as the input element. Thus step can be simplified
-- and SkipRep and YieldRep can be removed. A fold is a scan which emits only
-- the last value of the stream and discards the intermediate values. Thus
-- Yield and YieldRep can be removed and we will need "Done b" to get the value
-- on stop.
--
-- The "final" in folds always returns a single output. Scans can also have the
-- "final" function return a stream instead of a single value e.g. to drain a
-- buffer. Therefore, folds are simplified version of Scans. Scans can
-- represent folds as well.
--
-- Thus folds can be implemented by using a single fold which discards
-- everything but the last element.
--
-- Compositions:
--
-- We can do contravariant compositions as well for scans e.g. we can unfold
-- the input of a scan or we can group the input of a scan using a fold. However,
-- we can achieve everything using only covariant compositions i.e. by
-- unfolding or folding the output of a scan. Thus for intuitive composition we
-- will try to use only covariant compositions as long as possible.
--
-- The role of the Fold type will only be to split the stream like an unfold
-- expands it. Folds will also be useful for Applicative and Monadic
-- compositions to split and consume streams. If we have an existing fold and
-- we want to extend it then we will have to do contravariant composition. The
-- alternative is to not have any folds but only scans and when we need to
-- convert the scans into folds using a combinator.
--
-- Stream + Scan = Stream
-- Scan + Scan = Scan
-- Scan + Fold = Fold
--
-- Finalization in composed pipelines:
--
-- A driver is pulling from a stream and pushing to a fold. If the stream stops
-- it needs to finalize the fold side, and if the fold stops it needs to
-- finalize the stream side.
--
-- Finalization can do two things. One, release any allocated resources. Two,
-- drain any buffered streams.
--
-- The direction of finalization depends on whether it is stream side or fold
-- side. Data flow in a pipeline looks like s1 -> s2 -> s3 -> driver -> f1 ->
-- f2 -> f3. Driver is pulling from stream s3 and pushing to fold f1. On stream
-- side, finalization starts from the output end, the driver calls the "final"
-- of s3 which calls the final of s2 and so on. On the fold side the sequence
-- is opposite, finalization starts from the input end, the driver calls f1, it
-- calls f2 and so on. Therefore, the finalization composition depends on
-- whether it is fold side or stream side.
--
-- We can use separate ScanL and ScanR types where ScanR compose in pull style
-- and ScanL compose in push style for different finalization flows. However,
-- we can compose in push style always and pull style finalization can be
-- handled properly at the point when we compose a stream and ScanL. Or
-- vice-versa.
--
-- "Stop" is needed for finalization:
--
-- The finalization routine may or may not return an output. If we have already
-- emitted an output on the last input then returning the same output again in
-- "final" routine would emit a duplicate output. But in some cases we may have
-- to return an output stream in "final" e.g. when draining a buffer. To be
-- able to represent no output in "final" we need the "Stop" constructor.
--
-- Scans vs Folds and Unfolds:
--
-- a) Write an Unfold to convert an element of stream into a stream in two or
-- more different ways. This could be an alternative to the tee composition of
-- scans, but state sharing is limited to one instance of Unfold. An Unfold tee
-- combinator can be written to pass an input through two different Unfolds.
--
-- b) use the tee combinator from Fold, use the fold on each element of the
-- stream, then unfold the resulting stream to flatten it. The state sharing is
-- limited to the fold.
--
-- c) Create a stream tee combinator taking two folds and passing the input
-- through them. State sharing is limited to the fold. With that we can run
-- multiple folds in parallel using parEval. But that won't be efficient as a
-- Channel would be created on every split.
--
-- Terminology of Folds vs Scans:
--
-- length is a fold but count is a scan.
-- last is a fold but latest is a scan.
-- head is a fold, oldest is a scan.
-- sum is a fold but add is a scan.
--
module Streamly.Internal.Data.Scan
    (
    -- * Type
      Scan (..)
    , Step (..)

    -- * Primitive Scans
    , identity
    , map
    , mapM
    , filter
    , filterM

    -- * Combinators
    , compose
    , teeMerge
    )
where

-- #include "inline.hs"
import Control.Category (Category(..))
import Data.Functor ((<&>))
import Fusion.Plugin.Types (Fuse(..))

import Prelude hiding (map, mapM, filter)

-- $setup
-- >>> :m
-- >>> :set -XFlexibleContexts
-- >>> import Control.Category
--
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Scan as Scan
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

-------------------------------------------------------------------------------
-- Scan
-------------------------------------------------------------------------------

-- XXX Instead of repeat input constructors we could use a skip input
-- constructor but then we also have to split the "step" function in "produce"
-- and "consume", when we skip input we call produce otherwise we call consume.
-- By using "repeat" we are basically using the consume step function for
-- produce mode as well. It may be cleaner to use separate consume and produce
-- functions. In that case we can also start the scan in produce mode even
-- without an input.
--
-- XXX The Fold "Partial s" is "Skip s". We can add SkipRep in folds. Also we
-- can use "Stop" and "final" instead of "Done". So we can have SkipRep, Skip
-- and Stop in folds. And Yield, Skip and Stop in streams.
--
-- XXX If we do not want to change Streams, we should use "Yield b s" instead
-- of "Yield s b". Though "Yield s b" is sometimes better when using curried
-- "Yield s". "Yield b" sounds better because the verb applies to "b".
--
-- XXX We could reduce the number of constructors by using Consume | Produce
-- wrapper around the state. But when fusion does not occur, it may be better
-- yo use a flat structure rather than nested to avoid more allocations. In a
-- flat structure the pointer tag from the Step constructor itself can identiy
-- any of the 5 constructors.
--
{-# ANN type Step Fuse #-}
data Step cs ps b =
      YieldC cs b -- Partial
    | SkipC cs -- Continue
    | Stop
    | YieldP ps b -- Yield
    | SkipP ps -- Skip

instance Functor (Step cs ps) where
    {-# INLINE fmap #-}
    fmap f (YieldC s b) = YieldC s (f b)
    fmap f (YieldP s b) = YieldP s (f b)
    fmap _ (SkipC s) = SkipC s
    fmap _ (SkipP s) = SkipP s
    fmap _ Stop = Stop

data Scan m a b =
    forall cs ps. Scan
        (cs -> a -> m (Step cs ps b))
        (ps -> m (Step cs ps b))
        cs

------------------------------------------------------------------------------
-- Functor: Mapping on the output
------------------------------------------------------------------------------

-- | 'fmap' maps a pure function on a scan output.
--
-- >>> Stream.toList $ Stream.runScan (fmap (+1) Scan.identity) $ Stream.fromList [1..5::Int]
-- [2,3,4,5,6]
--
instance Functor m => Functor (Scan m a) where
    {-# INLINE fmap #-}
    fmap f (Scan consume produce cinitial) =
        Scan consume1 produce1 cinitial

        where

        consume1 s b = fmap (fmap f) (consume s b)
        produce1 s = fmap (fmap f) (produce s)

-------------------------------------------------------------------------------
-- Category
-------------------------------------------------------------------------------

{-# ANN type ComposeConsume Fuse #-}
data ComposeConsume csL psL csR =
      ComposeConsume csL csR

{-# ANN type ComposeProduce Fuse #-}
data ComposeProduce csL psL csR psR =
      ComposeProduceR csL psR
    | ComposeProduceL psL csR
    | ComposeProduceLR psL psR

-- | Connect two scans in series. The second scan is the input end, and the
-- first scan is the output end.
--
-- >>> import Control.Category
-- >>> Stream.toList $ Stream.runScan (Scan.map (+1) >>> Scan.map (+1)) $ Stream.fromList [1..5::Int]
-- [3,4,5,6,7]
--
{-# INLINE compose #-}
compose :: Monad m => Scan m b c -> Scan m a b -> Scan m a c
compose
    (Scan consumeR produceR initialR)
    (Scan consumeL produceL initialL) =
        Scan consume produce (ComposeConsume initialL initialR)

    where

    {-# INLINE consumeLFeedR #-}
    consumeLFeedR csL csR bL = do
        rR <- consumeR csR bL
        return
            $ case rR of
                YieldC csR1 br -> YieldC (ComposeConsume csL csR1) br
                SkipC csR1 -> SkipC (ComposeConsume csL csR1)
                Stop -> Stop
                YieldP psR br -> YieldP (ComposeProduceR csL psR) br
                SkipP psR -> SkipP (ComposeProduceR csL psR)

    {-# INLINE produceLFeedR #-}
    produceLFeedR psL csR bL = do
        rR <- consumeR csR bL
        return
            $ case rR of
                YieldC csR1 br -> YieldP (ComposeProduceL psL csR1) br
                SkipC csR1 -> SkipP (ComposeProduceL psL csR1)
                Stop -> Stop
                YieldP psR br -> YieldP (ComposeProduceLR psL psR) br
                SkipP psR -> SkipP (ComposeProduceLR psL psR)

    consume (ComposeConsume csL csR) x = do
        rL <- consumeL csL x
        case rL of
            YieldC csL1 bL ->
                -- XXX Use SkipC instead? Flat may be better for fusion.
                consumeLFeedR csL1 csR bL
            SkipC csL1 -> return $ SkipC (ComposeConsume csL1 csR)
            Stop -> return Stop
            YieldP psL bL ->
                -- XXX Use SkipC instead?
                produceLFeedR psL csR bL
            SkipP psL -> return $ SkipP (ComposeProduceL psL csR)

    produce (ComposeProduceL psL csR) = do
        rL <- produceL psL
        case rL of
            YieldC csL bL ->
                -- XXX Use SkipC instead?
                consumeLFeedR csL csR bL
            SkipC csL -> return $ SkipC (ComposeConsume csL csR)
            Stop -> return Stop
            YieldP psL1 bL ->
                -- XXX Use SkipC instead?
                produceLFeedR psL1 csR bL
            SkipP psL1 -> return $ SkipP (ComposeProduceL psL1 csR)

    produce (ComposeProduceR csL psR) = do
        rR <- produceR psR
        return
            $ case rR of
                YieldC csR1 br -> YieldC (ComposeConsume csL csR1) br
                SkipC csR1 -> SkipC (ComposeConsume csL csR1)
                Stop -> Stop
                YieldP psR1 br -> YieldP (ComposeProduceR csL psR1) br
                SkipP psR1 -> SkipP (ComposeProduceR csL psR1)

    produce (ComposeProduceLR psL psR) = do
        rR <- produceR psR
        return
            $ case rR of
                YieldC csR1 br -> YieldP (ComposeProduceL psL csR1) br
                SkipC csR1 -> SkipP (ComposeProduceL psL csR1)
                Stop -> Stop
                YieldP psR1 br -> YieldP (ComposeProduceLR psL psR1) br
                SkipP psR1 -> SkipP (ComposeProduceLR psL psR1)

-- | A scan representing mapping of a monadic action.
--
-- >>> Stream.toList $ Stream.runScan (Scan.mapM print) $ Stream.fromList [1..5::Int]
-- 1
-- 2
-- 3
-- 4
-- 5
-- [(),(),(),(),()]
--
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Scan m a b
mapM f = Scan (\() a -> f a <&> YieldC ()) undefined ()

-- | A scan representing mapping of a pure function.
--
-- >>> Stream.toList $ Stream.runScan (Scan.map (+1)) $ Stream.fromList [1..5::Int]
-- [2,3,4,5,6]
--
{-# INLINE map #-}
map :: Monad m => (a -> b) -> Scan m a b
map f = mapM (return Prelude.. f)

{- HLINT ignore "Redundant map" -}

-- | An identity scan producing the same output as input.
--
-- >>> Stream.toList $ Stream.runScan (Scan.identity) $ Stream.fromList [1..5::Int]
-- [1,2,3,4,5]
--
{-# INLINE identity #-}
identity :: Monad m => Scan m a a
identity = map Prelude.id

instance Monad m => Category (Scan m) where
    id = identity

    (.) = compose

-------------------------------------------------------------------------------
-- Scans
-------------------------------------------------------------------------------

-- | A filtering scan.
{-# INLINE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Scan m a a
filterM f = Scan (\() a -> f a >>= g a) undefined ()

    where

    {-# INLINE g #-}
    g a b =
        return
            $ if b
              then YieldC () a
              else SkipC ()

-- | A filtering scan.
--
-- >>> Stream.toList $ Stream.runScan (Scan.filter odd) $ Stream.fromList [1..5::Int]
-- [1,3,5]
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Scan m a a
filter f = filterM (return Prelude.. f)

{-
{-# ANN type TeeMergeState Fuse #-}
data TeeMergeState sL sR
    = TeeMergeLeft !sL !sR
    | TeeMergeRight !sL !sR
    | TeeMergeLeftOnly !sL
    | TeeMergeRightOnly !sR
-}

{-# ANN type TeeMergeConsume Fuse #-}
data TeeMergeConsume csL csR
    = TeeMergeConsume !csL !csR
    | TeeMergeConsumeOnlyL !csL
    | TeeMergeConsumeOnlyR !csR

{-# ANN type TeeMergeProduce Fuse #-}
data TeeMergeProduce csL csR psL psR x
    = TeeMergeProduce !csL !csR x
    | TeeMergeProduceL !psL !csR x
    | TeeMergeProduceR !csL !psR
    | TeeMergeProduceOnlyL !psL
    | TeeMergeProduceOnlyR !psR

-- | Connect two scans in parallel. Distribute the input across two scans and
-- merge their outputs as soon as they become available. Note that a scan may
-- not generate output on each input, it might filter it.
--
-- >>> Stream.toList $ Stream.runScan (Scan.teeMerge Scan.identity (Scan.map (\x -> x * x))) $ Stream.fromList [1..5::Int]
-- [1,1,2,4,3,9,4,16,5,25]
--
{-# INLINE teeMerge #-}
teeMerge :: Monad m => Scan m a b -> Scan m a b -> Scan m a b
teeMerge (Scan consumeL produceL initialL) (Scan consumeR produceR initialR) =
    Scan consume produce (TeeMergeConsume initialL initialR)

    where

    {-# INLINE feedRightOnly #-}
    feedRightOnly csR a = do
        resR <- consumeR csR a
        return
            $ case resR of
                  YieldC cs b -> YieldC (TeeMergeConsumeOnlyR cs) b
                  SkipC cs -> SkipC (TeeMergeConsumeOnlyR cs)
                  Stop -> Stop
                  YieldP ps b -> YieldP (TeeMergeProduceOnlyR ps) b
                  SkipP ps -> SkipP (TeeMergeProduceOnlyR ps)

    consume (TeeMergeConsume csL csR) a = do
        resL <- consumeL csL a
        case resL of
              YieldC cs b -> return $ YieldP (TeeMergeProduce cs csR a) b
              SkipC cs -> return $ SkipP (TeeMergeProduce cs csR a)
              Stop ->
                -- XXX Skip to a state instead?
                feedRightOnly csR a
              YieldP ps b -> return $ YieldP (TeeMergeProduceL ps csR a) b
              SkipP ps -> return $ SkipP (TeeMergeProduceL ps csR a)

    consume (TeeMergeConsumeOnlyL csL) a = do
        resL <- consumeL csL a
        return
            $ case resL of
                  YieldC cs b -> YieldC (TeeMergeConsumeOnlyL cs) b
                  SkipC cs -> SkipC (TeeMergeConsumeOnlyL cs)
                  Stop -> Stop
                  YieldP ps b -> YieldP (TeeMergeProduceOnlyL ps) b
                  SkipP ps -> SkipP (TeeMergeProduceOnlyL ps)
    consume (TeeMergeConsumeOnlyR csR) a = feedRightOnly csR a

    produce (TeeMergeProduce csL csR a) = do
        res <- consumeR csR a
        return
            $ case res of
                  YieldC cs b -> YieldC (TeeMergeConsume csL cs) b
                  SkipC cs -> SkipC (TeeMergeConsume csL cs)
                  Stop -> SkipC (TeeMergeConsumeOnlyL csL)
                  YieldP ps b -> YieldP (TeeMergeProduceR csL ps) b
                  SkipP ps -> SkipP (TeeMergeProduceR csL ps)

    produce (TeeMergeProduceL psL csR a) = do
        res <- produceL psL
        case res of
              YieldC cs b -> return $ YieldP (TeeMergeProduce cs csR a) b
              SkipC cs -> return $ SkipP (TeeMergeProduce cs csR a)
              Stop -> feedRightOnly csR a
              YieldP ps b -> return $ YieldP (TeeMergeProduceL ps csR a) b
              SkipP ps -> return $ SkipP (TeeMergeProduceL ps csR a)

    produce (TeeMergeProduceR csL psR) = do
        res <- produceR psR
        return $ case res of
              YieldC cs b -> YieldC (TeeMergeConsume csL cs) b
              SkipC cs -> SkipC (TeeMergeConsume csL cs)
              Stop -> SkipC (TeeMergeConsumeOnlyL csL)
              YieldP ps b -> YieldP (TeeMergeProduceR csL ps) b
              SkipP ps -> SkipP (TeeMergeProduceR csL ps)

    produce (TeeMergeProduceOnlyL psL) = do
        resL <- produceL psL
        return
            $ case resL of
                  YieldC cs b -> YieldC (TeeMergeConsumeOnlyL cs) b
                  SkipC cs -> SkipC (TeeMergeConsumeOnlyL cs)
                  Stop -> Stop
                  YieldP ps b -> YieldP (TeeMergeProduceOnlyL ps) b
                  SkipP ps -> SkipP (TeeMergeProduceOnlyL ps)

    produce (TeeMergeProduceOnlyR psR) = do
        resL <- produceR psR
        return
            $ case resL of
                  YieldC cs b -> YieldC (TeeMergeConsumeOnlyR cs) b
                  SkipC cs -> SkipC (TeeMergeConsumeOnlyR cs)
                  Stop -> Stop
                  YieldP ps b -> YieldP (TeeMergeProduceOnlyR ps) b
                  SkipP ps -> SkipP (TeeMergeProduceOnlyR ps)
