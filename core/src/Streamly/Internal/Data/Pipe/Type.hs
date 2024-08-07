-- |
-- Module      : Streamly.Internal.Data.Pipe.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Pipe.Type
    (
    -- * Type
      Step (..)
    , Pipe (..)

    -- * From folds
    , fromStream
    , fromScanr
    , fromFold
    , scanFold

    -- * Primitive Pipes
    , identity
    , map -- function?
    , mapM -- functionM?
    , filter
    , filterM

    -- * Combinators
    , compose
    , teeMerge
    -- , zipWith -- teeZip
    )
where

#include "inline.hs"
-- import Control.Arrow (Arrow(..))
import Control.Category (Category(..))
import Data.Functor ((<&>))
import Fusion.Plugin.Types (Fuse(..))
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Internal.Data.Scanr (Scanr(..))
import Streamly.Internal.Data.Stream.Type (Stream(..))
-- import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.SVar.Type (defState)

import qualified Prelude
import qualified Streamly.Internal.Data.Fold.Type as Fold
import qualified Streamly.Internal.Data.Stream.Type as Stream

import Prelude hiding (filter, zipWith, map, mapM, id, unzip, null)

-- $setup
-- >>> :m
-- >>> :set -XFlexibleContexts
-- >>> import Control.Category
--
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Pipe as Pipe
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Pipes
------------------------------------------------------------------------------

-- XXX If we do not want to change Streams, we should use "Yield b s" instead
-- of "Yield s b". Though "Yield s b" is sometimes better when using curried
-- "Yield s". "Yield b" sounds better because the verb applies to "b".
--
-- Note: We could reduce the number of constructors by using Consume | Produce
-- wrapper around the state. But when fusion does not occur, it may be better
-- to use a flat structure rather than nested to avoid more allocations. In a
-- flat structure the pointer tag from the Step constructor itself can identiy
-- any of the 5 constructors.
--
{-# ANN type Step Fuse #-}
data Step cs ps b =
      YieldC cs b -- ^ Yield and consume
    | SkipC cs -- ^ Skip and consume
    | Stop -- ^ when consuming, Stop means input remains unused
    -- Therefore, Stop should not be used when we are processing an input,
    -- instead use YieldP and then Stop.
    | YieldP ps b -- ^ Yield and produce
    | SkipP ps -- ^ Skip and produce

instance Functor (Step cs ps) where
    {-# INLINE fmap #-}
    fmap f (YieldC s b) = YieldC s (f b)
    fmap f (YieldP s b) = YieldP s (f b)
    fmap _ (SkipC s) = SkipC s
    fmap _ (SkipP s) = SkipP s
    fmap _ Stop = Stop

-- A pipe uses a consume function and a produce function. It can dynamically
-- switch from consume/fold mode to a produce/source mode.
--
-- We can upgrade a stream, fold or scan into a pipe. However, the simpler
-- types should be preferred because they can be more efficient and fuse
-- better.
--
-- The design of the Pipe type is such that the pipe decides whether it wants
-- to consume or produce, not the driver. The driver has to do what the pipe
-- dictates, if it can. The starting state of the pipe could either be
-- consuming or producing. Current implementation starts with a consuming
-- state. If the default state of the pipe is consumption state and there is no
-- input, the driver can call finalC :: cs -> m (Step cs ps b) to switch the
-- pipe to production state. The pipe can use SkipP to switch to production
-- state. If the default state of the pipe is producing state, the pipe can use
-- SkipC to switch to the consumer state. The driver can use finalP to switch
-- to consuming state.

-- | Represents a stateful transformation over an input stream of values of
-- type @a@ to outputs of type @b@ in 'Monad' @m@.
--
-- The constructor is @Pipe consume produce initial@.
data Pipe m a b =
    forall cs ps. Pipe
        (cs -> a -> m (Step cs ps b))
        (ps -> m (Step cs ps b))
     -- (cs -> m (Step cs ps b)) -- finalC
     -- (ps -> m (Step cs ps b)) -- finalP
        cs                       -- Either cs ps

------------------------------------------------------------------------------
-- Functor: Mapping on the output
------------------------------------------------------------------------------

-- | 'fmap' maps a pure function on a scan output.
--
-- >>> Stream.toList $ Stream.pipe (fmap (+1) Pipe.identity) $ Stream.fromList [1..5::Int]
-- [2,3,4,5,6]
--
instance Functor m => Functor (Pipe m a) where
    {-# INLINE_NORMAL fmap #-}
    fmap f (Pipe consume produce cinitial) =
        Pipe consume1 produce1 cinitial

        where

        {-# INLINE_LATE consume1 #-}
        consume1 s b = fmap (fmap f) (consume s b)
        {-# INLINE_LATE produce1 #-}
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

-- | Series composition. Compose two pipes such that the output of the second
-- pipe is attached to the input of the first pipe.
--
-- >>> Stream.toList $ Stream.pipe (Pipe.map (+1) >>> Pipe.map (+1)) $ Stream.fromList [1..5::Int]
-- [3,4,5,6,7]
--
{-# INLINE_NORMAL compose #-}
compose :: Monad m => Pipe m b c -> Pipe m a b -> Pipe m a c
compose
    (Pipe consumeR produceR initialR)
    (Pipe consumeL produceL initialL) =
        Pipe consume produce (ComposeConsume initialL initialR)

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

-- | A pipe representing mapping of a monadic action.
--
-- >>> Stream.toList $ Stream.pipe (Pipe.mapM print) $ Stream.fromList [1..5::Int]
-- 1
-- 2
-- 3
-- 4
-- 5
-- [(),(),(),(),()]
--
{-# INLINE mapM #-}
mapM :: Monad m => (a -> m b) -> Pipe m a b
mapM f = Pipe (\() a -> f a <&> YieldC ()) undefined ()

-- | A pipe representing mapping of a pure function.
--
-- >>> Stream.toList $ Stream.pipe (Pipe.map (+1)) $ Stream.fromList [1..5::Int]
-- [2,3,4,5,6]
--
{-# INLINE map #-}
map :: Monad m => (a -> b) -> Pipe m a b
map f = mapM (return Prelude.. f)

{- HLINT ignore "Redundant map" -}

-- | An identity pipe producing the same output as input.
--
-- >>> identity = Pipe.map Prelude.id
--
-- >>> Stream.toList $ Stream.pipe (Pipe.identity) $ Stream.fromList [1..5::Int]
-- [1,2,3,4,5]
--
{-# INLINE identity #-}
identity :: Monad m => Pipe m a a
identity = map Prelude.id

-- | "." composes the pipes in series.
instance Monad m => Category (Pipe m) where
    {-# INLINE id #-}
    id = identity

    {-# INLINE (.) #-}
    (.) = compose

{-# ANN type TeeMergeConsume Fuse #-}
data TeeMergeConsume csL csR
    = TeeMergeConsume !csL !csR
    | TeeMergeConsumeOnlyL !csL
    | TeeMergeConsumeOnlyR !csR

{-# ANN type TeeMergeProduce Fuse #-}
data TeeMergeProduce csL csR psL psR x
    = TeeMergeProduce !csL !csR !x
    | TeeMergeProduceL !psL !csR !x
    | TeeMergeProduceR !csL !psR
    | TeeMergeProduceOnlyL !psL
    | TeeMergeProduceOnlyR !psR

-- | Parallel composition. Distribute the input across two pipes and merge
-- their outputs.
--
-- >>> Stream.toList $ Stream.pipe (Pipe.teeMerge Pipe.identity (Pipe.map (\x -> x * x))) $ Stream.fromList [1..5::Int]
-- [1,1,2,4,3,9,4,16,5,25]
--
{-# INLINE_NORMAL teeMerge #-}
teeMerge :: Monad m => Pipe m a b -> Pipe m a b -> Pipe m a b
teeMerge (Pipe consumeL produceL initialL) (Pipe consumeR produceR initialR) =
    Pipe consume produce (TeeMergeConsume initialL initialR)

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

    {-# INLINE_LATE consume #-}
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

    -- XXX Adding additional consume states causes 4x regression in
    -- All.Data.Stream/o-1-space.pipesX4.tee benchmark (mapM 4 times).
    -- Commenting these two states makes it 4x faster. Need to investigate why.
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

    {-# INLINE_LATE produce #-}
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

-- | '<>' composes the pipes in parallel.
instance Monad m => Semigroup (Pipe m a b) where
    {-# INLINE (<>) #-}
    (<>) = teeMerge

-------------------------------------------------------------------------------
-- Arrow
-------------------------------------------------------------------------------

{-
unzip :: Pipe m a x -> Pipe m b y -> Pipe m (a, b) (x, y)
unzip = undefined

-- XXX move this to a separate module
data Deque a = Deque [a] [a]

{-# INLINE null #-}
null :: Deque a -> Bool
null (Deque [] []) = True
null _ = False

{-# INLINE snoc #-}
snoc :: a -> Deque a -> Deque a
snoc a (Deque snocList consList) = Deque (a : snocList) consList

{-# INLINE uncons #-}
uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque snocList consList) =
  case consList of
    h : t -> Just (h, Deque snocList t)
    _ ->
      case Prelude.reverse snocList of
        h : t -> Just (h, Deque [] t)
        _ -> Nothing

-- XXX This is old code retained for reference until rewritten.

-- | The composed pipe distributes the input to both the constituent pipes and
-- zips the output of the two using a supplied zipping function.
--
-- @since 0.7.0
{-# INLINE_NORMAL zipWith #-}
zipWith :: Monad m => (a -> b -> c) -> Pipe m i a -> Pipe m i b -> Pipe m i c
zipWith f (Pipe consumeL produceL stateL) (Pipe consumeR produceR stateR) =
                    Pipe consume produce state
        where

        -- Left state means we need to consume input from the source. A Right
        -- state means we either have buffered input or we are in generation
        -- mode so we do not need input from source in either case.
        --
        state = Tuple' (Consume stateL, Nothing, Nothing)
                       (Consume stateR, Nothing, Nothing)

        -- XXX for heavy buffering we need to have the (ring) buffer in pinned
        -- memory using the Storable instance.
        {-# INLINE_LATE consume #-}
        consume (Tuple' (sL, resL, lq) (sR, resR, rq)) a = do
            s1 <- drive sL resL lq consumeL produceL a
            s2 <- drive sR resR rq consumeR produceR a
            yieldOutput s1 s2

            where

            {-# INLINE drive #-}
            drive st res queue fConsume fProduce val =
                case res of
                    Nothing -> goConsume st queue val fConsume fProduce
                    Just x -> return $
                        case queue of
                            Nothing -> (st, Just x, Just $ Deque [val] [])
                            Just q  -> (st, Just x, Just $ snoc val q)

            {-# INLINE goConsume #-}
            goConsume stt queue val fConsume stp2 =
                case stt of
                    Consume st ->
                        case queue of
                            Nothing -> do
                                r <- fConsume st val
                                return $ case r of
                                    Yield x s  -> (s, Just x, Nothing)
                                    Continue s -> (s, Nothing, Nothing)
                            Just queue' ->
                                case uncons queue' of
                                    Just (v, q) -> do
                                        r <- fConsume st v
                                        let q' = snoc val q
                                        return $ case r of
                                            Yield x s  -> (s, Just x, Just q')
                                            Continue s -> (s, Nothing, Just q')
                                    Nothing -> undefined -- never occurs
                    Produce st -> do
                        r <- stp2 st
                        return $ case r of
                            Yield x s  -> (s, Just x, queue)
                            Continue s -> (s, Nothing, queue)

        {-# INLINE_LATE produce #-}
        produce (Tuple' (sL, resL, lq) (sR, resR, rq)) = do
            s1 <- drive sL resL lq consumeL produceL
            s2 <- drive sR resR rq consumeR produceR
            yieldOutput s1 s2

            where

            {-# INLINE drive #-}
            drive stt res q fConsume fProduce =
                case res of
                    Nothing -> goProduce stt q fConsume fProduce
                    Just x -> return (stt, Just x, q)

            {-# INLINE goProduce #-}
            goProduce stt queue fConsume fProduce =
                case stt of
                    Consume st ->
                        case queue of
                            -- See yieldOutput. We enter produce mode only when
                            -- each pipe is either in Produce state or the
                            -- queue is non-empty. So this case cannot occur.
                            Nothing -> undefined
                            Just queue' ->
                                case uncons queue' of
                                    Just (v, q) -> do
                                        r <- fConsume st v
                                        -- We provide a guarantee that if the
                                        -- queue is "Just" it is always
                                        -- non-empty. yieldOutput and goConsume
                                        -- depend on it.
                                        let q' = if null q
                                                 then Nothing
                                                 else Just q
                                        return $ case r of
                                            Yield x s  -> (s, Just x, q')
                                            Continue s -> (s, Nothing, q')
                                    Nothing -> return (stt, Nothing, Nothing)
                    Produce st -> do
                        r <- fProduce st
                        return $ case r of
                            Yield x s  -> (s, Just x, queue)
                            Continue s -> (s, Nothing, queue)

        {-# INLINE yieldOutput #-}
        yieldOutput s1@(sL', resL', lq') s2@(sR', resR', rq') = return $
            -- switch to produce mode if we do not need input
            if (isProduce sL' || isJust lq') && (isProduce sR' || isJust rq')
            then
                case (resL', resR') of
                    (Just xL, Just xR) ->
                        Yield (f xL xR) (Produce (Tuple' (clear s1) (clear s2)))
                    _ -> Continue (Produce (Tuple' s1 s2))
            else
                case (resL', resR') of
                    (Just xL, Just xR) ->
                        Yield (f xL xR) (Consume (Tuple' (clear s1) (clear s2)))
                    _ -> Continue (Consume (Tuple' s1 s2))
            where clear (s, _, q) = (s, Nothing, q)

instance Monad m => Applicative (Pipe m a) where
    {-# INLINE pure #-}
    pure b = Pipe (\_ _ -> pure $ Yield b (Consume ())) undefined ()

    (<*>) = zipWith id

instance Monad m => Arrow (Pipe m) where
    {-# INLINE arr #-}
    arr = map

    {-# INLINE (***) #-}
    (***) = unzip

    {-# INLINE (&&&) #-}
    -- (&&&) = zipWith (,)
    (&&&) = undefined
-}

-------------------------------------------------------------------------------
-- Primitive pipes
-------------------------------------------------------------------------------

-- | A filtering pipe using a monadic predicate.
{-# INLINE filterM #-}
filterM :: Monad m => (a -> m Bool) -> Pipe m a a
filterM f = Pipe (\() a -> f a >>= g a) undefined ()

    where

    {-# INLINE g #-}
    g a b =
        return
            $ if b
              then YieldC () a
              else SkipC ()

-- | A filtering pipe using a pure predicate.
--
-- >>> Stream.toList $ Stream.pipe (Pipe.filter odd) $ Stream.fromList [1..5::Int]
-- [1,3,5]
--
{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Pipe m a a
filter f = filterM (return Prelude.. f)

-------------------------------------------------------------------------------
-- Convert folds to pipes
-------------------------------------------------------------------------------

-- Note when we have a separate Scan type then we can remove extract from
-- Folds. Then folds can only be used for foldMany or many and not for
-- scanning. This combinator has to be removed then.

-- XXX The way filter is implemented in Folds is that it discards the input and
-- on "extract" it will return the previous accumulator value only. Thus the
-- accumulator may repeat in the output stream when filter is used. Ideally the
-- output stream should not have a value corresponding to the filtered value.
-- With "Continue s" and "Partial s b" instead of using "extract" we can do
-- that.

{-# ANN type FromFoldConsume Fuse #-}
data FromFoldConsume s x = FoldConsumeInit | FoldConsumeGo s

{-# ANN type FromFoldProduce Fuse #-}
data FromFoldProduce s x = FoldProduceInit s x | FoldProduceStop

-- XXX This should be removed once we remove "extract" from folds.

-- | Pipes do not support finalization yet. This does not finalize the fold
-- when the stream stops before the fold terminates. So cannot be used on folds
-- that require finalization.
--
-- >>> Stream.toList $ Stream.pipe (Pipe.scanFold Fold.sum) $ Stream.fromList [1..5::Int]
-- [1,3,6,10,15]
--
{-# INLINE scanFold #-}
scanFold :: Monad m => Fold m a b -> Pipe m a b
scanFold (Fold fstep finitial fextract _) =
    Pipe consume produce FoldConsumeInit

    where

    -- XXX make the initial state Either type and start in produce mode
    consume FoldConsumeInit x = do
        r <- finitial
        return $ case r of
            Fold.Partial s -> SkipP (FoldProduceInit s x)
            Fold.Done b -> YieldP FoldProduceStop b

    consume (FoldConsumeGo st) a = do
        r <- fstep st a
        case r of
            Fold.Partial s -> do
                b <- fextract s
                return $ YieldC (FoldConsumeGo s) b
            Fold.Done b -> return $ YieldP FoldProduceStop b

    produce (FoldProduceInit st x) = consume (FoldConsumeGo st) x
    produce FoldProduceStop = return Stop

-- | Create a singleton pipe from a fold.
--
-- Pipes do not support finalization yet. This does not finalize the fold
-- when the stream stops before the fold terminates. So cannot be used on folds
-- that require such finalization.
--
-- >>> Stream.toList $ Stream.pipe (Pipe.fromFold Fold.sum) $ Stream.fromList [1..5::Int]
-- [15]
--
{-# INLINE fromFold #-}
fromFold :: Monad m => Fold m a b -> Pipe m a b
fromFold (Fold fstep finitial _ _) =
    Pipe consume produce FoldConsumeInit

    where

    -- XXX make the initial state Either type and start in produce mode
    consume FoldConsumeInit x = do
        r <- finitial
        return $ case r of
            Fold.Partial s -> SkipP (FoldProduceInit s x)
            Fold.Done b -> YieldP FoldProduceStop b

    consume (FoldConsumeGo st) a = do
        r <- fstep st a
        return $ case r of
            Fold.Partial s -> SkipC (FoldConsumeGo s)
            Fold.Done b -> YieldP FoldProduceStop b

    produce (FoldProduceInit st x) = consume (FoldConsumeGo st) x
    produce FoldProduceStop = return Stop

-- | Produces the stream on consuming ().
--
{-# INLINE fromStream #-}
fromStream :: Monad m => Stream m a -> Pipe m () a
fromStream (Stream step state) = Pipe consume produce ()

    where

    -- XXX make the initial state Either type and start in produce mode
    consume () () = return $ SkipP state

    produce st = do
        r <- step defState st
        return $ case r of
            Stream.Yield b s -> YieldP s b
            Stream.Skip s -> SkipP s
            Stream.Stop -> Stop

{-# INLINE fromScanr #-}
fromScanr :: Monad m => Scanr m a b -> Pipe m a b
fromScanr (Scanr step initial) = Pipe consume undefined initial

    where

    consume st a = do
        r <- step st a
        return $ case r of
            Stream.Yield b s -> YieldC s b
            Stream.Skip s -> SkipC s
            Stream.Stop -> Stop
