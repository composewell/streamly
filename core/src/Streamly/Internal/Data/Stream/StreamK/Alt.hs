-- |
-- Module      : Streamly.StreamDK.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A CPS style stream using a constructor based representation instead of a
-- function based representation.
--
-- Streamly internally uses two fundamental stream representations, (1) streams
-- with an open or arbitrary control flow (we call it StreamK), (2) streams
-- with a structured or closed loop control flow (we call it StreamD). The
-- higher level stream types can use any of these representations under the
-- hood and can interconvert between the two.
--
-- StreamD:
--
-- StreamD is a non-recursive data type in which the state of the stream and
-- the step function are separate. When the step function is called, a stream
-- element and the new stream state is yielded. The generated element and the
-- state are passed to the next consumer in the loop. The state is threaded
-- around in the loop until control returns back to the original step function
-- to run the next step. This creates a structured closed loop representation
-- (like "for" loops in C) with state of each step being hidden/abstracted or
-- existential within that step. This creates a loop representation identical
-- to the "for" or "while" loop constructs in imperative languages, the states
-- of the steps combined together constitute the state of the loop iteration.
--
-- Internally most combinators use a closed loop representation because it
-- provides very high efficiency due to stream fusion. The performance of this
-- representation is competitive to the C language implementations.
--
-- Pros and Cons of StreamD:
--
-- 1) stream-fusion: This representation can be optimized very efficiently by
-- the compiler because the state is explicitly separated from step functions,
-- represented using pure data constructors and visible to the compiler, the
-- stream steps can be fused using case-of-case transformations and the state
-- can be specialized using spec-constructor optimization, yielding a C like
-- tight loop/state machine with no constructors, the state is used unboxed and
-- therefore no unnecessary allocation.
--
-- 2) Because of a closed representation consing too many elements in this type
-- of stream does not scale, it will have quadratic performance slowdown. Each
-- cons creates a layer that needs to return the control back to the caller.
-- Another implementation of cons is possible but that will have to box/unbox
-- the state and will not fuse. So effectively cons breaks fusion.
--
-- 3) unconsing an item from the stream breaks fusion, we have to "pause" the
-- loop, rebox and save the state.
--
-- 3) Exception handling is easy to implement in this model because control
-- flow is structured in the loop and cannot be arbitrary. Therefore,
-- implementing "bracket" is natural.
--
-- 4) Round-robin scheduling for co-operative multitasking is easy to implement.
--
-- 5) It fuses well with the direct style Fold implementation.
--
-- StreamK/StreamDK:
--
-- StreamDK i.e. the stream defined in this module, like StreamK, is a
-- recursive data type which has no explicit state defined using constructors,
-- each step yields an element and a computation representing the rest of the
-- stream.  Stream state is part of the function representing the rest of the
-- stream.  This creates an open computation representation, or essentially a
-- continuation passing style computation.  After the stream step is executed,
-- the caller is free to consume the produced element and then send the control
-- wherever it wants, there is no restriction on the control to return back
-- somewhere, the control is free to go anywhere. The caller may decide not to
-- consume the rest of the stream. This representation is more like a "goto"
-- based implementation in imperative languages.
--
-- Pros and Cons of StreamK:
--
-- 1) The way StreamD can be optimized using stream-fusion, this type can be
-- optimized using foldr/build fusion. However, foldr/build has not yet been
-- fully implemented for StreamK/StreamDK.
--
-- 2) Using cons is natural in this representation, unlike in StreamD it does
-- not have a quadratic slowdown. Currently, we in fact wrap StreamD in StreamK
-- to support a better cons operation.
--
-- 3) Similarly, uncons is natural in this representation.
--
-- 4) Exception handling is not easy to implement because of the "goto" nature
-- of CPS.
--
-- 5) Composable folds are not implemented/proven, however, intuition says that
-- a push style CPS representation should be able to be used along with StreamK
-- to efficiently implement composable folds.

module Streamly.Internal.Data.Stream.StreamK.Alt
    (
    -- * Stream Type

      Stream
    , Step (..)

    -- * Construction
    , nil
    , cons
    , consM
    , unfoldr
    , unfoldrM
    , replicateM

    -- * Folding
    , uncons
    , foldrS

    -- * Specific Folds
    , drain
    )
where

#include "inline.hs"

-- XXX Use Cons and Nil instead of Yield and Stop?
data Step m a = Yield a (Stream m a) | Stop

newtype Stream m a = Stream (m (Step m a))

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

nil :: Monad m => Stream m a
nil = Stream $ return Stop

{-# INLINE_NORMAL cons #-}
cons :: Monad m => a -> Stream m a -> Stream m a
cons x xs = Stream $ return $ Yield x xs

consM :: Monad m => m a -> Stream m a -> Stream m a
consM eff xs = Stream $ eff >>= \x -> return $ Yield x xs

unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM next state = Stream (step' state)
  where
    step' st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x (Stream (step' s))
            Nothing     -> Stop
{-
unfoldrM next s0 = buildM $ \yld stp ->
    let go s = do
            r <- next s
            case r of
                Just (a, b) -> yld a (go b)
                Nothing -> stp
    in go s0
-}

{-# INLINE unfoldr #-}
unfoldr :: Monad m => (b -> Maybe (a, b)) -> b -> Stream m a
unfoldr next s0 = build $ \yld stp ->
    let go s =
            case next s of
                Just (a, b) -> yld a (go b)
                Nothing -> stp
    in go s0

replicateM :: Monad m => Int -> a -> Stream m a
replicateM n x = Stream (step n)
    where
    step i = return $
        if i <= 0
        then Stop
        else Yield x (Stream (step (i - 1)))

-------------------------------------------------------------------------------
-- Folding
-------------------------------------------------------------------------------

uncons :: Monad m => Stream m a -> m (Maybe (a, Stream m a))
uncons (Stream step) = do
    r <- step
    return $ case r of
        Yield x xs -> Just (x, xs)
        Stop -> Nothing

-- | Lazy right associative fold to a stream.
{-# INLINE_NORMAL foldrS #-}
foldrS :: Monad m
       => (a -> Stream m b -> Stream m b)
       -> Stream m b
       -> Stream m a
       -> Stream m b
foldrS f streamb = go
    where
    go (Stream stepa) = Stream $ do
        r <- stepa
        case r of
            Yield x xs -> let Stream step = f x (go xs) in step
            Stop -> let Stream step = streamb in step

{-# INLINE_LATE foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> Stream m a -> m b
foldrM fstep acc = go
    where
    go (Stream step) = do
        r <- step
        case r of
            Yield x xs -> fstep x (go xs)
            Stop -> acc

{-# INLINE_NORMAL build #-}
build :: Monad m
    => forall a. (forall b. (a -> b -> b) -> b -> b) -> Stream m a
build g = g cons nil

{-# RULES
"foldrM/build"  forall k z (g :: forall b. (a -> b -> b) -> b -> b).
                foldrM k z (build g) = g k z #-}

{-
-- To fuse foldrM with unfoldrM we need the type m1 to be polymorphic such that
-- it is either Monad m or Stream m.  So that we can use cons/nil as well as
-- monadic construction function as its arguments.
--
{-# INLINE_NORMAL buildM #-}
buildM :: Monad m
    => forall a. (forall b. (a -> m1 b -> m1 b) -> m1 b -> m1 b) -> Stream m a
buildM g = g cons nil
-}

-------------------------------------------------------------------------------
-- Specific folds
-------------------------------------------------------------------------------

{-# INLINE drain #-}
drain :: Monad m => Stream m a -> m ()
drain = foldrM (\_ xs -> xs) (return ())
{-
drain (Stream step) = do
    r <- step
    case r of
        Yield _ next -> drain next
        Stop      -> return ()
        -}
