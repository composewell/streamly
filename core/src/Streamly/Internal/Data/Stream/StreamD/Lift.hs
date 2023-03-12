{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Lift
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Transform the underlying monad of a stream.

module Streamly.Internal.Data.Stream.StreamD.Lift
    (
    -- * Generalize Inner Monad
      morphInner
    , generalizeInner

    -- * Transform Inner Monad
    , liftInnerWith
    , runInnerWith
    , runInnerWithState
    )
where

#include "inline.hs"

import Data.Functor.Identity (Identity(..))
import Streamly.Internal.Data.SVar.Type (adaptState)

import Streamly.Internal.Data.Stream.StreamD.Type

#include "DocTestDataStream.hs"

-------------------------------------------------------------------------------
-- Generalize Inner Monad
-------------------------------------------------------------------------------

-- | Transform the inner monad of a stream using a natural transformation.
--
-- Example, generalize the inner monad from Identity to any other:
--
-- >>> generalizeInner = Stream.morphInner (return . runIdentity)
--
-- Also known as hoist.
--
{-# INLINE_NORMAL morphInner #-}
morphInner :: Monad n => (forall x. m x -> n x) -> Stream m a -> Stream n a
morphInner f (Stream step state) = Stream step' state
    where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- f $ step (adaptState gst) st
        return $ case r of
            Yield x s -> Yield x s
            Skip  s   -> Skip s
            Stop      -> Stop

-- | Generalize the inner monad of the stream from 'Identity' to any monad.
--
-- Definition:
--
-- >>> generalizeInner = Stream.morphInner (return . runIdentity)
--
{-# INLINE generalizeInner #-}
generalizeInner :: Monad m => Stream Identity a -> Stream m a
generalizeInner = morphInner (return . runIdentity)

-------------------------------------------------------------------------------
-- Transform Inner Monad
-------------------------------------------------------------------------------

-- | Lift the inner monad @m@ of a stream @Stream m a@ to @t m@ using the
-- supplied lift function.
--
{-# INLINE_NORMAL liftInnerWith #-}
liftInnerWith :: (Monad (t m)) =>
    (forall b. m b -> t m b) -> Stream m a -> Stream (t m) a
liftInnerWith lift (Stream step state) = Stream step1 state

    where

    {-# INLINE_LATE step1 #-}
    step1 gst st = do
        r <- lift $ step (adaptState gst) st
        return $ case r of
            Yield x s -> Yield x s
            Skip s    -> Skip s
            Stop      -> Stop

-- | Evaluate the inner monad of a stream using the supplied runner function.
--
{-# INLINE_NORMAL runInnerWith #-}
runInnerWith :: Monad m =>
    (forall b. t m b -> m b) -> Stream (t m) a -> Stream m a
runInnerWith run (Stream step state) = Stream step1 state

    where

    {-# INLINE_LATE step1 #-}
    step1 gst st = do
        r <- run $ step (adaptState gst) st
        return $ case r of
            Yield x s -> Yield x s
            Skip s -> Skip s
            Stop -> Stop

-- | Evaluate the inner monad of a stream using the supplied stateful runner
-- function and the initial state. The state returned by an invocation of the
-- runner is supplied as input state to the next invocation.
--
{-# INLINE_NORMAL runInnerWithState #-}
runInnerWithState :: Monad m =>
    (forall b. s -> t m b -> m (b, s))
    -> m s
    -> Stream (t m) a
    -> Stream m (s, a)
runInnerWithState run initial (Stream step state) =
    Stream step1 (state, initial)

    where

    {-# INLINE_LATE step1 #-}
    step1 gst (st, action) = do
        sv <- action
        (r, !sv1) <- run sv (step (adaptState gst) st)
        return $ case r of
            Yield x s -> Yield (sv1, x) (s, return sv1)
            Skip s -> Skip (s, return sv1)
            Stop -> Stop
