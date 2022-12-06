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
      hoist
    , generally -- XXX generalize

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

-------------------------------------------------------------------------------
-- Generalize Inner Monad
-------------------------------------------------------------------------------

{-# INLINE_NORMAL hoist #-}
hoist :: Monad n => (forall x. m x -> n x) -> Stream m a -> Stream n a
hoist f (Stream step state) = Stream step' state
    where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- f $ step (adaptState gst) st
        return $ case r of
            Yield x s -> Yield x s
            Skip  s   -> Skip s
            Stop      -> Stop

{-# INLINE generally #-}
generally :: Monad m => Stream Identity a -> Stream m a
generally = hoist (return . runIdentity)

-------------------------------------------------------------------------------
-- Transform Inner Monad
-------------------------------------------------------------------------------

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
