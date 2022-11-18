-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Transformer
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Transform the underlying monad of a stream using a monad transfomer.

module Streamly.Internal.Data.Stream.StreamD.Transformer
    (
    -- * Transform Inner Monad
      liftInner
    , runReaderT
    , evalStateT
    , runStateT
    )
where

#include "inline.hs"

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Streamly.Internal.Data.SVar.Type (adaptState)

import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Strict as State

import Streamly.Internal.Data.Stream.StreamD.Type

-------------------------------------------------------------------------------
-- Transform Inner Monad
-------------------------------------------------------------------------------

{-# INLINE_NORMAL liftInner #-}
liftInner :: (Monad m, MonadTrans t, Monad (t m))
    => Stream m a -> Stream (t m) a
liftInner (Stream step state) = Stream step' state
    where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- lift $ step (adaptState gst) st
        return $ case r of
            Yield x s -> Yield x s
            Skip s    -> Skip s
            Stop      -> Stop

{-# INLINE_NORMAL runReaderT #-}
runReaderT :: Monad m => m s -> Stream (ReaderT s m) a -> Stream m a
runReaderT env (Stream step state) = Stream step' (state, env)
    where
    {-# INLINE_LATE step' #-}
    step' gst (st, action) = do
        sv <- action
        r <- Reader.runReaderT (step (adaptState gst) st) sv
        return $ case r of
            Yield x s -> Yield x (s, return sv)
            Skip  s   -> Skip (s, return sv)
            Stop      -> Stop

{-# INLINE_NORMAL evalStateT #-}
evalStateT :: Monad m => m s -> Stream (StateT s m) a -> Stream m a
evalStateT initial (Stream step state) = Stream step' (state, initial)
    where
    {-# INLINE_LATE step' #-}
    step' gst (st, action) = do
        sv <- action
        (r, !sv') <- State.runStateT (step (adaptState gst) st) sv
        return $ case r of
            Yield x s -> Yield x (s, return sv')
            Skip  s   -> Skip (s, return sv')
            Stop      -> Stop

{-# INLINE_NORMAL runStateT #-}
runStateT :: Monad m => m s -> Stream (StateT s m) a -> Stream m (s, a)
runStateT initial (Stream step state) = Stream step' (state, initial)
    where
    {-# INLINE_LATE step' #-}
    step' gst (st, action) = do
        sv <- action
        (r, !sv') <- State.runStateT (step (adaptState gst) st) sv
        return $ case r of
            Yield x s -> Yield (sv', x) (s, return sv')
            Skip  s   -> Skip (s, return sv')
            Stop      -> Stop
