{-# LANGUAGE CPP #-}
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
      foldlT
    , foldrT

    -- * Transform Inner Monad
    , liftInner
    , runReaderT
    , usingReaderT
    , evalStateT
    , runStateT
    , usingStateT
    )
where

#include "inline.hs"

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.SVar.Type (defState, adaptState)

import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Strict as State

import Streamly.Internal.Data.Stream.StreamD.Type

#include "DocTestDataStream.hs"

-- | Lazy left fold to a transformer monad.
--
{-# INLINE_NORMAL foldlT #-}
foldlT :: (Monad m, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> Stream m a -> s m b
foldlT fstep begin (Stream step state) = go SPEC begin state
  where
    go !_ acc st = do
        r <- lift $ step defState st
        case r of
            Yield x s -> go SPEC (fstep acc x) s
            Skip s -> go SPEC acc s
            Stop   -> acc

-- | Right fold to a transformer monad.  This is the most general right fold
-- function. 'foldrS' is a special case of 'foldrT', however 'foldrS'
-- implementation can be more efficient:
--
-- >>> foldrS = Stream.foldrT
--
-- >>> step f x xs = lift $ f x (runIdentityT xs)
-- >>> foldrM f z s = runIdentityT $ Stream.foldrT (step f) (lift z) s
--
-- 'foldrT' can be used to translate streamly streams to other transformer
-- monads e.g.  to a different streaming type.
--
-- /Pre-release/
{-# INLINE_NORMAL foldrT #-}
foldrT :: (Monad m, Monad (t m), MonadTrans t)
    => (a -> t m b -> t m b) -> t m b -> Stream m a -> t m b
foldrT f final (Stream step state) = go SPEC state
  where
    {-# INLINE_LATE go #-}
    go !_ st = do
          r <- lift $ step defState st
          case r of
            Yield x s -> f x (go SPEC s)
            Skip s    -> go SPEC s
            Stop      -> final

-------------------------------------------------------------------------------
-- Transform Inner Monad
-------------------------------------------------------------------------------

-- | Lift the inner monad @m@ of @Stream m a@ to @t m@ where @t@ is a monad
-- transformer.
--
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

------------------------------------------------------------------------------
-- Sharing read only state in a stream
------------------------------------------------------------------------------

-- | Evaluate the inner monad of a stream as 'ReaderT'.
--
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

-- | Run a stream transformation using a given environment.
--
{-# INLINE usingReaderT #-}
usingReaderT
    :: Monad m
    => m r
    -> (Stream (ReaderT r m) a -> Stream (ReaderT r m) a)
    -> Stream m a
    -> Stream m a
usingReaderT r f xs = runReaderT r $ f $ liftInner xs

------------------------------------------------------------------------------
-- Sharing read write state in a stream
------------------------------------------------------------------------------

-- | Evaluate the inner monad of a stream as 'StateT'.
--
-- >>> evalStateT s = fmap snd . Stream.runStateT s
--
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

-- | Evaluate the inner monad of a stream as 'StateT' and emit the resulting
-- state and value pair after each step.
--
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

-- | Run a stateful (StateT) stream transformation using a given state.
--
-- >>> usingStateT s f = Stream.evalStateT s . f . Stream.liftInner
--
-- See also: 'scan'
--
{-# INLINE usingStateT #-}
usingStateT
    :: Monad m
    => m s
    -> (Stream (StateT s m) a -> Stream (StateT s m) a)
    -> Stream m a
    -> Stream m a
usingStateT s f = evalStateT s . f . liftInner
