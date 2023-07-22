{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Lift
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.IsStream.Lift {-# DEPRECATED "Please use \"Streamly.Data.Stream.*\" instead." #-}
    (
    -- * Generalize Inner Monad
      hoist
    , generally

    -- * Transform Inner Monad
    , liftInner
    , usingReaderT
    , runReaderT
    , evalStateT
    , usingStateT
    , runStateT
    )
where

#include "inline.hs"

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor.Identity (Identity (..))
import Streamly.Internal.Data.Stream.IsStream.Type
    (IsStream(..), fromStreamD, toStreamD)
import Streamly.Internal.Data.Stream.Serial (SerialT)

import qualified Streamly.Internal.Data.Stream as D
    (morphInner, liftInner, runReaderT, evalStateT, runStateT)

------------------------------------------------------------------------------
-- Generalize the underlying monad
------------------------------------------------------------------------------

-- | Transform the inner monad of a stream using a natural transformation.
--
-- / Internal/
--
{-# INLINE hoist #-}
hoist :: (Monad m, Monad n)
    => (forall x. m x -> n x) -> SerialT m a -> SerialT n a
hoist f xs = fromStreamD $ D.morphInner f (toStreamD xs)

-- | Generalize the inner monad of the stream from 'Identity' to any monad.
--
-- / Internal/
--
{-# INLINE generally #-}
generally :: (IsStream t, Monad m) => t Identity a -> t m a
generally xs = fromStreamD $ D.morphInner (return . runIdentity) (toStreamD xs)

------------------------------------------------------------------------------
-- Add and remove a monad transformer
------------------------------------------------------------------------------

-- | Lift the inner monad @m@ of a stream @t m a@ to @tr m@ using the monad
-- transformer @tr@.
--
-- @since 0.8.0
--
{-# INLINE liftInner #-}
liftInner :: (Monad m, IsStream t, MonadTrans tr, Monad (tr m))
    => t m a -> t (tr m) a
liftInner xs = fromStreamD $ D.liftInner (toStreamD xs)

------------------------------------------------------------------------------
-- Sharing read only state in a stream
------------------------------------------------------------------------------

-- | Evaluate the inner monad of a stream as 'ReaderT'.
--
-- @since 0.8.0
--
{-# INLINE runReaderT #-}
runReaderT :: (IsStream t, Monad m) => m s -> t (ReaderT s m) a -> t m a
runReaderT s xs = fromStreamD $ D.runReaderT s (toStreamD xs)

-- | Run a stream transformation using a given environment.
--
-- See also: 'Serial.map'
--
-- / Internal/
--
{-# INLINE usingReaderT #-}
usingReaderT
    :: (Monad m, IsStream t)
    => m r
    -> (t (ReaderT r m) a -> t (ReaderT r m) a)
    -> t m a
    -> t m a
usingReaderT r f xs = runReaderT r $ f $ liftInner xs

------------------------------------------------------------------------------
-- Sharing read write state in a stream
------------------------------------------------------------------------------

-- | Evaluate the inner monad of a stream as 'StateT'.
--
-- This is supported only for 'SerialT' as concurrent state updation may not be
-- safe.
--
-- > evalStateT s = Stream.map snd . Stream.runStateT s
--
-- / Internal/
--
{-# INLINE evalStateT #-}
evalStateT ::  Monad m => m s -> SerialT (StateT s m) a -> SerialT m a
-- evalStateT s = fmap snd . runStateT s
evalStateT s xs = fromStreamD $ D.evalStateT s (toStreamD xs)

-- | Run a stateful (StateT) stream transformation using a given state.
--
-- This is supported only for 'SerialT' as concurrent state updation may not be
-- safe.
--
-- > usingStateT s f = evalStateT s . f . liftInner
--
-- See also: 'scanl''
--
-- / Internal/
--
{-# INLINE usingStateT #-}
usingStateT
    :: Monad m
    => m s
    -> (SerialT (StateT s m) a -> SerialT (StateT s m) a)
    -> SerialT m a
    -> SerialT m a
usingStateT s f = evalStateT s . f . liftInner

-- | Evaluate the inner monad of a stream as 'StateT' and emit the resulting
-- state and value pair after each step.
--
-- This is supported only for 'SerialT' as concurrent state updation may not be
-- safe.
--
-- @since 0.8.0
--
{-# INLINE runStateT #-}
runStateT :: Monad m => m s -> SerialT (StateT s m) a -> SerialT m (s, a)
runStateT s xs = fromStreamD $ D.runStateT s (toStreamD xs)
