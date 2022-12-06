-- |
-- Module      : Streamly.Internal.Data.Stream.Transformer
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Transformer
    (
      foldlT
    , foldrT

    , liftInner
    , usingReaderT
    , runReaderT
    , evalStateT
    , usingStateT
    , runStateT
    )
where

import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Streamly.Internal.Data.Stream.Type (Stream, fromStreamD, toStreamD)

import qualified Streamly.Internal.Data.Stream.StreamD.Transformer as D

-- $setup
-- >>> :m
-- >>> import Control.Monad.Trans (lift)
-- >>> import Control.Monad.Trans.Identity (runIdentityT)
-- >>> import qualified Streamly.Internal.Data.Stream.Transformer as Stream

-- | Lazy left fold to a transformer monad.
--
{-# INLINE foldlT #-}
foldlT :: (Monad m, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> Stream m a -> s m b
foldlT f z s = D.foldlT f z (toStreamD s)

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
{-# INLINE foldrT #-}
foldrT :: (Monad m, Monad (s m), MonadTrans s)
    => (a -> s m b -> s m b) -> s m b -> Stream m a -> s m b
foldrT f z s = D.foldrT f z (toStreamD s)

------------------------------------------------------------------------------
-- Add and remove a monad transformer
------------------------------------------------------------------------------

-- | Lift the inner monad @m@ of a stream @Stream m a@ to @tr m@ using the monad
-- transformer @tr@.
--
{-# INLINE liftInner #-}
liftInner :: (Monad m, MonadTrans tr, Monad (tr m))
    => Stream m a -> Stream (tr m) a
liftInner xs = fromStreamD $ D.liftInner (toStreamD xs)

------------------------------------------------------------------------------
-- Sharing read only state in a stream
------------------------------------------------------------------------------

-- | Evaluate the inner monad of a stream as 'ReaderT'.
--
{-# INLINE runReaderT #-}
runReaderT :: Monad m => m s -> Stream (ReaderT s m) a -> Stream m a
runReaderT s xs = fromStreamD $ D.runReaderT s (toStreamD xs)

-- | Run a stream transformation using a given environment.
--
-- See also: 'Serial.map'
--
-- / Internal/
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
-- / Internal/
--
{-# INLINE evalStateT #-}
evalStateT ::  Monad m => m s -> Stream (StateT s m) a -> Stream m a
-- evalStateT s = fmap snd . runStateT s
evalStateT s xs = fromStreamD $ D.evalStateT s (toStreamD xs)

-- | Run a stateful (StateT) stream transformation using a given state.
--
-- >>> usingStateT s f = Stream.evalStateT s . f . Stream.liftInner
--
-- See also: 'scan'
--
-- / Internal/
--
{-# INLINE usingStateT #-}
usingStateT
    :: Monad m
    => m s
    -> (Stream (StateT s m) a -> Stream (StateT s m) a)
    -> Stream m a
    -> Stream m a
usingStateT s f = evalStateT s . f . liftInner

-- | Evaluate the inner monad of a stream as 'StateT' and emit the resulting
-- state and value pair after each step.
--
{-# INLINE runStateT #-}
runStateT :: Monad m => m s -> Stream (StateT s m) a -> Stream m (s, a)
runStateT s xs = fromStreamD $ D.runStateT s (toStreamD xs)
