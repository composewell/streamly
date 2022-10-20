-- |
-- Module      : Streamly.Internal.Data.Stream.Lift
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Lift
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
import Streamly.Internal.Data.Stream.Type
    (Stream, fromStreamK, toStreamK, fromStreamD, toStreamD)

import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K

-- $setup
-- >>> :m
-- >>> import Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Generalize the underlying monad
------------------------------------------------------------------------------

-- | Transform the inner monad of a stream using a natural transformation.
--
-- / Internal/
--
{-# INLINE hoist #-}
hoist :: (Monad m, Monad n)
    => (forall x. m x -> n x) -> Stream m a -> Stream n a
hoist f xs = fromStreamK $ K.hoist f (toStreamK xs)

-- | Generalize the inner monad of the stream from 'Identity' to any monad.
--
-- / Internal/
--
{-# INLINE generally #-}
generally :: Monad m => Stream Identity a -> Stream m a
generally xs = fromStreamK $ K.hoist (return . runIdentity) (toStreamK xs)

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
