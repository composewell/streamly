-- |
-- Module      : Streamly.Internal.Data.StreamK.Transformer
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.StreamK.Transformer
    (
      foldlT
    , foldrT

    , liftInner
    , evalStateT

    , localReaderT
    )
where

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State.Strict (StateT)
import Streamly.Internal.Data.StreamK.Type
    (StreamK(..), nil, cons, uncons, concatEffect, foldStream, mkStream)
import Control.Monad.Trans.Reader (ReaderT, local)

import qualified Control.Monad.Trans.State.Strict as State

-- | Lazy left fold to an arbitrary transformer monad.
{-# INLINE foldlT #-}
foldlT :: (Monad m, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> StreamK m a -> s m b
foldlT step = go
  where
    go acc m1 = do
        res <- lift $ uncons m1
        case res of
            Just (h, t) -> go (step acc h) t
            Nothing -> acc

-- | Right associative fold to an arbitrary transformer monad.
{-# INLINE foldrT #-}
foldrT :: (Monad m, Monad (s m), MonadTrans s)
    => (a -> s m b -> s m b) -> s m b -> StreamK m a -> s m b
foldrT step final = go
  where
    go m1 = do
        res <- lift $ uncons m1
        case res of
            Just (h, t) -> step h (go t)
            Nothing -> final

------------------------------------------------------------------------------
-- Lifting inner monad
------------------------------------------------------------------------------

{-# INLINE evalStateT #-}
evalStateT :: Monad m => m s -> StreamK (StateT s m) a -> StreamK m a
evalStateT = go

    where

    go st m1 = concatEffect $ fmap f (st >>= State.runStateT (uncons m1))

    f (res, s1) =
        case res of
            Just (h, t) -> cons h (go (return s1) t)
            Nothing -> nil

{-# INLINE liftInner #-}
liftInner :: (Monad m, MonadTrans t, Monad (t m)) =>
    StreamK m a -> StreamK (t m) a
liftInner = go

    where

    go m1 = concatEffect $ fmap f $ lift $ uncons m1

    f res =
        case res of
            Just (h, t) -> cons h (go t)
            Nothing -> nil

-- | Modify the environment of the underlying ReaderT monad.
{-# INLINABLE localReaderT #-}
localReaderT :: (r -> r) -> StreamK (ReaderT r m) a -> StreamK (ReaderT r m) a
localReaderT f m =
    mkStream $ \st yld sng stp ->
        let single = local f . sng
            yieldk a r = local f $ yld a (localReaderT f r)
        in foldStream st yieldk single (local f stp) m
