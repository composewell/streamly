-- |
-- Module      : Streamly.Internal.Data.Stream.StreamK.Transformer
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.StreamK.Transformer
    (
      foldlT
    , foldrT

    , liftInner
    , evalStateT
    )
where

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State.Strict (StateT)
import Streamly.Internal.Data.Stream.StreamK
    (Stream, nil, cons, uncons, fromEffect)

import qualified Control.Monad.Trans.State.Strict as State

-- | Lazy left fold to an arbitrary transformer monad.
{-# INLINE foldlT #-}
foldlT :: (Monad m, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> Stream m a -> s m b
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
    => (a -> s m b -> s m b) -> s m b -> Stream m a -> s m b
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
evalStateT :: Monad m => m s -> Stream (StateT s m) a -> Stream m a
evalStateT = go

    where

    -- XXX Do not use stream monad
    go st m1 = do
        (res, s1) <- lift $ st >>= State.runStateT (uncons m1)
        case res of
            Just (h, t) -> cons h (go (return s1) t)
            Nothing -> nil

{-# INLINE liftInner #-}
liftInner :: (Monad m, MonadTrans t, Monad (t m)) =>
    Stream m a -> Stream (t m) a
liftInner = go

    where

    -- XXX Do not use stream monad
    go m1 = do
        res <- fromEffect $ lift $ uncons m1
        case res of
            Just (h, t) -> cons h (go t)
            Nothing -> nil
