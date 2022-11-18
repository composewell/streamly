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
      liftInner
    , evalStateT
    )
where

import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.State.Strict (StateT)
import Streamly.Internal.Data.Stream.StreamK
    (Stream, nil, cons, uncons, fromEffect)

import qualified Control.Monad.Trans.State.Strict as State

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
