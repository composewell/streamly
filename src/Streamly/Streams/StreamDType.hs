{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE RankNTypes                #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Streams.StreamDType
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Streams.StreamDType
    (
    -- * The stream type
      Step (..)
#if __GLASGOW_HASKELL__ >= 800
    , Stream (Stream)
#else
    , Stream ()
    , pattern Stream
#endif
    , map
    , mapM
    )
where

import Streamly.SVar (State(..), adaptState)
import qualified Streamly.Streams.StreamK as K
import Prelude hiding (map, mapM)

------------------------------------------------------------------------------
-- The direct style stream type
------------------------------------------------------------------------------

-- | A stream is a succession of 'Step's. A 'Yield' produces a single value and
-- the next state of the stream. 'Stop' indicates there are no more values in
-- the stream.
data Step s a = Yield a s | Skip s | Stop

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap f (Yield x s) = Yield (f x) s
    fmap _ (Skip s) = Skip s
    fmap _ Stop = Stop

-- gst = global state
-- | A stream consists of a step function that generates the next step given a
-- current state, and the current state.
data Stream m a =
    forall s. UnStream (State K.Stream m a -> s -> m (Step s a)) s

unShare :: Stream m a -> Stream m a
unShare (UnStream step state) = UnStream step' state
    where step' gst = step (adaptState gst)

pattern Stream :: (State K.Stream m a -> s -> m (Step s a)) -> s -> Stream m a
pattern Stream step state <- (unShare -> UnStream step state)
    where Stream = UnStream

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Stream #-}
#endif

------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

-- | Map a monadic function over a 'Stream'
{-# INLINE_NORMAL mapM #-}
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> f x >>= \a -> return $ Yield a s
            Skip s    -> return $ Skip s
            Stop      -> return Stop

{-# INLINE map #-}
map :: Monad m => (a -> b) -> Stream m a -> Stream m b
map f = mapM (return . f)

instance Monad m => Functor (Stream m) where
    {-# INLINE fmap #-}
    fmap = map
