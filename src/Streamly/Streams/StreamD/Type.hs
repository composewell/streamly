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

#include "../inline.hs"

-- |
-- Module      : Streamly.Streams.StreamD.Type
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Streams.StreamD.Type
    (
    -- * The stream type
      Step (..)
    -- XXX UnStream is exported to avoid a performance issue in concatMap if we
    -- use the pattern synonym "Stream".
#if __GLASGOW_HASKELL__ >= 800
    , Stream (Stream, UnStream)
#else
    , Stream (UnStream)
    , pattern Stream
#endif
    , map
    , mapM
    , foldrM
    , foldrMx
    , foldr
    , toList
    )
where

import Control.Applicative (liftA2)
import GHC.Types (SPEC(..))
import Streamly.SVar (State(..), adaptState, defState)
import qualified Streamly.Streams.StreamK as K
import Prelude hiding (map, mapM, foldr)

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

-- Note: toList is used in Array.Type, which is used in StreamD module,
-- therefore these definitions have been pushed here from StreamD.

-- The way we want a left fold to be strict, dually we want the right fold to
-- be lazy.  The correct signature of the fold function must be (a -> m b -> m
-- b) instead of (a -> b -> m b). We were using the latter earlier, which is
-- incorrect. In the latter signature we have to feed the value to the fold
-- function after evaluating the monadic action, depending on the bind behavior
-- of the monad, the action may get evaluated introducing unnecessary
-- strictness to the fold. If the implementation is lazy the following example,
-- must work:
--
-- S.foldrM (\x t -> if x then return t else return False) (return True)
--  (S.fromList [False,undefined] :: SerialT IO Bool)
--
-- Removed SPEC constructor, it was causing 2x performance degradation in
-- any/or.
--
{-# INLINE_NORMAL foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> Stream m a -> m b
foldrM f z (Stream step state) = go state
  where
    go st = do
          r <- step defState st
          case r of
            Yield x s -> f x (go s)
            Skip s    -> go s
            Stop      -> z

{-# INLINE_NORMAL foldrMx #-}
foldrMx :: Monad m
    => (a -> m x -> m x) -> m x -> (m x -> m b) -> Stream m a -> m b
foldrMx fstep final project (Stream step state) = project $ go state
  where
    go st = do
          r <- step defState st
          case r of
            Yield x s -> fstep x (go s)
            Skip s    -> go s
            Stop      -> final

-- Note that foldr becomes necessarily strict if the monad m is strict. In that
-- case it cannot terminate early, it would evaluate all of its input. For this
-- reason we have stopped exporting it. Though, this should work fine with
-- lazy monads.
--
-- XXX In many cases foldrM seems to perform much better than foldr. For
-- example if foldr is used in "any" instead of foldrM, it seems to perform
-- 1000x poorly. Perhaps does not fuse well?  Is that because of the extra bind
-- in foldr? Need to investigate.
--
{-# INLINE_NORMAL foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> Stream m a -> m b
foldr f z = foldrM (\a b -> liftA2 f (return a) b) (return z)

{-# INLINE toList #-}
toList :: Monad m => Stream m a -> m [a]
toList = foldr (:) []
