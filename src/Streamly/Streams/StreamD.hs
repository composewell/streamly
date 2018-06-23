{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnboxedTuples             #-}

#include "inline.h"

-- |
-- Module      : Streamly.Streams.StreamD
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Direct style re-implementation of CPS style stream in StreamK module.  The
-- symbol or suffix 'D' in this module denotes the "Direct" style.  GHC is able
-- to INLINE and fuse direct style better, providing better performance than
-- CPS implementation.
--
-- import qualified Streamly.Streams.StreamD as D
--
module Streamly.Streams.StreamD
    (
    -- * The stream type
      Step (..)
    , Stream (..)

    -- * Elimination
    , runStream
    , uncons

    -- * Generation
    , nil
    , yield
    , yieldM
    , unfoldr
    , unfoldrM
    , repeat
    , enumFromStepN
    , fromList

    -- * Transformation
    , mapM
    , mapM_

    -- * Conversion
    , toStreamK
    , fromStreamK
    )
where

import qualified Streamly.Streams.StreamK as K
import Prelude hiding (mapM, mapM_, repeat)

------------------------------------------------------------------------------
-- The direct style stream type
------------------------------------------------------------------------------

-- | A stream is a succession of 'Step's. A 'Yield' produces a single value and
-- the next state of the stream. Stop indicates there are no more values in the
-- stream.
data Step s a = Yield a s | Stop

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap f (Yield x s) = Yield (f x) s
    fmap _ Stop = Stop

-- | A stream consists of a step function that generates the next step given a
-- current state, and the current state.
data Stream m a = forall s. Stream (s -> m (Step s a)) s

-------------------------------------------------------------------------------
-- IsStream Stream
-------------------------------------------------------------------------------

-- Convert a direct stream to and from CPS encoded stream
{-# INLINE_NORMAL toStreamK #-}
toStreamK :: Monad m => Stream m a -> K.Stream m a
toStreamK (Stream step state) = go state
    where
    go st = K.Stream $ \_ stp _ yld -> do
        r <- step st
        case r of
            Yield x s -> yld x (go s)
            Stop      -> stp

{-# INLINE_NORMAL fromStreamK #-}
fromStreamK :: Monad m => K.Stream m a -> Stream m a
fromStreamK m = Stream step m
    where
    step m1 =
        let stop       = return Stop
            single a   = return $ Yield a K.nil
            yieldk a r = return $ Yield a r
         in K.unStream m1 Nothing stop single yieldk

{-
instance K.IsStream Stream where
    toStream = toStreamK
    fromStream = fromStreamK

    consM = undefined
    (|:) = undefined
-}

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- | An empty 'Stream'.
{-# INLINE_NORMAL nil #-}
nil :: Monad m => Stream m a
nil = Stream (const $ return Stop) ()

-- | Create a singleton 'Stream' from a pure value.
{-# INLINE_NORMAL yield #-}
yield :: Monad m => a -> Stream m a
yield x = Stream (return . step) True
  where
    {-# INLINE_LATE step #-}
    step True  = Yield x False
    step False = Stop

-- | Create a singleton 'Stream' from a monadic action.
{-# INLINE_NORMAL yieldM #-}
yieldM :: Monad m => m a -> Stream m a
yieldM m = Stream step True
  where
    {-# INLINE_LATE step #-}
    step True  = m >>= \x -> return $ Yield x False
    step False = return Stop

------------------------------------------------------------------------------
-- Generation by unfold
------------------------------------------------------------------------------

{-# INLINE_NORMAL unfoldrM #-}
unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Stream m a
unfoldrM next state = Stream step state
  where
    {-# INLINE_LATE step #-}
    step st = do
        r <- next st
        return $ case r of
            Just (x, s) -> Yield x s
            Nothing     -> Stop

{-# INLINE_NORMAL unfoldr #-}
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
unfoldr f = unfoldrM (return . f)

------------------------------------------------------------------------------
-- Generation
------------------------------------------------------------------------------

repeat :: Monad m => a -> Stream m a
repeat x = Stream (const $ return $ Yield x ()) ()

{-# INLINE_NORMAL enumFromStepN #-}
enumFromStepN :: (Num a, Monad m) => a -> a -> Int -> Stream m a
enumFromStepN from stride n =
    from `seq` stride `seq` n `seq` Stream step (from, n)
    where
        {-# INLINE_LATE step #-}
        step (x, i) | i > 0     = return $ Yield x (x + stride, i - 1)
                    | otherwise = return $ Stop

-- | Convert a list to a 'Stream'
{-# INLINE_NORMAL fromList #-}
fromList :: Monad m => [a] -> Stream m a
fromList zs = Stream step zs
  where
    {-# INLINE_LATE step #-}
    step (x:xs) = return $ Yield x xs
    step []     = return Stop

------------------------------------------------------------------------------
-- Transformation
------------------------------------------------------------------------------

-- | Map a monadic function over a 'Stream'
{-# INLINE_NORMAL mapM #-}
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' st = do
        r <- step st
        case r of
            Yield x s -> f x >>= \a -> return $ Yield a s
            Stop      -> return Stop

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE_NORMAL uncons #-}
uncons :: Monad m => Stream m a -> m (Maybe (a, Stream m a))
uncons (Stream step state) = go state
  where
    go st = do
        r <- step st
        return $ case r of
            Yield x s -> Just (x, (Stream step s))
            Stop      -> Nothing

-- | Run a streaming composition, discard the results.
{-# INLINE_NORMAL runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream (Stream step state) = go state
  where
    go st = do
        r <- step st
        case r of
            Yield _ s -> go s
            Stop      -> return ()

-- | Execute a monadic action for each element of the 'Stream'
{-# INLINE_NORMAL mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
mapM_ m = runStream . mapM m
