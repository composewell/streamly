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
-- @
-- import qualified Streamly.Streams.StreamD as D
-- @

-- A majority of functions in this file have been adapted from the vector
-- library,  https://hackage.haskell.org/package/vector.

module Streamly.Streams.StreamD
    (
    -- * The stream type
      Step (..)
    , Stream (..)

    -- * Construction
    , nil
    , cons

    -- * Deconstruction
    , uncons

    -- * Generation
    -- ** Unfolds
    , unfoldr
    , unfoldrM

    -- ** Specialized Generation
    -- | Generate a monadic stream from a seed.
    , repeat
    , enumFromStepN

    -- ** Conversions
    -- | Transform an input structure into a stream.
    -- | Direct style stream does not support @fromFoldable@.
    , yield
    , yieldM
    , fromList
    , fromListM
    , fromStreamK

    -- * Elimination
    -- ** General Folds
    , foldr
    , foldrM
    , foldl'
    , foldlM'

    -- ** Specialized Folds
    , runStream
    , last
    , elem
    , notElem
    , all
    , any
    , maximum
    , minimum

    -- ** Map and Fold
    , mapM_

    -- ** Conversions
    -- | Transform a stream into another type.
    , toList
    , toStreamK

    -- * Transformation
    -- ** By folding (scans)
    , scanlM'

    -- * Filtering
    , filter
    , filterM
    , take
    , takeWhile
    , takeWhileM
    , drop
    , dropWhile
    , dropWhileM

    -- * Mapping
    , map
    , mapM

    -- ** Map and Filter
    , mapMaybe
    , mapMaybeM
    )
where

import Data.Maybe (fromJust, isJust)
import GHC.Types ( SPEC(..) )
import Prelude
       hiding (map, mapM, mapM_, repeat, foldr, last, take, filter,
               takeWhile, drop, dropWhile, all, any, maximum, minimum, elem,
               notElem)

import Streamly.SVar (MonadAsync)
import qualified Streamly.Streams.StreamK as K

------------------------------------------------------------------------------
-- The direct style stream type
------------------------------------------------------------------------------

-- | A stream is a succession of 'Step's. A 'Yield' produces a single value and
-- the next state of the stream. 'Stop' indicates there are no more values in
-- the stream.
data Step s a = Yield a s | Stop

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap f (Yield x s) = Yield (f x) s
    fmap _ Stop = Stop

-- | A stream consists of a step function that generates the next step given a
-- current state, and the current state.
data Stream m a = forall s. Stream (s -> m (Step s a)) s

------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- | An empty 'Stream'.
{-# INLINE_NORMAL nil #-}
nil :: Monad m => Stream m a
nil = Stream (const $ return Stop) ()

-- | Note that the cons operation is not scalable as it has O(n^2) complexity.
cons :: Monad m => a -> Stream m a -> Stream m a
cons x (Stream step state) = Stream step1 Nothing
    where
    step1 Nothing   = return $ Yield x (Just state)
    step1 (Just st) = do
        r <- step st
        case r of
            Yield a s -> return $ Yield a (Just s)
            Stop -> return Stop

-------------------------------------------------------------------------------
-- Deconstruction
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

{-# INLINE_LATE unfoldr #-}
unfoldr :: Monad m => (s -> Maybe (a, s)) -> s -> Stream m a
unfoldr f = unfoldrM (return . f)

------------------------------------------------------------------------------
-- Specialized Generation
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

-------------------------------------------------------------------------------
-- Generation by Conversion
-------------------------------------------------------------------------------

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

-- XXX we need the MonadAsync constraint because of a rewrite rule.
-- | Convert a list of monadic actions to a 'Stream'
{-# INLINE_LATE fromListM #-}
fromListM :: MonadAsync m => [m a] -> Stream m a
fromListM zs = Stream step zs
  where
    {-# INLINE_LATE step #-}
    step (m:ms) = m >>= \x -> return $ Yield x ms
    step []     = return Stop

-- | Convert a list of pure values to a 'Stream'
{-# INLINE_LATE fromList #-}
fromList :: Monad m => [a] -> Stream m a
fromList zs = Stream step zs
  where
    {-# INLINE_LATE step #-}
    step (x:xs) = return $ Yield x xs
    step []     = return Stop

{-# INLINE_LATE fromStreamK #-}
fromStreamK :: Monad m => K.Stream m a -> Stream m a
fromStreamK m = Stream step m
    where
    step m1 =
        let stop       = return Stop
            single a   = return $ Yield a K.nil
            yieldk a r = return $ Yield a r
         in K.unStream m1 Nothing stop single yieldk

------------------------------------------------------------------------------
-- Elimination by Folds
------------------------------------------------------------------------------

{-# INLINE_NORMAL foldrM #-}
foldrM :: Monad m => (a -> b -> m b) -> b -> Stream m a -> m b
foldrM f z (Stream step state) = go SPEC state
  where
    go !_ st = do
          r <- step st
          case r of
            Yield x s -> go SPEC s >>= f x
            Stop      -> return z

{-# INLINE_NORMAL foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> Stream m a -> m b
foldr f = foldrM (\a b -> return (f a b))

{-# INLINE_NORMAL foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> b -> Stream m a -> m b
foldlM' fstep begin (Stream step state) = go SPEC begin state
  where
    go !_ acc st = acc `seq` do
        r <- step st
        case r of
            Yield x s -> do
                acc' <- fstep acc x
                go SPEC acc' s
            Stop -> return acc

{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
foldl' fstep = foldlM' (\b a -> return (fstep b a))

------------------------------------------------------------------------------
-- Specialized Folds
------------------------------------------------------------------------------

-- | Run a streaming composition, discard the results.
{-# INLINE_LATE runStream #-}
runStream :: Monad m => Stream m a -> m ()
runStream (Stream step state) = go SPEC state
  where
    go !_ st = do
        r <- step st
        case r of
            Yield _ s -> go SPEC s
            Stop      -> return ()

{-# INLINE_NORMAL last #-}
last :: Monad m => Stream m a -> m (Maybe a)
last = foldl' (\_ y -> Just y) Nothing

{-# INLINE_NORMAL elem #-}
elem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
elem e (Stream step state) = go state
  where
    go st = do
        r <- step st
        case r of
            Yield x s ->
                if x == e
                then return True
                else go s
            Stop -> return False

{-# INLINE_NORMAL notElem #-}
notElem :: (Monad m, Eq a) => a -> Stream m a -> m Bool
notElem e (Stream step state) = go state
  where
    go st = do
        r <- step st
        case r of
            Yield x s ->
                if x == e
                then return False
                else go s
            Stop -> return True

{-# INLINE_NORMAL all #-}
all :: Monad m => (a -> Bool) -> Stream m a -> m Bool
all p (Stream step state) = go state
  where
    go st = do
        r <- step st
        case r of
            Yield x s ->
                if p x
                then go s
                else return False
            Stop -> return True

{-# INLINE_NORMAL any #-}
any :: Monad m => (a -> Bool) -> Stream m a -> m Bool
any p (Stream step state) = go state
  where
    go st = do
        r <- step st
        case r of
            Yield x s ->
                if p x
                then return True
                else go s
            Stop -> return False

{-# INLINE_NORMAL maximum #-}
maximum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
maximum (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step st
        case r of
            Yield x s -> go (Just x) s
            Stop -> return Nothing
    go (Just acc) st = do
        r <- step st
        case r of
            Yield x s ->
                if acc <= x
                then go (Just x) s
                else go (Just acc) s
            Stop -> return (Just acc)

{-# INLINE_NORMAL minimum #-}
minimum :: (Monad m, Ord a) => Stream m a -> m (Maybe a)
minimum (Stream step state) = go Nothing state
  where
    go Nothing st = do
        r <- step st
        case r of
            Yield x s -> go (Just x) s
            Stop -> return Nothing
    go (Just acc) st = do
        r <- step st
        case r of
            Yield x s ->
                if acc <= x
                then go (Just acc) s
                else go (Just x) s
            Stop -> return (Just acc)

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- | Execute a monadic action for each element of the 'Stream'
{-# INLINE_NORMAL mapM_ #-}
mapM_ :: Monad m => (a -> m b) -> Stream m a -> m ()
mapM_ m = runStream . mapM m

------------------------------------------------------------------------------
-- Converting folds
------------------------------------------------------------------------------

{-# INLINE toList #-}
toList :: Monad m => Stream m a -> m [a]
toList = foldr (:) []

-- Convert a direct stream to and from CPS encoded stream
{-# INLINE_LATE toStreamK #-}
toStreamK :: Monad m => Stream m a -> K.Stream m a
toStreamK (Stream step state) = go state
    where
    go st = K.Stream $ \_ stp _ yld -> do
        r <- step st
        case r of
            Yield x s -> yld x (go s)
            Stop      -> stp

#ifndef DISABLE_FUSION
{-# RULES "fromStreamK/toStreamK fusion"
    forall s. toStreamK (fromStreamK s) = s #-}
{-# RULES "toStreamK/fromStreamK fusion"
    forall s. fromStreamK (toStreamK s) = s #-}
#endif

------------------------------------------------------------------------------
-- Transformation by Folding (Scans)
------------------------------------------------------------------------------

{-# INLINE_NORMAL postscanlM' #-}
postscanlM' :: Monad m => (b -> a -> m b) -> b -> Stream m a -> Stream m b
postscanlM' fstep begin (Stream step state) =
    begin `seq` Stream step' (state, begin)
  where
    {-# INLINE_LATE step' #-}
    step' (st, acc) = acc `seq` do
        r <- step st
        case r of
            Yield x s -> do
                y <- fstep acc x
                y `seq` return (Yield y (s, y))
            Stop -> return Stop

{-# INLINE scanlM' #-}
scanlM' :: Monad m => (b -> a -> m b) -> b -> Stream m a -> Stream m b
scanlM' fstep begin s = begin `seq` (begin `cons` postscanlM' fstep begin s)

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE_NORMAL take #-}
take :: Monad m => Int -> Stream m a -> Stream m a
take n (Stream step state) = n `seq` Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' (st, i) | i < n = do
        r <- step st
        return $ case r of
            Yield x s -> Yield x (s, i + 1)
            Stop      -> Stop
    step' (_, _) = return Stop

{-# INLINE_NORMAL takeWhileM #-}
takeWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
takeWhileM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' st = do
        r <- step st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Stop
            Stop -> return $ Stop

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
takeWhile f = takeWhileM (return . f)

{-# INLINE_NORMAL drop #-}
drop :: Monad m => Int -> Stream m a -> Stream m a
drop n (Stream step state) = Stream step' (state, Just n)
  where
    {-# INLINE_LATE step' #-}
    step' (st, Just i)
        | i > 0 = do
            r <- step st
            case r of
               Yield _ s -> step' (s, Just (i - 1))
               Stop      -> return Stop
        | otherwise = step' (st, Nothing)

    step' (st, Nothing) =  do
        r <- step st
        return $ case r of
            Yield x s -> Yield x (s, Nothing)
            Stop      -> Stop

data DropWhileState s a
    = DropWhileDrop s
    | DropWhileYield a s
    | DropWhileNext s

{-# INLINE_NORMAL dropWhileM #-}
dropWhileM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
dropWhileM f (Stream step state) = Stream step' (DropWhileDrop state)
  where
    {-# INLINE_LATE step' #-}
    step' (DropWhileDrop st) = do
        r <- step st
        case r of
            Yield x s -> do
                b <- f x
                if b
                then step' (DropWhileDrop s)
                else step' (DropWhileYield x s)
            Stop -> return Stop

    step' (DropWhileNext st) =  do
        r <- step st
        case r of
            Yield x s -> step' (DropWhileYield x s)
            Stop      -> return Stop

    step' (DropWhileYield x st) = return $ Yield x (DropWhileNext st)

{-# INLINE dropWhile #-}
dropWhile :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
dropWhile f = dropWhileM (return . f)

{-# INLINE_NORMAL filterM #-}
filterM :: Monad m => (a -> m Bool) -> Stream m a -> Stream m a
filterM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' st = do
        r <- step st
        case r of
            Yield x s -> do
                b <- f x
                if b
                then return $ Yield x s
                else step' s
            Stop -> return $ Stop

{-# INLINE filter #-}
filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a
filter f = filterM (return . f)

------------------------------------------------------------------------------
-- Transformation by Mapping
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

{-# INLINE map #-}
map :: Monad m => (a -> b) -> Stream m a -> Stream m b
map f = mapM (return . f)

------------------------------------------------------------------------------
-- Transformation by Map and Filter
------------------------------------------------------------------------------

{-# INLINE_NORMAL mapMaybe #-}
mapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
mapMaybe f = fmap fromJust . filter isJust . map f

{-# INLINE_NORMAL mapMaybeM #-}
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Stream m a -> Stream m b
mapMaybeM f = fmap fromJust . filter isJust . mapM f

------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

instance Monad m => Functor (Stream m) where
    {-# INLINE fmap #-}
    fmap = map
