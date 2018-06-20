{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.CPS.Stream
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.CPS.Stream
    (
    -- * Streams
      Stream (..)

    -- * Construction (pure)
    , nil
    , cons
    , singleton
    , once
    , repeat

    -- * Construction (monadic)
    , consM

    -- * Semigroup Style Composition
    , serial
    , wSerial

    -- * zip
    , zipWith
    )
where

import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Semigroup (Semigroup(..))
import Prelude hiding (repeat, zipWith)

import Streamly.SVar

------------------------------------------------------------------------------
-- The stream type
------------------------------------------------------------------------------

-- | The type 'Stream m a' represents a monadic stream of values of type 'a'
-- constructed using actions in monad 'm'. It uses stop, singleton and yield
-- continuations equivalent to the following direct style type:
--
-- data Stream m a = Stop | Singleton a | Yield a (Stream m a)
--
-- To facilitate parallel composition we maintain a local state in an SVar that
-- is shared across and is used for synchronization of the streams being
-- composed.
--
-- The singleton case can be expressed in terms of stop and yield but we have
-- it as a separate case to optimize composition operations for streams with
-- single element.  We build singleton streams in the implementation of 'pure'
-- for Applicative and Monad, and in 'lift' for MonadTrans.
--
newtype Stream m a =
    Stream {
        runStream :: forall r.
               Maybe (SVar Stream m a)   -- local state
            -> m r                       -- stop
            -> (a -> m r)                -- singleton
            -> (a -> Stream m a -> m r)  -- yield
            -> m r
    }

nil :: Stream m a
nil = Stream $ \_ stp _ _ -> stp

-- | faster than consM because there is no bind.
cons :: a -> Stream m a -> Stream m a
cons a r = Stream $ \_ _ _ yld -> yld a r

-- | Same as @once . return@ but may be faster because there is no bind
singleton :: a -> Stream m a
singleton a = Stream $ \_ _ single _ -> single a

{-# INLINE once #-}
once :: Monad m => m a -> Stream m a
once m = Stream $ \_ _ single _ -> m >>= single

{-# INLINE consM #-}
consM :: Monad m => m a -> Stream m a -> Stream m a
consM m r = Stream $ \_ _ _ yld -> m >>= \a -> yld a r

repeat :: a -> Stream m a
repeat a = let x = cons a x in x

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Concatenates two streams sequentially i.e. the first stream is
-- exhausted completely before yielding any element from the second stream.
{-# INLINE serial #-}
serial :: Stream m a -> Stream m a -> Stream m a
serial m1 m2 = go m1
    where
    go (Stream m) = Stream $ \_ stp sng yld ->
            let stop      = (runStream m2) Nothing stp sng yld
                single a  = yld a m2
                yield a r = yld a (go r)
            in m Nothing stop single yield

instance Semigroup (Stream m a) where
    (<>) = serial

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (Stream m a) where
    mempty = nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Interleave
------------------------------------------------------------------------------

{-# INLINE wSerial #-}
wSerial :: Stream m a -> Stream m a -> Stream m a
wSerial m1 m2 = Stream $ \_ stp sng yld -> do
    let stop      = (runStream m2) Nothing stp sng yld
        single a  = yld a m2
        yield a r = yld a (wSerial m2 r)
    (runStream m1) Nothing stop single yield

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

instance Monad m => Functor (Stream m) where
    fmap f m = Stream $ \_ stp sng yld ->
        let single    = sng . f
            yield a r = yld (f a) (fmap f r)
        in (runStream m) Nothing stp single yield

------------------------------------------------------------------------------
-- Alternative & MonadPlus
------------------------------------------------------------------------------

_alt :: Stream m a -> Stream m a -> Stream m a
_alt m1 m2 = Stream $ \_ stp sng yld ->
    let stop  = runStream m2 Nothing stp sng yld
    in runStream m1 Nothing stop sng yld

------------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------------

{-# INLINE zipWith #-}
zipWith :: (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith f m1 m2 = go m1 m2
    where
    go mx my = Stream $ \_ stp sng yld -> do
        let merge a ra =
                let single2 b = sng (f a b)
                    yield2 b rb = yld (f a b) (go ra rb)
                 in (runStream my) Nothing stp single2 yield2
        let single1 a   = merge a nil
            yield1 a ra = merge a ra
        (runStream mx) Nothing stp single1 yield1

-------------------------------------------------------------------------------
-- Transformers
-------------------------------------------------------------------------------

instance MonadTrans Stream where
    lift = once
