{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Unfold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streams forcing a closed control flow loop can be categorized under
-- two types, unfolds and folds, both of these are duals of each other.
--
-- Unfold streams are really generators of a sequence of elements, we can also
-- call them pull style streams. These are lazy producers of streams. On each
-- evaluation the producer generates the next element.  A consumer can
-- therefore pull elements from the stream whenever it wants to.  A stream
-- consumer can multiplex pull streams by pulling elements from the chosen
-- streams, therefore, pull streams allow merging or multiplexing.  On the
-- other hand, with this representation we cannot split or demultiplex a
-- stream.  So really these are stream sources that can be generated from a
-- seed and can be merged or zipped into a single stream.
--
-- The dual of Unfolds are Folds. Folds can also be called as push style
-- streams or reducers. These are strict consumers of streams. We keep pushing
-- elements to a fold and we can extract the result at any point. A driver can
-- choose which fold to push to and can also push the same element to multiple
-- folds. Therefore, folds allow splitting or demultiplexing a stream. On the
-- other hand, we cannot merge streams using this representation. So really
-- these are stream consumers that reduce the stream to a single value, these
-- consumers can be composed such that a stream can be split over multiple
-- consumers.
--
-- Performance:
--
-- Composing a tree or graph of computations with unfolds can be much more
-- efficient compared to composing with the Monad instance.  The reason is that
-- unfolds allow the compiler to statically know the state and optimize it
-- using stream fusion whereas it is not possible with the monad bind because
-- the state is determined dynamically.

-- Open control flow style streams can also have two representations. StreamK
-- is a producer style representation. We can also have a consumer style
-- representation. We can use that for composable folds in StreamK
-- representation.
--
module Streamly.Internal.Data.Unfold
    (
    -- * Unfold Type
      Unfold

    -- * Operations on Input
    , close
    , first
    , second
    , lmap
    -- coapply
    -- comonad

    -- * Operations on Output
    , fold
    -- pipe

    -- * Unfolds
    , singleton
    , replicateM
    , fromList
    , enumerateFromStepIntegral
    , enumerateFromToIntegral
    , enumerateFromIntegral

    -- * Transformations
    , map

    -- * Filtering
    , takeWhileM
    , takeWhile
    , take
    , filter
    , filterM

    -- * Nesting
    , concat
    , concatMapM
    , outerProduct

    -- * Exceptions
    , before
    , after
    , onException
    , finally
    , bracket
    , handle
    )
where

import Control.Exception (Exception)
import Data.Void (Void)
import GHC.Types (SPEC(..))
import Prelude hiding (concat, map, takeWhile, take, filter)

import Streamly.Streams.StreamD.Type (Stream(..), Step(..))
#if __GLASGOW_HASKELL__ < 800
import Streamly.Streams.StreamD.Type (pattern Stream)
#endif
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.SVar (defState)
import Control.Monad.Catch (MonadCatch)

import qualified Control.Monad.Catch as MC

-------------------------------------------------------------------------------
-- Input operations
-------------------------------------------------------------------------------

-- | Supply the seed to an unfold closing the input end of the unfold.
--
-- /Internal/
--
{-# INLINE_NORMAL close #-}
close :: Unfold m a b -> a -> Unfold m Void b
close (Unfold ustep uinject) a = Unfold ustep (const $ uinject a)

-- XXX rename to closeFst/closeSnd
--
-- | Supply the first component of the tuple to an unfold that accepts a tuple
-- as a seed resulting in a fold that accepts the second component of the tuple
-- as a seed.
--
-- /Internal/
--
{-# INLINE_NORMAL first #-}
first :: Unfold m (a, b) c -> a -> Unfold m b c
first (Unfold ustep uinject) a = Unfold ustep (\b -> uinject (a, b))

-- | Supply the second component of the tuple to an unfold that accepts a tuple
-- as a seed resulting in a fold that accepts the first component of the tuple
-- as a seed.
--
-- /Internal/
--
{-# INLINE_NORMAL second #-}
second :: Unfold m (a, b) c -> b -> Unfold m a c
second (Unfold ustep uinject) b = Unfold ustep (\a -> uinject (a, b))

{-# INLINE_NORMAL lmap #-}
lmap :: (a -> c) -> Unfold m c b -> Unfold m a b
lmap f (Unfold ustep uinject) = Unfold ustep (\x -> uinject $ f x)

-------------------------------------------------------------------------------
-- Output operations
-------------------------------------------------------------------------------

-- | Compose an 'Unfold' and a 'Fold'. Given an @Unfold m a b@ and a
-- @Fold m b c@, returns a monadic action @a -> m c@ representing the
-- application of the fold on the unfolded stream.
--
-- /Internal/
--
{-# INLINE_NORMAL fold #-}
fold :: Monad m => Unfold m a b -> Fold m b c -> a -> m c
fold (Unfold ustep inject) (Fold fstep initial extract) a =
    initial >>= \x -> inject a >>= go SPEC x
  where
    -- XXX !acc?
    {-# INLINE_LATE go #-}
    go !_ acc st = acc `seq` do
        r <- ustep st
        case r of
            Yield x s -> do
                acc' <- fstep acc x
                go SPEC acc' s
            Skip s -> go SPEC acc s
            Stop   -> extract acc

{-# INLINE_NORMAL map #-}
map :: Monad m => (b -> c) -> Unfold m a b -> Unfold m a c
map f (Unfold ustep uinject) = Unfold step uinject
    where
    {-# INLINE_LATE step #-}
    step st = do
        r <- ustep st
        return $ case r of
            Yield x s -> Yield (f x) s
            Skip s    -> Skip s
            Stop      -> Stop

-------------------------------------------------------------------------------
-- Unfolds
-------------------------------------------------------------------------------

-- | Identity unfold. Generates a singleton stream with the seed as the only
-- element in the stream.
--
-- > singleton = replicateM 1
--
{-# INLINE singleton #-}
singleton :: Monad m => Unfold m a a
singleton = Unfold step inject
    where
    inject x = return $ Just x
    {-# INLINE_LATE step #-}
    step (Just x) = return $ Yield x Nothing
    step Nothing = return Stop

-- | Generates a stream replicating the seed @n@ times.
--
{-# INLINE replicateM #-}
replicateM :: Monad m => Int -> Unfold m a a
replicateM n = Unfold step inject
    where
    inject x = return (x, n)
    {-# INLINE_LATE step #-}
    step (x, i) = return $
        if i <= 0
        then Stop
        else Yield x (x, (i - 1))

-- | Convert a list of pure values to a 'Stream'
{-# INLINE_LATE fromList #-}
fromList :: Monad m => Unfold m [a] a
fromList = Unfold step inject
  where
    inject x = return x
    {-# INLINE_LATE step #-}
    step (x:xs) = return $ Yield x xs
    step []     = return Stop

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE_NORMAL take #-}
take :: Monad m => Int -> Unfold m a b -> Unfold m a b
take n (Unfold step1 inject1) = Unfold step inject
  where
    inject x = do
        s <- inject1 x
        return (s, 0)
    {-# INLINE_LATE step #-}
    step (st, i) | i < n = do
        r <- step1 st
        return $ case r of
            Yield x s -> Yield x (s, i + 1)
            Skip s -> Skip (s, i)
            Stop   -> Stop
    step (_, _) = return Stop

{-# INLINE_NORMAL takeWhileM #-}
takeWhileM :: Monad m => (b -> m Bool) -> Unfold m a b -> Unfold m a b
takeWhileM f (Unfold step1 inject1) = Unfold step inject1
  where
    {-# INLINE_LATE step #-}
    step st = do
        r <- step1 st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Stop
            Skip s -> return $ Skip s
            Stop   -> return Stop

{-# INLINE takeWhile #-}
takeWhile :: Monad m => (b -> Bool) -> Unfold m a b -> Unfold m a b
takeWhile f = takeWhileM (return . f)

{-# INLINE_NORMAL filterM #-}
filterM :: Monad m => (b -> m Bool) -> Unfold m a b -> Unfold m a b
filterM f (Unfold step1 inject1) = Unfold step inject1
  where
    {-# INLINE_LATE step #-}
    step st = do
        r <- step1 st
        case r of
            Yield x s -> do
                b <- f x
                return $ if b then Yield x s else Skip s
            Skip s -> return $ Skip s
            Stop   -> return Stop

{-# INLINE filter #-}
filter :: Monad m => (b -> Bool) -> Unfold m a b -> Unfold m a b
filter f = filterM (return . f)

-------------------------------------------------------------------------------
-- Enumeration
-------------------------------------------------------------------------------

-- | Can be used to enumerate unbounded integrals. This does not check for
-- overflow or underflow for bounded integrals.
{-# INLINE_NORMAL enumerateFromStepIntegral #-}
enumerateFromStepIntegral :: (Integral a, Monad m) => Unfold m (a, a) a
enumerateFromStepIntegral = Unfold step inject
    where
    inject (from, stride) = from `seq` stride `seq` return (from, stride)
    {-# INLINE_LATE step #-}
    step !(x, stride) = return $ Yield x $! (x + stride, stride)

-- We are assuming that "to" is constrained by the type to be within
-- max/min bounds.
{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: (Monad m, Integral a) => a -> Unfold m a a
enumerateFromToIntegral to =
    takeWhile (<= to) $ second enumerateFromStepIntegral 1

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: (Monad m, Integral a, Bounded a) => Unfold m a a
enumerateFromIntegral = enumerateFromToIntegral maxBound

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

data ConcatState s1 s2 = ConcatOuter s1 | ConcatInner s1 s2

{-# INLINE_NORMAL concat #-}
concat :: Monad m => Unfold m a b -> Unfold m b c -> Unfold m a c
concat (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject
    where
    inject x = do
        s <- inject1 x
        return $ ConcatOuter s

    {-# INLINE_LATE step #-}
    step (ConcatOuter st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                innerSt <- inject2 x
                return $ Skip (ConcatInner s innerSt)
            Skip s    -> return $ Skip (ConcatOuter s)
            Stop      -> return Stop

    step (ConcatInner ost ist) = do
        r <- step2 ist
        return $ case r of
            Yield x s -> Yield x (ConcatInner ost s)
            Skip s    -> Skip (ConcatInner ost s)
            Stop      -> Skip (ConcatOuter ost)

data OuterProductState s1 s2 sy x y =
    OuterProductOuter s1 y | OuterProductInner s1 sy s2 x

{-# INLINE_NORMAL outerProduct #-}
outerProduct :: Monad m
    => Unfold m a b -> Unfold m c d -> Unfold m (a, c) (b, d)
outerProduct (Unfold step1 inject1) (Unfold step2 inject2) = Unfold step inject
    where
    inject (x, y) = do
        s1 <- inject1 x
        return $ OuterProductOuter s1 y

    {-# INLINE_LATE step #-}
    step (OuterProductOuter st1 sy) = do
        r <- step1 st1
        case r of
            Yield x s -> do
                s2 <- inject2 sy
                return $ Skip (OuterProductInner s sy s2 x)
            Skip s    -> return $ Skip (OuterProductOuter s sy)
            Stop      -> return Stop

    step (OuterProductInner ost sy ist x) = do
        r <- step2 ist
        return $ case r of
            Yield y s -> Yield (x, y) (OuterProductInner ost sy s x)
            Skip s    -> Skip (OuterProductInner ost sy s x)
            Stop      -> Skip (OuterProductOuter ost sy)

-- XXX This can be used to implement a Monad instance for "Unfold m ()".

data ConcatMapState s1 s2 = ConcatMapOuter s1 | ConcatMapInner s1 s2

{-# INLINE_NORMAL concatMapM #-}
concatMapM :: Monad m
    => (b -> m (Unfold m () c)) -> Unfold m a b -> Unfold m a c
concatMapM f (Unfold step1 inject1) = Unfold step inject
    where
    inject x = do
        s <- inject1 x
        return $ ConcatMapOuter s

    {-# INLINE_LATE step #-}
    step (ConcatMapOuter st) = do
        r <- step1 st
        case r of
            Yield x s -> do
                Unfold step2 inject2 <- f x
                innerSt <- inject2 ()
                return $ Skip (ConcatMapInner s (Stream (\_ ss -> step2 ss)
                                                        innerSt))
            Skip s    -> return $ Skip (ConcatMapOuter s)
            Stop      -> return Stop

    step (ConcatMapInner ost (Stream istep ist)) = do
        r <- istep defState ist
        return $ case r of
            Yield x s -> Yield x (ConcatMapInner ost (Stream istep s))
            Skip s    -> Skip (ConcatMapInner ost (Stream istep s))
            Stop      -> Skip (ConcatMapOuter ost)

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Run a side effect before the unfold yields its first element.
--
-- /Internal/
{-# INLINE_NORMAL before #-}
before :: Monad m => (a -> m c) -> Unfold m a b -> Unfold m a b
before action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        _ <- action x
        st <- inject1 x
        return st

    {-# INLINE_LATE step #-}
    step st = do
        res <- step1 st
        case res of
            Yield x s -> return $ Yield x s
            Skip s    -> return $ Skip s
            Stop      -> return Stop

-- | Run a side effect whenever the unfold stops normally.
--
-- /Internal/
{-# INLINE_NORMAL after #-}
after :: Monad m => (a -> m c) -> Unfold m a b -> Unfold m a b
after action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return (s, x)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> action v >> return Stop

-- | Run a side effect whenever the unfold aborts due to an exception.
--
-- /Internal/
{-# INLINE_NORMAL onException #-}
onException :: MonadCatch m => (a -> m c) -> Unfold m a b -> Unfold m a b
onException action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return (s, x)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st `MC.onException` action v
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> return Stop

-- | Run a side effect whenever the unfold stops normally or aborts due to an
-- exception.
--
-- /Internal/
{-# INLINE_NORMAL finally #-}
finally :: MonadCatch m => (a -> m c) -> Unfold m a b -> Unfold m a b
finally action (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        s <- inject1 x
        return (s, x)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st `MC.onException` action v
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> action v >> return Stop

-- | @bracket before after between@ runs the @before@ action and then unfolds
-- its output using the @between@ unfold. When the @between@ unfold is done or
-- if an exception occurs then the @after@ action is run with the output of
-- @before@ as argument.
--
-- /Internal/
{-# INLINE_NORMAL bracket #-}
bracket :: MonadCatch m
    => (a -> m c) -> (c -> m d) -> Unfold m c b -> Unfold m a b
bracket bef aft (Unfold step1 inject1) = Unfold step inject

    where

    inject x = do
        r <- bef x
        s <- inject1 r
        return (s, r)

    {-# INLINE_LATE step #-}
    step (st, v) = do
        res <- step1 st `MC.onException` aft v
        case res of
            Yield x s -> return $ Yield x (s, v)
            Skip s    -> return $ Skip (s, v)
            Stop      -> aft v >> return Stop

-- | When unfolding an unfold if an exception occurs, unfold aborts
-- and the specified exception handler is run with the exception as argument.
--
-- /Internal/
{-# INLINE_NORMAL handle #-}
handle :: (MonadCatch m, Exception e)
    => (e -> m b) -> Unfold m a b -> Unfold m a b
handle f (Unfold step1 inject1) = Unfold step inject

    where

    inject x = inject1 x >>= return . Just

    {-# INLINE_LATE step #-}
    step (Just st) = do
        res <- MC.try $ step1 st
        case res of
            Left e -> f e >>= \x -> return (Yield x Nothing)
            Right r -> case r of
                Yield x s -> return $ Yield x (Just s)
                Skip s    -> return $ Skip (Just s)
                Stop      -> return Stop
    step Nothing = return Stop
