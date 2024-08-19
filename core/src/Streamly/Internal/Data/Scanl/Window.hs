{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Scanl.Window
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Simple incremental statistical measures over a stream of data. All
-- operations use numerically stable floating point arithmetic.
--
-- Measurements can be performed over the entire input stream or on a sliding
-- window of fixed or variable size.  Where possible, measures are computed
-- online without buffering the input stream.
--
-- Currently there is no overflow detection.
--
-- For more advanced statistical measures see the @streamly-statistics@
-- package.

-- XXX A window fold can be driven either using the Ring.slidingWindow
-- combinator or by zipping nthLast fold and last fold.

module Streamly.Internal.Data.Scanl.Window
    (
    -- * Incremental Folds
    -- | Folds of type @Fold m (a, Maybe a) b@ are incremental sliding window
    -- folds. An input of type @(a, Nothing)@ indicates that the input element
    -- @a@ is being inserted in the window without ejecting an old value
    -- increasing the window size by 1. An input of type @(a, Just a)@
    -- indicates that the first element is being inserted in the window and the
    -- second element is being removed from the window, the window size remains
    -- the same. The window size can only increase and never decrease.
    --
    -- You can compute the statistics over the entire stream using sliding
    -- window folds by keeping the second element of the input tuple as
    -- @Nothing@.
    --
      windowLmap
    , cumulative

    , windowScan
    , windowScanWith

    , windowRollingMap
    , windowRollingMapM

    -- ** Sums
    , windowLength
    , windowSum
    , windowSumInt
    , windowPowerSum
    , windowPowerSumFrac

    -- ** Location
    , windowMinimum
    , windowMaximum
    , windowRange
    , windowMean
    )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor(bimap)
import Streamly.Internal.Data.MutArray.Type (MutArray)
import Streamly.Internal.Data.Unbox (Unbox(..))

import Streamly.Internal.Data.Scanl.Type (Scanl(..), Step(..))
import Streamly.Internal.Data.Tuple.Strict
    (Tuple'(..), Tuple3Fused' (Tuple3Fused'))

import qualified Streamly.Internal.Data.MutArray.Type as MutArray
import qualified Streamly.Internal.Data.Ring as Ring
import qualified Streamly.Internal.Data.Scanl.Type as Scanl

import Prelude hiding (length, sum, minimum, maximum)

#include "DocTestDataScanl.hs"

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

data Tuple4' a b c d = Tuple4' !a !b !c !d deriving Show

-- | Like 'windowScan' but also provides the entire ring contents as an Array.
-- The array reflects the state of the ring after inserting the incoming
-- element.
--
-- IMPORTANT NOTE: The ring is mutable, therefore, the result of @(m (Array
-- a))@ action depends on when it is executed. It does not capture the snapshot
-- of the ring at a particular time.
{-# INLINE windowScanWith #-}
windowScanWith :: forall m a b. (MonadIO m, Unbox a)
    => Int -> Scanl m ((a, Maybe a), m (MutArray a)) b -> Scanl m a b
windowScanWith n (Scanl step1 initial1 extract1 final1) =
    Scanl step initial extract final

    where

    initial = do
        if n <= 0
        then error "Window size must be > 0"
        else do
            r <- initial1
            rb <- liftIO $ Ring.emptyOf n
            return $
                case r of
                    Partial s -> Partial $ Tuple4' rb 0 (0 :: Int) s
                    Done b -> Done b

    toArray foldRing rb rh = do
        -- Using unpinned array here instead of pinned
        arr <- liftIO $ MutArray.emptyOf n
        let snoc' b a = liftIO $ MutArray.unsafeSnoc b a
        foldRing rh snoc' arr rb

    step (Tuple4' rb rh i st) a
        | i < n = do
            rh1 <- liftIO $ Ring.unsafeInsert rb rh a
            -- NOTE: We use (rh + 1) instead of rh1 in the code below as if we
            -- are at the last element of the ring, rh1 would become 0 and we
            -- would not fold anything.
            let action = toArray Ring.unsafeFoldRingM rb (rh + 1)
            r <- step1 st ((a, Nothing), action)
            return $
                case r of
                    Partial s -> Partial $ Tuple4' rb rh1 (i + 1) s
                    Done b -> Done b
        | otherwise = do
            old <- Ring.unsafeGetIndex rh rb
            rh1 <- liftIO $ Ring.unsafeInsert rb rh a
            r <- step1 st ((a, Just old), toArray Ring.unsafeFoldRingFullM rb rh1)
            return $
                case r of
                    Partial s -> Partial $ Tuple4' rb rh1 (i + 1) s
                    Done b -> Done b

    extract (Tuple4' _ _ _ st) = extract1 st

    final (Tuple4' _ _ _ st) = final1 st

-- | @windowScan collector@ is an incremental sliding window fold that does not
-- require all the intermediate elements in a computation. This maintains @n@
-- elements in the window, when a new element comes it slides out the oldest
-- element and the new element along with the old element are supplied to the
-- collector fold.
--
-- The 'Maybe' type is for the case when initially the window is filling and
-- there is no old element.
--
{-# INLINE windowScan #-}
windowScan :: forall m a b. (MonadIO m, Unbox a)
    => Int -> Scanl m (a, Maybe a) b -> Scanl m a b
windowScan n f = windowScanWith n (Scanl.lmap fst f)

-- | Map a function on the incoming as well as outgoing element of a rolling
-- window fold.
--
-- >>> lmap f = Scanl.lmap (bimap f (f <$>))
--
{-# INLINE windowLmap #-}
windowLmap :: (c -> a) -> Scanl m (a, Maybe a) b -> Scanl m (c, Maybe c) b
windowLmap f = Scanl.lmap (bimap f (f <$>))

-- | Convert an incremental fold to a cumulative fold using the entire input
-- stream as a single window.
--
-- >>> cumulative f = Scanl.lmap (\x -> (x, Nothing)) f
--
{-# INLINE cumulative #-}
cumulative :: Scanl m (a, Maybe a) b -> Scanl m a b
cumulative = Scanl.lmap (, Nothing)

-- XXX Exchange the first two arguments of rollingMap or exchange the order in
-- the fold input tuple.

-- | Apply an effectful function on the latest and the oldest element of the
-- window.
{-# INLINE windowRollingMapM #-}
windowRollingMapM :: Monad m =>
    (Maybe a -> a -> m (Maybe b)) -> Scanl m (a, Maybe a) (Maybe b)
windowRollingMapM f = Scanl.mkScanlM f1 initial

    where

    initial = return Nothing

    f1 _ (a, ma) = f ma a

-- | Apply a pure function on the latest and the oldest element of the window.
--
-- >>> windowRollingMap f = Scanl.windowRollingMapM (\x y -> return $ f x y)
--
{-# INLINE windowRollingMap #-}
windowRollingMap :: Monad m =>
    (Maybe a -> a -> Maybe b) -> Scanl m (a, Maybe a) (Maybe b)
windowRollingMap f = Scanl.mkScanl f1 initial

    where

    initial = Nothing

    f1 _ (a, ma) = f ma a

-------------------------------------------------------------------------------
-- Sum
-------------------------------------------------------------------------------

-- XXX Overflow.

-- | The sum of all the elements in a rolling window. The input elements are
-- required to be intergal numbers.
--
-- This was written in the hope that it would be a tiny bit faster than 'sum'
-- for 'Integral' values. But turns out that 'sum' is 2% faster than this even
-- for intergal values!
--
-- /Internal/
--
{-# INLINE windowSumInt #-}
windowSumInt :: forall m a. (Monad m, Integral a) => Scanl m (a, Maybe a) a
windowSumInt = Scanl step initial extract extract

    where

    initial = return $ Partial (0 :: a)

    step s (a, ma) =
        return
            $ Partial
                $ case ma of
                    Nothing -> s + a
                    Just old -> s + a - old

    extract = return

-- XXX Overflow.

-- | Sum of all the elements in a rolling window:
--
-- \(S = \sum_{i=1}^n x_{i}\)
--
-- This is the first power sum.
--
-- >>> sum = Scanl.windowPowerSum 1
--
-- Uses Kahan-Babuska-Neumaier style summation for numerical stability of
-- floating precision arithmetic.
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
--
{-# INLINE windowSum #-}
windowSum :: forall m a. (Monad m, Num a) => Scanl m (a, Maybe a) a
windowSum = Scanl step initial extract extract

    where

    initial =
        return
            $ Partial
            $ Tuple'
                (0 :: a) -- running sum
                (0 :: a) -- accumulated rounding error

    step (Tuple' total err) (new, mOld) =
        let incr =
                case mOld of
                    -- XXX new may be large and err may be small we may lose it
                    Nothing -> new - err
                    -- XXX if (new - old) is large we may lose err
                    Just old -> (new - old) - err
            -- total is large and incr may be small, we may round incr here but
            -- we will accumulate the rounding error in err1 in the next step.
            total1 = total + incr
            -- Accumulate any rounding error in err1
            -- XXX In the Nothing case above we may lose err, therefore we
            -- should use ((total1 - total) - new) + err here.
            -- Or even in the just case if (new - old) is large we may lose
            -- err, so we should use ((total1 - total) + (old - new)) + err.
            err1 = (total1 - total) - incr
        in return $ Partial $ Tuple' total1 err1

    extract (Tuple' total _) = return total

-- | The number of elements in the rolling window.
--
-- This is the \(0\)th power sum.
--
-- >>> length = Scanl.windowPowerSum 0
--
{-# INLINE windowLength #-}
windowLength :: (Monad m, Num b) => Scanl m (a, Maybe a) b
windowLength = Scanl.mkScanl step 0

    where

    step w (_, Nothing) = w + 1
    step w _ = w

-- | Sum of the \(k\)th power of all the elements in a rolling window:
--
-- \(S_k = \sum_{i=1}^n x_{i}^k\)
--
-- >>> windowPowerSum k = Scanl.windowLmap (^ k) Scanl.windowSum
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE windowPowerSum #-}
windowPowerSum :: (Monad m, Num a) => Int -> Scanl m (a, Maybe a) a
windowPowerSum k = windowLmap (^ k) windowSum

-- | Like 'powerSum' but powers can be negative or fractional. This is slower
-- than 'powerSum' for positive intergal powers.
--
-- >>> windowPowerSumFrac p = Scanl.windowLmap (** p) Scanl.windowSum
--
{-# INLINE windowPowerSumFrac #-}
windowPowerSumFrac :: (Monad m, Floating a) => a -> Scanl m (a, Maybe a) a
windowPowerSumFrac p = windowLmap (** p) windowSum

-------------------------------------------------------------------------------
-- Location
-------------------------------------------------------------------------------

-- XXX Remove MonadIO constraint

-- | Determine the maximum and minimum in a rolling window.
--
-- If you want to compute the range of the entire stream @Scanl.teeWith (,)
-- Scanl.maximum Scanl.minimum@ would be much faster.
--
-- /Space/: \(\mathcal{O}(n)\) where @n@ is the window size.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE windowRange #-}
windowRange :: (MonadIO m, Unbox a, Ord a) => Int -> Scanl m a (Maybe (a, a))
windowRange n = Scanl step initial extract extract

    where

    -- XXX Use Ring unfold and then fold for composing maximum and minimum to
    -- get the range.

    initial =
        if n <= 0
        then error "range: window size must be > 0"
        else
            let f a = Partial $ Tuple3Fused' a 0 (0 :: Int)
             in fmap f $ liftIO $ Ring.emptyOf n

    step (Tuple3Fused' rb rh i) a = do
        rh1 <- liftIO $ Ring.unsafeInsert rb rh a
        return $ Partial $ Tuple3Fused' rb rh1 (i + 1)

    -- XXX We need better Ring array APIs so that we can unfold the ring to a
    -- stream and fold the stream using a fold of our choice.
    --
    -- We could just scan the stream to get a stream of ring buffers and then
    -- map required folds over those, but we need to be careful that all those
    -- rings refer to the same mutable ring, therefore, downstream needs to
    -- process those strictly before it can change.
    foldFunc i
        | i < n = Ring.unsafeFoldRingM
        | otherwise = Ring.unsafeFoldRingFullM

    extract (Tuple3Fused' rb rh i) =
        if i == 0
        then return Nothing
        else do
            -- Here we use "ringStart" over "ringHead" as "ringHead" will be
            -- uninitialized if the ring is not full.
            -- Using "unsafeForeignPtrToPtr" here is safe as we touch the ring
            -- again in "foldFunc".
            x <- liftIO $ peekAt 0 (Ring.ringContents rb)
            let accum (mn, mx) a = return (min mn a, max mx a)
            fmap Just $ foldFunc i rh accum (x, x) rb

-- | Find the minimum element in a rolling window.
--
-- This implementation traverses the entire window buffer to compute the
-- minimum whenever we demand it.  It performs better than the dequeue based
-- implementation in @streamly-statistics@ package when the window size is
-- small (< 30).
--
-- If you want to compute the minimum of the entire stream
-- 'Streamly.Data.Scanl.minimum' is much faster.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE windowMinimum #-}
windowMinimum :: (MonadIO m, Unbox a, Ord a) => Int -> Scanl m a (Maybe a)
windowMinimum n = fmap (fmap fst) $ windowRange n

-- | The maximum element in a rolling window.
--
-- See the performance related comments in 'minimum'.
--
-- If you want to compute the maximum of the entire stream 'Scanl.maximum' would
-- be much faster.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE windowMaximum #-}
windowMaximum :: (MonadIO m, Unbox a, Ord a) => Int -> Scanl m a (Maybe a)
windowMaximum n = fmap (fmap snd) $ windowRange n

-- | Arithmetic mean of elements in a sliding window:
--
-- \(\mu = \frac{\sum_{i=1}^n x_{i}}{n}\)
--
-- This is also known as the Simple Moving Average (SMA) when used in the
-- sliding window and Cumulative Moving Avergae (CMA) when used on the entire
-- stream.
--
-- >>> mean = Scanl.teeWith (/) Scanl.windowSum Scanl.windowLength
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE windowMean #-}
windowMean :: forall m a. (Monad m, Fractional a) => Scanl m (a, Maybe a) a
windowMean = Scanl.teeWith (/) windowSum windowLength
