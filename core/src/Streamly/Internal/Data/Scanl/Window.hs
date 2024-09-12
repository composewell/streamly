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
    -- * Types
      Incr (..)

    -- * Running Incremental Scans
    -- | Scans of type @Scanl m (Incr a) b@ are incremental sliding window
    -- scans and are prefixed with @incr@. An input of type @(Insert a)@
    -- indicates that the input element @a@ is being inserted in the window
    -- without ejecting an old value increasing the window size by 1. An input
    -- of type @(Replace a a)@ indicates that the first element is being inserted
    -- in the window and the second element is being removed from the window,
    -- the window size remains the same. The window size can only increase and
    -- never decrease.
    --
    -- You can compute the statistics over the entire stream using window folds
    -- by always supplying input of type @Insert a@.
    --
    -- The incremental scans are converted into scans over a window using the
    -- 'windowScan' operation which maintains a sliding window and supplies the
    -- new and/or exiting element of the window to the window scan in the form
    -- of an incremental operation. The names of window scans are prefixed with
    -- @window@.
    --
    , cumulativeScan
    , windowScan
    , windowScanWith

    -- * Incremental Scans

    , incrRollingMap -- XXX remove?
    , incrRollingMapM -- XXX remove?

    -- ** Sums
    , incrCount
    , incrSum
    , incrSumInt
    , incrPowerSum
    , incrPowerSumFrac

    -- ** Location
    , windowMinimum
    , windowMaximum
    , windowRange
    , incrMean
    )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
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
-- Incremental operations
-------------------------------------------------------------------------------

-- The delete operation could be quite useful e.g. if we are computing stats
-- over last one hour of trades. The window would be growing when trade
-- frequency is increasing, the window would remain constant when the trade
-- frequency is steady, but it would shrink when the trade frequency reduces.
-- If no trades are happening our clock would still be ticking and to maintain
-- a 1 hour window we would be ejecting the oldest elements from the window
-- even without any other elements entering the window. In fact, it is required
-- for time based windows.
--
-- Replace can be implemented using Insert and Delete.

data Incr a =
      Insert !a
    -- | Delete !a
    | Replace !a !a

instance Functor Incr where
    fmap f (Insert x) = Insert (f x)
    -- fmap f (Delete x) = Delete (f x)
    fmap f (Replace x y) = Replace (f x) (f y)

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
    => Int -> Scanl m (Incr a, m (MutArray a)) b -> Scanl m a b
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
            r <- step1 st (Insert a, action)
            return $
                case r of
                    Partial s -> Partial $ Tuple4' rb rh1 (i + 1) s
                    Done b -> Done b
        | otherwise = do
            old <- Ring.unsafeGetIndex rh rb
            rh1 <- liftIO $ Ring.unsafeInsert rb rh a
            r <- step1 st (Replace a old, toArray Ring.unsafeFoldRingFullM rb rh1)
            return $
                case r of
                    Partial s -> Partial $ Tuple4' rb rh1 (i + 1) s
                    Done b -> Done b

    extract (Tuple4' _ _ _ st) = extract1 st

    final (Tuple4' _ _ _ st) = final1 st

-- | @windowScan collector@ is an incremental sliding window scan that does not
-- require all the intermediate elements in a computation. This maintains @n@
-- elements in the window, when a new element comes it slides out the oldest
-- element and the new element along with the old element are supplied to the
-- collector fold.
--
{-# INLINE windowScan #-}
windowScan :: forall m a b. (MonadIO m, Unbox a)
    => Int -> Scanl m (Incr a) b -> Scanl m a b
windowScan n f = windowScanWith n (Scanl.lmap fst f)

-- | Convert an incremental scan to a cumulative scan using the entire input
-- stream as a single window.
--
-- >>> cumulativeScan = Scanl.lmap Insert
--
{-# INLINE cumulativeScan #-}
cumulativeScan :: Scanl m (Incr a) b -> Scanl m a b
cumulativeScan = Scanl.lmap Insert

-- | Apply an effectful function on the entering and the exiting element of the
-- window. The first argument of the mapped function is the exiting element and
-- the second argument is the entering element.
{-# INLINE incrRollingMapM #-}
incrRollingMapM :: Monad m =>
    (Maybe a -> a -> m (Maybe b)) -> Scanl m (Incr a) (Maybe b)
incrRollingMapM f = Scanl.mkScanlM f1 initial

    where

    initial = return Nothing

    f1 _ (Insert a) = f Nothing a
    -- f1 _ (Delete _) = return Nothing
    f1 _ (Replace x y) = f (Just y) x

-- | Apply a pure function on the latest and the oldest element of the window.
--
-- >>> incrRollingMap f = Scanl.incrRollingMapM (\x y -> return $ f x y)
--
{-# INLINE incrRollingMap #-}
incrRollingMap :: Monad m =>
    (Maybe a -> a -> Maybe b) -> Scanl m (Incr a) (Maybe b)
incrRollingMap f = Scanl.mkScanl f1 initial

    where

    initial = Nothing

    f1 _ (Insert a) = f Nothing a
    -- f1 _ (Delete _) = Nothing
    f1 _ (Replace x y) = f (Just y) x

-------------------------------------------------------------------------------
-- Sum
-------------------------------------------------------------------------------

-- XXX Overflow.

-- | The sum of all the elements in a rolling window. The input elements are
-- required to be integral numbers.
--
-- This was written in the hope that it would be a tiny bit faster than 'incrSum'
-- for 'Integral' values. But turns out that 'incrSum' is 2% faster than this even
-- for integral values!
--
-- /Internal/
--
{-# INLINE incrSumInt #-}
incrSumInt :: forall m a. (Monad m, Integral a) => Scanl m (Incr a) a
incrSumInt = Scanl step initial extract extract

    where

    initial = return $ Partial (0 :: a)

    step s (Insert a) = return $ Partial (s + a)
    -- step s (Delete a) = return $ Partial (s - a)
    step s (Replace new old) = return $ Partial (s + new - old)

    extract = return

-- XXX Overflow.

-- | Sum of all the elements in a rolling window:
--
-- \(S = \sum_{i=1}^n x_{i}\)
--
-- This is the first power sum.
--
-- >>> incrSum = Scanl.incrPowerSum 1
--
-- Uses Kahan-Babuska-Neumaier style summation for numerical stability of
-- floating precision arithmetic.
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
--
{-# INLINE incrSum #-}
incrSum :: forall m a. (Monad m, Num a) => Scanl m (Incr a) a
incrSum = Scanl step initial extract extract

    where

    initial =
        return
            $ Partial
            $ Tuple'
                (0 :: a) -- running sum
                (0 :: a) -- accumulated rounding error

    add total incr =
        let
            -- total is large and incr may be small, we may round incr here but
            -- we will accumulate the rounding error in err1 in the next step.
            total1 = total + incr
            -- Accumulate any rounding error in err1
            -- XXX In the Insert case we may lose err, therefore we
            -- should use ((total1 - total) - new) + err here.
            -- Or even in the Replace case if (new - old) is large we may lose
            -- err, so we should use ((total1 - total) + (old - new)) + err.
            err1 = (total1 - total) - incr
        in return $ Partial $ Tuple' total1 err1

    step (Tuple' total err) (Insert new) =
        -- XXX if new is large we may lose err
        let incr = new - err
         in add total incr
    {-
    step (Tuple' total err) (Delete new) =
        -- XXX if new is large we may lose err
        let incr = -new - err
         in add total incr
    -}
    step (Tuple' total err) (Replace new old) =
        -- XXX if (new - old) is large we may lose err
        let incr = (new - old) - err
         in add total incr

    extract (Tuple' total _) = return total

-- | The number of elements in the rolling window.
--
-- This is the \(0\)th power sum.
--
-- >>> incrCount = Scanl.incrPowerSum 0
--
{-# INLINE incrCount #-}
incrCount :: (Monad m, Num b) => Scanl m (Incr a) b
incrCount = Scanl.mkScanl step 0

    where

    step w (Insert _) = w + 1
    -- step w (Delete _) = w - 1
    step w (Replace _ _) = w

-- | Sum of the \(k\)th power of all the elements in a rolling window:
--
-- \(S_k = \sum_{i=1}^n x_{i}^k\)
--
-- >>> incrPowerSum k = Scanl.lmap (fmap (^ k)) Scanl.incrSum
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE incrPowerSum #-}
incrPowerSum :: (Monad m, Num a) => Int -> Scanl m (Incr a) a
incrPowerSum k = Scanl.lmap (fmap (^ k)) incrSum

-- | Like 'incrPowerSum' but powers can be negative or fractional. This is
-- slower than 'incrPowerSum' for positive intergal powers.
--
-- >>> incrPowerSumFrac p = Scanl.lmap (fmap (** p)) Scanl.incrSum
--
{-# INLINE incrPowerSumFrac #-}
incrPowerSumFrac :: (Monad m, Floating a) => a -> Scanl m (Incr a) a
incrPowerSumFrac p = Scanl.lmap (fmap (** p)) incrSum

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

-- XXX Returns NaN on empty stream.

-- | Arithmetic mean of elements in a sliding window:
--
-- \(\mu = \frac{\sum_{i=1}^n x_{i}}{n}\)
--
-- This is also known as the Simple Moving Average (SMA) when used in the
-- sliding window and Cumulative Moving Avergae (CMA) when used on the entire
-- stream.
--
-- >>> incrMean = Scanl.teeWith (/) Scanl.incrSum Scanl.incrCount
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE incrMean #-}
incrMean :: forall m a. (Monad m, Fractional a) => Scanl m (Incr a) a
incrMean = Scanl.teeWith (/) incrSum incrCount
