module Streamly.Internal.Data.Fold.Window
( lmap
, length
, whole
, sum
, sumInt
, minimum
, maximum
, range
)
where

import Data.Bifunctor(bimap)
import Data.Function ((&))
import Data.Maybe (fromMaybe)

import Streamly.Internal.Data.Fold.Type (Fold(..), Step(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))

import Prelude hiding (length, sum, minimum, maximum)

import qualified Deque.Strict as DQ
import qualified Streamly.Data.Fold as Fold

-- $setup
-- >>> import Data.Bifunctor(bimap)
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import Prelude hiding (length, sum, minimum, maximum)

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | Map a function on the incoming as well as outgoing element of a rolling
-- window fold.
--
-- >>> lmap f = Fold.lmap (bimap f (f <$>))
--
{-# INLINE lmap #-}
lmap :: (c -> a) -> Fold m (a, Maybe a) b -> Fold m (c, Maybe c) b
lmap f = Fold.lmap (bimap f (f <$>))

-- | Convert a rolling fold to a normal fold using the entire input stream as a
-- single window.
--
-- >>> whole f = Fold.lmap (\x -> (x, Nothing)) f
--
{-# INLINE whole #-}
whole :: Fold m (a, Maybe a) b -> Fold m a b
whole = Fold.lmap (, Nothing)

-------------------------------------------------------------------------------
-- Sum
-------------------------------------------------------------------------------

-- XXX Overflow.
--
-- | The sum of all the elements in a rolling window. The input elements are
-- required to be intergal numbers.
--
-- This was written in the hope that it would be a tiny bit faster than 'sum'
-- for 'Integral' values. But turns out that 'sum' is 2% faster than this even
-- for intergal values!
--
-- /Internal/
--
{-# INLINE sumInt #-}
sumInt :: forall m a. (Monad m, Integral a) => Fold m (a, Maybe a) a
sumInt = Fold step initial extract

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
--
-- | Sum of all the elements in a rolling window:
--
-- Uses Kahan-Babuska-Neumaier style summation for numerical stability of
-- floating precision arithmetic.
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
--
{-# INLINE sum #-}
sum :: forall m a. (Monad m, Num a) => Fold m (a, Maybe a) a
sum = Fold step initial extract

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
{-# INLINE length #-}
length :: (Monad m, Num b) => Fold m (a, Maybe a) b
length = Fold.foldl' step 0

    where

    step w (_, Nothing) = w + 1
    step w _ = w

-------------------------------------------------------------------------------
-- Location
-------------------------------------------------------------------------------

-- Theoretically, we can approximate minimum in a rolling window by using a
-- 'powerMean' with sufficiently large negative power.
--
-- XXX If we need to know the minimum in the window only once in a while then
-- we can use linear search when it is extracted and not pay the cost all the
-- time.
--
-- | The minimum element in a rolling window.
--
-- If you want to compute the minimum of the entire stream Fold.minimum from streamly
-- package would be much faster.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => Fold m (a, Maybe a) a
minimum = Fold step initial extract

    where

    initial = return $ Partial $ Tuple3' (0 :: Int) (0 :: Int)
                (mempty :: DQ.Deque (Int, a))

    step (Tuple3' i w q) (a, ma) =
        case ma of
            Nothing ->
                return $ Partial $ Tuple3' (i + 1) (w + 1)
                    (headCheck i q (w + 1) & dqloop (i, a))
            Just _ ->
                return $ Partial $ Tuple3' (i + 1) w
                    (headCheck i q w & dqloop (i,a))

    {-# INLINE headCheck #-}
    headCheck i q w =
        case DQ.uncons q of
            Nothing -> q
            Just (ia', q') ->
                if fst ia' <= i - w
                then q'
                else q

    dqloop ia q =
        case DQ.unsnoc q of
            Nothing -> DQ.snoc ia q
            -- XXX This can be improved for the case of `=`
            Just (ia', q') ->
                if snd ia <= snd ia'
                then dqloop ia q'
                else DQ.snoc ia q

    extract (Tuple3' _ _ q) = return $ snd
                                $ fromMaybe (0, error "min: Empty stream")
                                $ DQ.head q

-- Theoretically, we can approximate maximum in a rolling window by using a
-- 'powerMean' with sufficiently large positive power.
--
-- | The maximum element in a rolling window.
--
-- If you want to compute the maximum of the entire stream Fold.maximum from streamly
-- package would be much faster.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => Fold m (a, Maybe a) a
maximum = Fold step initial extract

    where

    initial = return $ Partial $ Tuple3' (0 :: Int) (0 :: Int)
                (mempty :: DQ.Deque (Int, a))

    step (Tuple3' i w q) (a, ma) =
        case ma of
            Nothing ->
                return $ Partial $ Tuple3' (i + 1) (w + 1)
                    (headCheck i q (w + 1) & dqloop (i, a))
            Just _ ->
                return $ Partial $ Tuple3' (i + 1) w
                    (headCheck i q w & dqloop (i,a))

    {-# INLINE headCheck #-}
    headCheck i q w =
        case DQ.uncons q of
            Nothing -> q
            Just (ia', q') ->
                if fst ia' <= i - w
                then q'
                else q

    dqloop ia q =
        case DQ.unsnoc q of
        Nothing -> DQ.snoc ia q
        -- XXX This can be improved for the case of `=`
        Just (ia', q') ->
            if snd ia >= snd ia'
            then dqloop ia q'
            else DQ.snoc ia q

    extract (Tuple3' _ _ q) =
        return
            $ snd
            $ fromMaybe (0, error "max: Empty stream")
            $ DQ.head q


-- | The difference between the maximum and minimum elements of a rolling window.
--
-- >>> range = Fold.teeWith (-) maximum minimum
--
-- If you want to compute the range of the entire stream @Fold.teeWith (-)
-- Fold.maximum Fold.min@  from the streamly package would be much faster.
--
-- /Space/: \(\mathcal{O}(n)\) where @n@ is the window size.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE range #-}
range :: (Monad m, Num a, Ord a) => Fold m (a, Maybe a) a
range = Fold.teeWith (-) maximum minimum
