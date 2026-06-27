-- |
-- Module      : Streamly.Internal.Data.Scanl.Step
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Scanl.Step
    (
    -- * Step Type
      Step (..)

    , mapMStep
    , chainStepM
    )
where

import Data.Bifunctor (Bifunctor(..))
import Fusion.Plugin.Types (Fuse(..))

------------------------------------------------------------------------------
-- Step of a scan
------------------------------------------------------------------------------

-- The Step functor around b allows expressing early termination like a right
-- fold. Traditional list right folds use function composition and laziness to
-- terminate early whereas we use data constructors. It allows stream fusion in
-- contrast to the foldr/build fusion when composing with functions.

-- | Represents the result of the @step@ of a 'Scanl'.  'Partial' returns an
-- intermediate state of the scan, the scan step can be called again with the
-- state or the driver can use @extract@ on the state to get the result out.
-- 'Continue' is like 'Partial' in that it returns an intermediate state, but it
-- additionally indicates that no output is available for this step: a scan
-- driver must advance the state without calling @extract@ (so a scan can
-- consume an input without emitting an output). 'Done' returns the final result
-- and the scan cannot be driven further.
--
-- /Pre-release/
--
{-# ANN type Step Fuse #-}
data Step s b
    = Partial !s
    | Continue !s
    | Done !b

-- | 'first' maps over the scan state and 'second' maps over the scan result.
--
instance Bifunctor Step where
    {-# INLINE bimap #-}
    bimap f _ (Partial a) = Partial (f a)
    bimap f _ (Continue a) = Continue (f a)
    bimap _ g (Done b) = Done (g b)

    {-# INLINE first #-}
    first f (Partial a) = Partial (f a)
    first f (Continue a) = Continue (f a)
    first _ (Done x) = Done x

    {-# INLINE second #-}
    second _ (Partial x) = Partial x
    second _ (Continue x) = Continue x
    second f (Done a) = Done (f a)

-- | 'fmap' maps over 'Done'.
--
-- @
-- fmap = 'second'
-- @
--
instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap = second

-- | Map a monadic function over the result @b@ in @Step s b@.
--
-- /Internal/
{-# INLINE mapMStep #-}
mapMStep :: Applicative m => (a -> m b) -> Step s a -> m (Step s b)
mapMStep f res =
    case res of
        Partial s -> pure $ Partial s
        Continue s -> pure $ Continue s
        Done b -> Done <$> f b

-- | If 'Partial' then map the state, if 'Done' then call the next step.
{-# INLINE chainStepM #-}
chainStepM :: Applicative m =>
    (s1 -> m s2) -> (a -> m (Step s2 b)) -> Step s1 a -> m (Step s2 b)
chainStepM f _ (Partial s) = Partial <$> f s
chainStepM f _ (Continue s) = Continue <$> f s
chainStepM _ g (Done b) = g b
