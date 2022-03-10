-- |
-- Module      : Streamly.Internal.Data.Fold.Step
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Fold.Step
    (
    -- * Types
      Step (..)

    , mapMStep
    , chainStepM
    )
where

import Data.Bifunctor (Bifunctor(..))
import Fusion.Plugin.Types (Fuse(..))

------------------------------------------------------------------------------
-- Step of a fold
------------------------------------------------------------------------------

-- The Step functor around b allows expressing early termination like a right
-- fold. Traditional list right folds use function composition and laziness to
-- terminate early whereas we use data constructors. It allows stream fusion in
-- contrast to the foldr/build fusion when composing with functions.

-- | Represents the result of the @step@ of a 'Fold'.  'Partial' returns an
-- intermediate state of the fold, the fold step can be called again with the
-- state or the driver can use @extract@ on the state to get the result out.
-- 'Done' returns the final result and the fold cannot be driven further.
--
-- /Pre-release/
--
{-# ANN type Step Fuse #-}
data Step s b
    = Partial !s
    | Done !b

{-
-- We can use this type for more general use cases. This can also represent the
-- ability to either perform an effect during initial (partial) or not
-- (Continue).
--
{-# ANN type Step Fuse #-}
data Step s b =
      Partial !s !b -- in initial, b can be used for scan instead of postscan
    | Continue !s -- in initial, it can be used for postscan, instead of scan
    | Done !b -- in initial this can be used for `take 0` type of use case
-}

-- | 'first' maps over 'Partial' and 'second' maps over 'Done'.
--
instance Bifunctor Step where
    {-# INLINE bimap #-}
    bimap f _ (Partial a) = Partial (f a)
    bimap _ g (Done b) = Done (g b)

    {-# INLINE first #-}
    first f (Partial a) = Partial (f a)
    first _ (Done x) = Done x

    {-# INLINE second #-}
    second _ (Partial x) = Partial x
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
        Done b -> Done <$> f b

-- | If 'Partial' then map the state, if 'Done' then call the next step.
{-# INLINE chainStepM #-}
chainStepM :: Applicative m =>
    (s1 -> m s2) -> (a -> m (Step s2 b)) -> Step s1 a -> m (Step s2 b)
chainStepM f _ (Partial s) = Partial <$> f s
chainStepM _ g (Done b) = g b
