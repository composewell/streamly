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
    , extractStep
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
data Step s b =
      Partial !s !b -- for transformation
    | Done !b       -- for termination with output
    | Continue !s   -- for filtering
    | Stop !s       -- termination without output (empty stream)

-- | 'first' maps over state and 'second' maps over result.
--
instance Bifunctor Step where
    {-# INLINE bimap #-}
    bimap f g (Partial s b) = Partial (f s) (g b)
    bimap _ g (Done b) = Done (g b)
    bimap f _ (Continue s) = Continue (f s)
    bimap f _ (Stop s) = Stop (f s)

    {-# INLINE first #-}
    first f (Partial s b) = Partial (f s) b
    first _ (Done b) = Done b
    first f (Continue s) = Continue (f s)
    first f (Stop s) = Stop (f s)

    {-# INLINE second #-}
    second g (Partial s b) = Partial s (g b)
    second g (Done b) = Done (g b)
    second _ (Continue s) = Continue s
    second _ (Stop s) = Stop s

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
        Partial s b -> Partial s <$> f b
        Done b -> Done <$> f b
        Continue s -> pure $ Continue s
        Stop s -> pure $ Stop s

{-# INLINE extractStep #-}
extractStep :: Monad m => (s -> m (Maybe b)) -> Step s b -> m (Maybe b)
extractStep f res = do
    case res of
        Partial s b -> return (Just b)
        Done b -> return (Just b)
        Continue s -> f s
        Stop s -> f s

-- | If 'Partial' then map the state, if 'Done' then call the next step.
{-# INLINE chainStepM #-}
chainStepM :: Monad m =>
       (s1 -> m s2)         -- state map
    -> (s1 -> m (Maybe a))  -- extract
    -> (a -> m (Step s2 b)) -- Continuation
    -> Step s1 a            -- input
    -> m (Step s2 b)
chainStepM f _ _ (Partial s _) = f s >>= \s1 -> return $ Continue s1
chainStepM f _ _ (Continue s) = f s >>= \s1 -> return $ Continue s1
chainStepM _ _ g (Done a) = g a
chainStepM f extract g (Stop s) = do
    r <- extract s
    case r of
        Nothing -> f s >>= \s1 -> return $ Stop s1
        Just a -> g a
