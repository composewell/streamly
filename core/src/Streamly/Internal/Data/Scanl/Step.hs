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
    , fromFoldStep
    , toFoldStep
    )
where

import Data.Bifunctor (Bifunctor(..))
import Fusion.Plugin.Types (Fuse(..))

import qualified Streamly.Internal.Data.Fold.Step as Fold

------------------------------------------------------------------------------
-- Step of a scan
------------------------------------------------------------------------------

-- | Represents the result of the @step@ of a 'Scanl'.
--
{-# ANN type Step Fuse #-}
data Step s b
    = Partial !s
    -- ^ Returns the next state of the scan accumulator indicating a new
    -- result, the scan driver can extract the value of the accumulator and
    -- emit it in the output stream, and then it can call the scan step
    -- function again.
    | Continue !s
    -- ^ Returns the next state of the scan, the result should not be emitted
    -- in the output stream, the accumulator may or may not have advanced to
    -- the next state. If the driver calls "extract" on the state it may get
    -- the same old result, but extract will never fail regardless. This is
    -- essentially a mechanism to filter the output of a scan.
    | Done !b
    -- ^ Returns the final result, scan stops and cannot be driven further.

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

-- | Convert a fold 'Fold.Step' into a scan 'Step'.
{-# INLINE fromFoldStep #-}
fromFoldStep :: Fold.Step s b -> Step s b
fromFoldStep (Fold.Partial s) = Partial s
fromFoldStep (Fold.Done b) = Done b

-- | Convert a scan 'Step' into a fold 'Fold.Step'.
{-# INLINE toFoldStep #-}
toFoldStep :: Step s b -> Fold.Step s b
toFoldStep (Partial s) = Fold.Partial s
toFoldStep (Continue s) = Fold.Partial s
toFoldStep (Done b) = Fold.Done b
