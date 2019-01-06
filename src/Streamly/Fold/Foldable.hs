-- |
-- Module      : Streamly.Fold.Foldable
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Composable folds.

-- Stolen from the foldl library. Thanks Gabriel!
-- foldl has quite a few dependencies and we only need parts of plus we need to
-- adapt to monadic streams.

module Streamly.Fold.Foldable
    (
      Fold (..)
    , FoldM (..)
    , fold
    , foldM
    )
where

import Streamly.Fold.Types (Fold(..), FoldM(..))
import qualified Data.Foldable as F

------------------------------------------------------------------------------
-- Folding pure containers
------------------------------------------------------------------------------

-- | Convert a `Fold` to a `FoldM`
--
-- > toFoldM (pure r) = pure r
-- > toFoldM (f <*> x) = toFoldM f <*> toFoldM x
{-
{-# INLINABLE toFoldM #-}
toFoldM :: Monad m => Fold a b -> FoldM m a b
toFoldM (Fold step begin done) = FoldM step' begin' done'
  where
    step' x a = return (step x a)
    begin'    = return  begin
    done' x   = return (done x)

-- | Convert a `FoldM` to a `Fold`
--
-- > toFold (pure r) = pure r
-- > toFold (f <*> x) = toFold f <*> toFold x
{-# INLINABLE toFold #-}
toFold :: FoldM Identity a b -> Fold a b
toFold (FoldM step begin done) = Fold step' begin' done'
  where
    step' x a = runIdentity (step x a)
    begin'    = runIdentity  begin
    done' x   = runIdentity (done x)
    -}

-- | Apply a strict left 'Fold' to a 'Foldable' container
{-# INLINE fold #-}
fold :: Foldable f => Fold a b -> f a -> b
fold (Fold step begin done) as = F.foldr cons done as begin
  where
    cons a k x = k $! step x a

-- | Like 'fold', but monadic
{-# INLINE foldM #-}
foldM :: (Foldable f, Monad m) => FoldM m a b -> f a -> m b
foldM (FoldM step begin done) as0 = do
    x0 <- begin
    F.foldr step' done as0 $! x0
  where
    step' a k x = do
        x' <- step x a
        k $! x'
