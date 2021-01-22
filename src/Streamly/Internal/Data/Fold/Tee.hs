-- |
-- Module      : Streamly.Internal.Data.Fold.Tee
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Fold.Tee
    ( Tee(..)
    , fromFold
    , toFold
    )
where

import Data.Coerce (coerce)
import Streamly.Internal.Data.Fold.Types (Fold)

import qualified Streamly.Internal.Data.Fold.Types as Fold

-- | The type @Tee m a b@ represents a left fold over an input stream of values
-- of type @a@ to a single value of type @b@ in 'Monad' @m@.
--
-- @Tee@ is a wrapper over 'Fold' that uses 'teeWith' to define the applicative
-- instance.
--
-- /Internal/
newtype Tee m a b =
    Tee { runTee :: Fold m a b }
    deriving (Functor)

-- | Convert a 'Tee' to 'Fold'.
{-# INLINE toFold #-}
toFold :: Tee m a b -> Fold m a b
toFold = coerce

-- | Convert a 'Fold' to 'Tee'.
{-# INLINE fromFold #-}
fromFold :: Fold m a b -> Tee m a b
fromFold = coerce

-- | The 'Tee' resulting from '<*>' distributes its input to both the argument
-- 'Tee's and combines their output using function application.
--
instance Monad m => Applicative (Tee m a) where

    {-# INLINE pure #-}
    pure a = fromFold (Fold.yield a)

    {-# INLINE (<*>) #-}
    (<*>) a b = fromFold (Fold.teeWith ($) (toFold a) (toFold b))
