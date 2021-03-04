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

import Control.Applicative (liftA2)
import Data.Coerce (coerce)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Streamly.Internal.Data.Fold.Types (Fold)

import qualified Streamly.Internal.Data.Fold.Types as Fold

-- | The type @Tee m a b@ represents a left fold over an input stream of values
-- of type @a@ to a single value of type @b@ in 'Monad' @m@.
--
-- @Tee@ is a wrapper over 'Fold' that uses 'teeWith' to define the applicative
-- instance.
--
-- /Pre-release/
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

-- | Combines the outputs (the type @b@) using their 'Semigroup' instances.
instance (Semigroup b, Monad m) => Semigroup (Tee m a b) where
    {-# INLINE (<>) #-}
    (<>) = liftA2 (<>)

-- | Combines the outputs (the type @b@) using their 'Monoid' instances.
instance (Semigroup b, Monoid b, Monad m) => Monoid (Tee m a b) where
    {-# INLINE mempty #-}
    mempty = pure mempty

    {-# INLINE mappend #-}
    mappend = (<>)

-- | Combines the outputs (type @b@) using their 'Num' instances.
instance (Monad m, Num b) => Num (Tee m a b) where
    {-# INLINE fromInteger #-}
    fromInteger = pure . fromInteger

    {-# INLINE negate #-}
    negate = fmap negate

    {-# INLINE abs #-}
    abs = fmap abs

    {-# INLINE signum #-}
    signum = fmap signum

    {-# INLINE (+) #-}
    (+) = liftA2 (+)

    {-# INLINE (*) #-}
    (*) = liftA2 (*)

    {-# INLINE (-) #-}
    (-) = liftA2 (-)

-- | Combines the outputs (type @b@) using their 'Fractional' instances.
instance (Monad m, Fractional b) => Fractional (Tee m a b) where
    {-# INLINE fromRational #-}
    fromRational = pure . fromRational

    {-# INLINE recip #-}
    recip = fmap recip

    {-# INLINE (/) #-}
    (/) = liftA2 (/)

-- | Combines the outputs using their 'Floating' instances.
instance (Monad m, Floating b) => Floating (Tee m a b) where
    {-# INLINE pi #-}
    pi = pure pi

    {-# INLINE exp #-}
    exp = fmap exp

    {-# INLINE sqrt #-}
    sqrt = fmap sqrt

    {-# INLINE log #-}
    log = fmap log

    {-# INLINE sin #-}
    sin = fmap sin

    {-# INLINE tan #-}
    tan = fmap tan

    {-# INLINE cos #-}
    cos = fmap cos

    {-# INLINE asin #-}
    asin = fmap asin

    {-# INLINE atan #-}
    atan = fmap atan

    {-# INLINE acos #-}
    acos = fmap acos

    {-# INLINE sinh #-}
    sinh = fmap sinh

    {-# INLINE tanh #-}
    tanh = fmap tanh

    {-# INLINE cosh #-}
    cosh = fmap cosh

    {-# INLINE asinh #-}
    asinh = fmap asinh

    {-# INLINE atanh #-}
    atanh = fmap atanh

    {-# INLINE acosh #-}
    acosh = fmap acosh

    {-# INLINE (**) #-}
    (**) = liftA2 (**)

    {-# INLINE logBase #-}
    logBase = liftA2 logBase
