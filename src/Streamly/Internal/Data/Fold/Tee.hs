-- |
-- Module      : Streamly.Internal.Data.Fold.Tee
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- The @Tee@ type is a newtype wrapper over the 'Fold' type providing
-- distributing 'Applicative', 'Semigroup', 'Monoid', 'Num', 'Floating' and
-- 'Fractional' instances. The input received by the composed 'Tee' is
-- replicated and distributed to both the constituent Tee's.
--
-- For example, to compute the average of numbers in a stream without going
-- through the stream twice:
--
-- >>> import Streamly.Internal.Data.Fold.Tee (Tee(..), toFold)
-- >>> import Streamly.Internal.Data.Fold as Fold
--
-- >>> avg = (/) <$> (Tee Fold.sum) <*> (Tee $ fmap fromIntegral Fold.length)
-- >>> Stream.fold (toFold avg) $ Stream.fromList [1.0..100.0]
-- 50.5
--
-- Similarly, the 'Semigroup' and 'Monoid' instances of 'Tee' distribute the
-- input to both the folds and combines the outputs using Monoid or Semigroup
-- instances of the output types:
--
-- >>> import Data.Monoid (Sum(..))
-- >>> t = Tee Fold.head <> Tee Fold.last
-- >>> Stream.fold (toFold t) (fmap Sum $ Stream.enumerateFromTo 1.0 100.0)
-- Just (Sum {getSum = 101.0})
--
-- The 'Num', 'Floating', and 'Fractional' instances work in the same way.
--
module Streamly.Internal.Data.Fold.Tee
    ( Tee(..)    
    )
where

import Control.Applicative (liftA2)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import Streamly.Internal.Data.Fold.Type (Fold)

import qualified Streamly.Internal.Data.Fold.Type as Fold

-- | @Tee@ is a newtype wrapper over the 'Fold' type providing distributing
-- 'Applicative', 'Semigroup', 'Monoid', 'Num', 'Floating' and 'Fractional'
-- instances.
--
-- /Pre-release/
newtype Tee m a b =
    Tee { toFold :: Fold m a b }
    deriving (Functor)

-- | '<*>' distributes the input to both the argument 'Tee's and combines their
-- outputs using function application.
--
instance Monad m => Applicative (Tee m a) where

    {-# INLINE pure #-}
    pure a = Tee (Fold.yield a)

    {-# INLINE (<*>) #-}
    (<*>) a b = Tee (Fold.teeWith ($) (toFold a) (toFold b))

-- | '<>' distributes the input to both the argument 'Tee's and combines their
-- outputs using the 'Semigroup' instance of the output type.
--
instance (Semigroup b, Monad m) => Semigroup (Tee m a b) where
    {-# INLINE (<>) #-}
    (<>) = liftA2 (<>)

-- | '<>' distributes the input to both the argument 'Tee's and combines their
-- outputs using the 'Monoid' instance of the output type.
--
instance (Semigroup b, Monoid b, Monad m) => Monoid (Tee m a b) where
    {-# INLINE mempty #-}
    mempty = pure mempty

    {-# INLINE mappend #-}
    mappend = (<>)

-- | Binary 'Num' operations distribute the input to both the argument 'Tee's
-- and combine their outputs using the 'Num' instance of the output type.
--
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

-- | Binary 'Fractional' operations distribute the input to both the argument
-- 'Tee's and combine their outputs using the 'Fractional' instance of the
-- output type.
--
instance (Monad m, Fractional b) => Fractional (Tee m a b) where
    {-# INLINE fromRational #-}
    fromRational = pure . fromRational

    {-# INLINE recip #-}
    recip = fmap recip

    {-# INLINE (/) #-}
    (/) = liftA2 (/)

-- | Binary 'Floating' operations distribute the input to both the argument
-- 'Tee's and combine their outputs using the 'Floating' instance of the output
-- type.
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
