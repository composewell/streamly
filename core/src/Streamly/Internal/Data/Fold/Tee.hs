{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Fold.Tee
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A newtype wrapper over the 'Fold' type providing distributing 'Applicative',
-- 'Semigroup', 'Monoid', 'Num', 'Floating' and 'Fractional' instances.
--
module Streamly.Internal.Data.Fold.Tee
    ( Tee(..)
    , toFold
    )
where

import Control.Applicative (liftA2)
import Streamly.Internal.Data.Fold.Type (Fold)

import qualified Streamly.Internal.Data.Fold.Type as Fold

#include "DocTestDataFold.hs"

-- | @Tee@ is a newtype wrapper over the 'Fold' type providing distributing
-- 'Applicative', 'Semigroup', 'Monoid', 'Num', 'Floating' and 'Fractional'
-- instances.
--
-- The input received by the composed 'Tee' is replicated and distributed to
-- the constituent folds of the 'Tee'.
--
-- For example, to compute the average of numbers in a stream without going
-- through the stream twice:
--
-- >>> avg = (/) <$> (Tee Fold.sum) <*> (Tee $ fmap fromIntegral Fold.length)
-- >>> Stream.fold (unTee avg) $ Stream.fromList [1.0..100.0]
-- 50.5
--
-- Similarly, the 'Semigroup' and 'Monoid' instances of 'Tee' distribute the
-- input to both the folds and combine the outputs using Monoid or Semigroup
-- instances of the output types:
--
-- >>> import Data.Monoid (Sum(..))
-- >>> t = Tee Fold.one <> Tee Fold.latest
-- >>> Stream.fold (unTee t) (fmap Sum $ Stream.enumerateFromTo 1.0 100.0)
-- Just (Sum {getSum = 101.0})
--
-- The 'Num', 'Floating', and 'Fractional' instances work in the same way.
--
newtype Tee m a b =
    Tee { unTee :: Fold m a b }
    deriving (Functor)

{-# DEPRECATED toFold "Please use 'unTee' instead." #-}
toFold :: Tee m a b -> Fold m a b
toFold = unTee

-- | '<*>' distributes the input to both the argument 'Tee's and combines their
-- outputs using function application.
--
instance Monad m => Applicative (Tee m a) where

    {-# INLINE pure #-}
    pure a = Tee (Fold.fromPure a)

    {-# INLINE (<*>) #-}
    (<*>) a b = Tee (Fold.teeWith ($) (unTee a) (unTee b))

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
