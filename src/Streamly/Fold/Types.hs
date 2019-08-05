{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification          #-}

-- |
-- Module      : Streamly.Fold.Types
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Fold.Types
    ( Fold (..)
    )
where

import Control.Applicative (liftA2)
import Data.Semigroup (Semigroup(..))
import Streamly.Strict (Tuple'(..))

------------------------------------------------------------------------------
-- Monadic left folds
------------------------------------------------------------------------------

-- | Represents a left fold over an input stream of values of type @a@ to a
-- single value of type @b@ in 'Monad' @m@.
--
-- @since 0.7.0

-- The fold uses an intermediate type @x@ as accumulator. The fold accumulator
-- is initialized by calling the @init@ function and is then driven by calling
-- the step function repeatedly. When the fold is done the @extract@ function
-- is used to map the intermediate type @x@ to the final type @b@. This allows
-- the state of the fold to be embedded in an arbitrary type @x@.
data Fold m a b =
  -- | @Fold @ @ step @ @ initial @ @ extract@
  forall x. Fold (x -> a -> m x) (m x) (x -> m b)

-- | Maps a function on the output of the fold (the type @b@).
instance Applicative m => Functor (Fold m a) where
    {-# INLINE fmap #-}
    fmap f (Fold step start done) = Fold step start done'
        where
        done' x = fmap f $! done x

    {-# INLINE (<$) #-}
    (<$) b = \_ -> pure b

-- | The fold resulting from '<*>' distributes its input to both the argument
-- folds and combines their output using the supplied function.
instance Applicative m => Applicative (Fold m a) where
    {-# INLINE pure #-}
    pure b = Fold (\() _ -> pure ()) (pure ()) (\() -> pure b)

    {-# INLINE (<*>) #-}
    (Fold stepL beginL doneL) <*> (Fold stepR beginR doneR) =
        let step (Tuple' xL xR) a = Tuple' <$> stepL xL a <*> stepR xR a
            begin = Tuple' <$> beginL <*> beginR
            done (Tuple' xL xR) = doneL xL <*> doneR xR
        in  Fold step begin done

    {-# INLINE (<*) #-}
    (<*) m = \_ -> m

    {-# INLINE (*>) #-}
    _ *> m = m

-- | Combines the outputs of the folds (the type @b@) using their 'Semigroup'
-- instances.
instance (Semigroup b, Monad m) => Semigroup (Fold m a b) where
    {-# INLINE (<>) #-}
    (<>) = liftA2 (<>)

-- | Combines the outputs of the folds (the type @b@) using their 'Monoid'
-- instances.
instance (Semigroup b, Monoid b, Monad m) => Monoid (Fold m a b) where
    {-# INLINE mempty #-}
    mempty = pure mempty

    {-# INLINE mappend #-}
    mappend = (<>)

-- | Combines the fold outputs (type @b@) using their 'Num' instances.
instance (Monad m, Num b) => Num (Fold m a b) where
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

-- | Combines the fold outputs (type @b@) using their 'Fractional' instances.
instance (Monad m, Fractional b) => Fractional (Fold m a b) where
    {-# INLINE fromRational #-}
    fromRational = pure . fromRational

    {-# INLINE recip #-}
    recip = fmap recip

    {-# INLINE (/) #-}
    (/) = liftA2 (/)

-- | Combines the fold outputs using their 'Floating' instances.
instance (Monad m, Floating b) => Floating (Fold m a b) where
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
