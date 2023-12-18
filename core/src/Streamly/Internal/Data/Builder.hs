-- |
-- Module      : Streamly.Internal.Data.Builder
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Builder
    (
    -- * Imports
    -- $setup

    -- * Types
      Builder (..)
    )
where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Data.Bifunctor (first)

------------------------------------------------------------------------------
-- The Builder type
------------------------------------------------------------------------------

-- | A simple stateful function composing monad that chains state passing
-- functions. This can be considered as a simplified version of the State monad
-- or even a Fold. Unlike fold the step function is one-shot and not called in
-- a loop.
newtype Builder s m a =
  Builder (s -> m (a, s))

-- | Maps a function on the output of the fold (the type @b@).
instance Functor m => Functor (Builder s m) where
    {-# INLINE fmap #-}
    fmap f (Builder step1) = Builder (fmap (first f) . step1)

{-# INLINE fromPure #-}
fromPure :: Applicative m => b -> Builder s m b
fromPure b = Builder (\s -> pure (b, s))

-- | Chain the actions and zip the outputs.
{-# INLINE sequenceWith #-}
sequenceWith :: Monad m =>
    (a -> b -> c) -> Builder x m a -> Builder x m b -> Builder x m c
sequenceWith func (Builder stepL) (Builder stepR) = Builder step

    where

    step s = do
        (x, s1) <- stepL s
        (y, s2) <- stepR s1
        pure (func x y, s2)

instance Monad m => Applicative (Builder a m) where
    {-# INLINE pure #-}
    pure = fromPure

    {-# INLINE (<*>) #-}
    (<*>) = sequenceWith id

    {-# INLINE (*>) #-}
    (*>) = sequenceWith (const id)

    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)

instance Monad m => Monad (Builder a m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (Builder stepL) >>= f = Builder step

        where

        step s = do
            (x, s1) <- stepL s
            let Builder stepR = f x
            (y, s2) <- stepR s1
            pure (y, s2)
