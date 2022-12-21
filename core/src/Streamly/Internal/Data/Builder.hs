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

import Control.Applicative (liftA2)

------------------------------------------------------------------------------
-- The Builder type
------------------------------------------------------------------------------

-- | A simple stateful function composing monad that chains state passing
-- functions. This can be considered as a simplified version of the State monad
-- or even a Fold. Unlike fold the step function is one-shot and not called in
-- a loop.
newtype Builder s m a =
  Builder (s -> m (s, a))

-- | Maps a function on the output of the fold (the type @b@).
instance Functor m => Functor (Builder s m) where
    {-# INLINE fmap #-}
    fmap f (Builder step1) = Builder (fmap (fmap f) . step1)

{-# INLINE fromPure #-}
fromPure :: Applicative m => b -> Builder s m b
fromPure b = Builder (\s -> pure (s, b))

-- | Chain the actions and zip the outputs.
{-# INLINE sequenceWith #-}
sequenceWith :: Monad m =>
    (a -> b -> c) -> Builder x m a -> Builder x m b -> Builder x m c
sequenceWith func (Builder stepL) (Builder stepR) = Builder step

    where

    step s = do
        (s1, x) <- stepL s
        (s2, y) <- stepR s1
        pure (s2, func x y)

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
            (s1, x) <- stepL s
            let Builder stepR = f x
            (s2, y) <- stepR s1
            pure (s2, y)
