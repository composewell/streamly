-- |
-- Module      : Streamly.Internal.Data.Monoid.Builder
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- This is basically 'Endo' for a Monoid type with a builder interface.  We
-- can use this for building strings or lists or any other monoid type. This
-- performs as well as a as a difference list.
--
-- = Usage
--
-- >>> b1 = mk "hello "
-- >>> b2 = b1 <> mk "world!"
-- >>> use b2
-- "hello world!"
--
module Streamly.Internal.Data.Monoid.Builder
    ( Builder (..)

    -- * Construction
    , mk

    -- * Elimination
    , use

    -- * Experimental
    , nil -- use mempty
    , cons
    , snoc
    , fromFoldable -- use foldMap mk
    , final
    )
where

import Data.Semigroup (Semigroup (..))
import Prelude hiding (concat)

newtype Builder a = Builder (a -> a)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Lift a single element to a builder.
--
-- /Internal/
--
mk :: Monoid a => a -> Builder a
mk = Builder . (<>)

-- | Append two builders sequentially, the left or right associativity of the
-- expression does not matter, @(a `append` b) `append` c@ has the same
-- performance characterstics as @a `append` (b `append` c)@.
--
-- /Internal/
--
{-# INLINE append #-}
append :: Builder a -> Builder a -> Builder a
append (Builder k1) (Builder k2) = Builder $ \next -> k1 (k2 next)

instance Semigroup (Builder a) where
    {-# INLINE (<>) #-}
    (<>) = append

    {-# INLINE stimes #-}
    stimes n x
        | n < 0 = error "Streamly.Data.Builder.stimes: negative multiplier"
        | otherwise = times n

        where

        times 0 = nil
        times i = x <> times (pred i)

-- | An empty builder.
--
-- > nil = Builder id
--
-- /Internal/
--
nil :: Builder a
nil = Builder id

instance Monoid (Builder a) where
    mempty = nil

infixr 5 `cons`

-- (.>)
--
-- | Add a value at the head of the builder.
--
-- > cons a b = mk a <> b
--
-- /Internal/
--
cons :: Monoid a => a -> Builder a -> Builder a
cons a b = mk a <> b

-- (<.)
--
-- | Add a value at the tail of the builder.
--
-- > snoc b a = b <> mk a
--
-- /Internal/
--
snoc :: Monoid a => Builder a -> a -> Builder a
snoc b a = b <> mk a

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------
--
-- | Convert a 'Foldable' container to a builder.
--
-- > fromFoldable = foldMap mk
--
-- /Internal/
--
fromFoldable :: (Foldable t, Monoid a) => t a -> Builder a
fromFoldable = foldMap mk

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Close the builder and extract the container.
--
-- /Internal/
--
{-# INLINE use #-}
use :: Monoid a => Builder a -> a
use (Builder k) = k mempty

-- | Close a builder by appending a final value to it.
--
-- /Internal/
--
{-# INLINE final #-}
final :: Builder a -> a -> a
final (Builder k) = k
