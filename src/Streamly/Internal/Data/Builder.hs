-- |
-- Module      : Streamly.Internal.Data.Builder
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Building a right associative structure by incremental appending.
--
-- = Right Associative Structures
--
-- Right associative consable structures (Haskell lists or streamly streams)
-- can be extended on the left maintaining O(1) lazy consumption
-- characteristics, however, they do not provide a @snoc@ operation to extend on
-- the right. The semigroup operation ('<>') can be used for appends. However,
-- from performance standpoint (<>) is a right associative operation, when left
-- associated, which is the case for incremental appends, each append adds one
-- nesting layer making the lazy consumption to take O(n^2), @n@ being the
-- number of appends.
--
-- = Illustration
--
-- The following expressions build a list in a natural right associated manner:
--
-- @
-- a : b : c ... : []
-- a <> b <> c <> ...
-- a <> (b <> (c <> ...))
-- @
--
-- However, consider incremental appending in a program:
--
-- @
-- let x = a <> b
--     y = x <> c
--     z = y <> ...
-- @
--
-- The expression @z@ would be equivalent to:
--
-- @
-- z = ((a <> b) <> c) <> ...
-- @
--
-- This is a left associated append which would make the consumption to take
-- O(n^2) where @n@ is the number of appends.
--
-- = Associative Builders
--
-- Builders are truly associative monoidal containers which can be extended on
-- the left as well as right side while maintaining an O(1) lazy consumption
-- characteristics.  Left associative or right associative appending does not
-- make any difference. However, an uncons like operation on builders would
-- require O(n) time.
--
-- The idea is to use right associative representations for lazy
-- transformations (requiring uncons) and fully associative representations for
-- incremental appending (requiring snoc). A builder must be converted to a
-- right associated structure if we want to transform it. For that reason, it
-- does not make sense for a builder to represent an infinite structure.
-- Therefore, builders are used to build finite right associative structures by
-- incremental appending.
--
-- = Usage
--
-- == Using 'one', 'bag' and ('<>')
--
-- Using '<>':
--
-- @
-- one x <> one y :: 'Builder' [] a
-- one x <> one y & 'close' :: [a]
-- @
--
-- @
-- bag xs <> bag ys :: 'Builder' [] a
-- bag xs <> bag ys & 'close' :: [a]
-- @
--
-- == Using 'cons', 'snoc' and 'nil'
--
-- Right associative, building with elements:
--
-- @
-- x `'cons'` y `'cons'` 'nil' :: 'Builder' [] a
-- 'close' $ x `'cons'` y `'cons'` 'nil' :: [a]
-- @
--
-- Right associative, building with lists:
--
-- @
-- xs `'bcons'` ys `'bcons'` 'nil' :: 'Builder' [] a
-- 'close' $ xs `'bcons'` ys `'bcons'` 'nil' :: [a]
-- @
--
-- Left associative, building with elements:
--
-- @
-- 'nil' `'snoc'` x `'snoc'` y :: 'Builder' [] a
-- 'nil' `'snoc'` x `'snoc'` y & 'close' :: [a]
-- @
--
-- Left associative, building with lists:
--
-- @
-- 'nil' `'bsnoc'` x `'bsnoc'` y :: 'Builder' [] a
-- 'nil' `'bsnoc'` x `'bsnoc'` y & 'close' :: [a]
-- @
--
-- = Notes
--
-- In general, we should preclude the possibility of left associated appends
-- because that is just inefficient. Is it possible to use static analysis to
-- find out all such uses in existing code?

module Streamly.Internal.Data.Builder
    ( Builder (..)

    -- * Construction
    , nil
    , one
    , cons
    , snoc

    -- * Generation
    -- | Experimental. In general, we can generate a structure and lift it into
    -- a builder. For lists there is no perf difference in wrapping lists in a
    -- builder vs wrapping elements. In case of streams there is a 2x
    -- difference in worst case of singleton streams, however, in practical use
    -- cases it may not matter.
    , unfoldr -- experimental, use builder

    -- * Semigroup
    -- | Open a 'Consable' structure for O(1) appends irrespective of
    -- associativity. We can then cheaply extend it on the left or on the
    -- right.

    , append -- use (<>)
    , bag
    , bcons
    , bsnoc

    -- * Conversion
    , fromFoldable -- experimental, use foldMap one

    -- * Elimination
    , close
    , final
    , concat -- this is of limited use perhaps
    )
where

import Data.Semigroup (Semigroup (..))
import Streamly.Internal.Data.Consable (Consable)

import qualified Streamly.Internal.Data.Consable as Consable

import Prelude hiding (concat)

-- A builder represents a 'Consable' container. It is essentially a linked list
-- of functions (continuations), each function generating a part of the
-- container.  The builder in this module is essentially difference lists
-- generalized to a 'Consable'. This is fully associative builder.
newtype Builder t a = Builder (t a -> t a)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

-- | Lift a single element to a builder.
--
-- For streams this is 2x faster than using 'build' with singleton streams.
--
-- /Internal/
--
one :: Consable t => a -> Builder t a
one = Builder . Consable.cons

-- | Append two builders sequentially, the left or right associativity of the
-- expression does not matter, @(a `append` b) `append` c@ has the same
-- performance characterstics as @a `append` (b `append` c)@.
--
-- /Internal/
--
{-# INLINE append #-}
append :: Builder t a -> Builder t a -> Builder t a
append (Builder k1) (Builder k2) = Builder $ \next -> k1 (k2 next)

instance Semigroup (Builder t a) where
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
nil :: Builder t a
nil = Builder id

instance Monoid (Builder t a) where
    mempty = nil

infixr 5 `cons`

-- (.>)
--
-- | Add a value at the head of the builder.
--
-- > cons a b = one a <> b
--
-- /Internal/
--
cons :: Consable t => a -> Builder t a -> Builder t a
cons a b = one a <> b

-- (<.)
--
-- | Add a value at the tail of the builder.
--
-- > snoc b a = b <> one a
--
-- /Internal/
--
snoc :: Consable t => Builder t a -> a -> Builder t a
snoc b a = b <> one a

-------------------------------------------------------------------------------
-- Semigroup operations
-------------------------------------------------------------------------------

-- | Lift a 'Semigroup' capable container to a builder.
--
-- > bag = Builder . (<>)
--
-- /Internal/
--
bag :: Semigroup (t a) => t a -> Builder t a
bag = Builder . (<>)

infixr 5 `bcons`

-- (+>)
--
-- | Extend a builder by prepending a structure at the beginning.
--
-- > bcons xs b = bag xs <> b
--
-- /Internal/
--
bcons :: Semigroup (t a) => t a -> Builder t a -> Builder t a
bcons xs b = bag xs <> b

-- (<+)
--
-- | Extend a builder by appending a structure at the end.
--
-- > bsnoc b xs = b <> bag xs
--
-- /Internal/
--
bsnoc :: Semigroup (t a) => Builder t a -> t a -> Builder t a
bsnoc b xs = b <> bag xs

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

-- Directly generate sequences into a builder instead of reconstructing a
-- builder from another container.
--
-- XXX add other operations like repeat/replicate etc.

-- | Unfold a seed generating a builder.
--
-- /Internal/
--
unfoldr :: Consable t => (b -> Maybe (a, b)) -> b -> Builder t a
unfoldr step b =
    case step b of
        Nothing -> nil
        Just (a, b1) -> a `cons` unfoldr step b1

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------
--
-- | Convert a 'Foldable' container to a builder.
--
-- > fromFoldable = foldMap one
--
-- /Internal/
--
fromFoldable :: (Foldable t1, Consable t2) => t1 a -> Builder t2 a
fromFoldable = foldMap one

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Close the builder and extract the container.
--
-- /Internal/
--
{-# INLINE close #-}
close :: Consable t => Builder t a -> t a
close (Builder k) = k Consable.nil

-- | Close a builder by appending a final container to it.
--
-- This is experimental. We can always 'extendR' and 'close' instead.
--
-- /Internal/
--
{-# INLINE final #-}
final :: Builder t a -> t a -> t a
final (Builder k) = k

-- | Flatten a builder building a container of containers.
--
-- /Internal/
--
concat :: (Consable t1, Foldable t1, Foldable t2) => Builder t1 (t2 a) -> t1 a
concat = foldr (\x y -> foldr Consable.cons y x) Consable.nil . close

{-
-- XXX creates an intermediate structure, can it be fused?
-- The foldable instance essentially realizes the builder to underlying
-- container and folds it.  For simplicity, it is perhaps better to perform
-- operations on the container explicitly rather doing it on the builder.
instance (Consable t, Foldable t) => Foldable (Builder t) where
    foldMap f = foldMap f . close
-}
