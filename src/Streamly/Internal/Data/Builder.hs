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
-- nesting layer making the lazy consumption to take O(n^2).
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
-- = Notes
--
-- In general, we should preclude the possibility of left associated appends
-- because that is just inefficient. Is it possible to use static analysis to
-- find out all such uses in existing code?

module Streamly.Internal.Data.Builder
    ( Builder (..)

    -- * Construction
    , nil
    , solo
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
    , build
    , extendL -- cons
    , extendR -- snoc

    -- * Conversion
    , fromFoldable -- experimental, use foldMap solo

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

-- | Lift a singleton value to a builder.
--
-- For streams this is 2x faster than using 'build' with singleton streams.
--
-- /Internal/
--
solo :: Consable t => a -> Builder t a
solo = Builder . Consable.cons

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

-- | Add a value at the head of the builder.
--
-- > cons a b = solo a <> b
--
-- /Internal/
--
cons :: Consable t => a -> Builder t a -> Builder t a
cons a b = solo a <> b

-- | Add a value at the tail of the builder.
--
-- > snoc b a = b <> solo a
--
-- /Internal/
--
snoc :: Consable t => Builder t a -> a -> Builder t a
snoc b a = b <> solo a

-------------------------------------------------------------------------------
-- Semigroup operations
-------------------------------------------------------------------------------

-- | Wrap a 'Semigroup' capable container into a builder.
--
-- > build = Builder . (<>)
--
-- /Internal/
--
build :: Semigroup (t a) => t a -> Builder t a
build = Builder . (<>)

-- | Extend a builder by appending a structure on the right side.
--
-- > extendR b xs = b <> build xs
--
-- /Internal/
--
extendR :: Semigroup (t a) => Builder t a -> t a -> Builder t a
extendR b xs = b <> build xs

-- | Extend a builder by prepending a structure on the left side.
--
-- > extendL xs b = build xs <> b
--
-- /Internal/
--
extendL :: Semigroup (t a) => t a -> Builder t a -> Builder t a
extendL xs b = build xs <> b

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
-- > fromFoldable = foldMap solo
--
-- /Internal/
--
fromFoldable :: (Foldable t1, Consable t2) => t1 a -> Builder t2 a
fromFoldable = foldMap solo

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
