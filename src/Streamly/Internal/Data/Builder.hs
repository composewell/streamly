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
-- == Using cons and snoc operators
--
-- Build using elements:
--
-- >>> b1 = 'h' <+ 'i' <+ mempty -- Builder t Char
-- >>> b2 = b1 +> '!'            -- Builder t Char
-- >>> use b2 :: [Char]
-- "hi!"
-- >>> use b2 :: SerialT Identity Char
-- "hi!"
--
-- Build using containers:
--
-- >>> b1 = "hello" <++ " world" <++ mempty -- Builder [] Char
-- >>> b2 = b1 ++> "!!"                     -- Builder [] Char
-- >>> use b2                               -- [Char]
-- "hello world!!"
--
-- Mixed:
--
-- >>> b1 = 'h' <+ "ello" <++ mempty -- Builder [] Char
-- >>> b2 = b1 ++> " world" +> '!'   -- Builder [] Char
-- >>> use b2                        -- [Char]
-- "hello world!"
--
-- == Using 'cons' and 'snoc'
--
-- >>> b1 = 'h' `cons` "ello" `bcons` mempty -- Builder [] Char
-- >>> b2 = b1 `bsnoc` " world" `snoc` '!'   -- Builder [] Char
-- >>> use b2                                -- [Char]
-- "hello world!"
--
-- == Using 'mk', 'add' and ('<>')
--
-- >>> b1 = mk 'h' <> add "ello"         -- Builder [] Char
-- >>> b2 = b1 <> add " world" <> mk '!' -- Builder [] Char
-- >>> use b2                            -- [Char]
-- "hello world!"
--
-- = Notes
--
-- In general, we should preclude the possibility of left associated appends
-- because that is just inefficient. Is it possible to use static analysis to
-- find out all such uses in existing code?

module Streamly.Internal.Data.Builder
    ( Builder (..)
    , Buildable (..)

    -- * Construction
    , nil

    , cons
    , snoc
    , (<+)
    , (+>)

    , bcons
    , bsnoc
    , (<++)
    , (++>)

    -- * Experimental

    -- ** Generation
    -- | Experimental. In general, we can generate a structure and lift it into
    -- a builder. For lists there is no perf difference in wrapping lists in a
    -- builder vs wrapping elements. In case of streams there is a 2x
    -- difference in worst case of singleton streams, however, in practical use
    -- cases it may not matter.
    , unfoldr -- experimental, use builder

    , append -- use (<>)

    -- ** Conversion
    , fromFoldable -- experimental, use foldMap mk

    -- ** Elimination
    , final
    )
where

import Data.Semigroup (Semigroup (..))
import Streamly.Internal.Data.Stream.Serial (SerialT)
import qualified Streamly.Internal.Data.Stream.StreamK as Serial

import Prelude hiding (concat)

-- A builder represents a 'Consable' container. It is essentially a linked list
-- of functions (continuations), each function generating a part of the
-- container.  The builder in this module is essentially difference lists
-- generalized to a 'Consable'. This is fully associative builder.
newtype Builder t a = Builder (t a -> t a)

class Buildable t where
    -- | Make a builder from a single element.
    mk :: a -> Builder t a
    -- | Make a builder from a container.
    add :: t a -> Builder t a
    -- | Use the builder as the underlying container type.
    use :: Builder t a -> t a

instance Buildable [] where
    mk = Builder . (:)
    add = Builder . (++)
    use (Builder k) = k []

instance Buildable (SerialT m) where
    mk = Builder . (Serial.cons)
    add = Builder . (Serial.serial)
    use (Builder k) = k Serial.nil

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

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

-- | Add a value at the head of the builder. Right associvative when used
-- infix.
--
-- > cons a b = mk a <> b
--
-- /Internal/
--
cons :: Buildable t => a -> Builder t a -> Builder t a
cons a b = mk a <> b

infixr 5 <+

-- | Same as 'cons'. Right associvative.
--
-- /Internal/
--
(<+) :: Buildable t => a -> Builder t a -> Builder t a
(<+) = cons

--
-- | Add a value at the tail of the builder. Left associvative when used infix.
--
-- > snoc b a = b <> mk a
--
-- /Internal/
--
snoc :: Buildable t => Builder t a -> a -> Builder t a
snoc b a = b <> mk a

-- | Same as 'snoc'. Left associvative.
--
-- /Internal/
--
(+>) :: Buildable t => Builder t a -> a -> Builder t a
(+>) = snoc

-------------------------------------------------------------------------------
-- Semigroup operations
-------------------------------------------------------------------------------

infixr 5 `bcons`

-- | Extend a builder by prepending a structure at the beginning. Right
-- associvative when used infix.
--
-- > bcons xs b = add xs <> b
--
-- /Internal/
--
bcons :: Buildable t => t a -> Builder t a -> Builder t a
bcons xs b = add xs <> b

infixr 5 <++

-- | Same as 'bcons'. Right associative.
--
-- /Internal/
--
(<++) :: Buildable t => t a -> Builder t a -> Builder t a
(<++) = bcons

-- | Extend a builder by appending a structure at the end. Left associative
-- when used infix.
--
-- > bsnoc b xs = b <> add xs
--
-- /Internal/
--
bsnoc :: Buildable t => Builder t a -> t a -> Builder t a
bsnoc b xs = b <> add xs

-- | Same as 'bsnoc'. Left associative.
--
-- /Internal/
--
(++>) :: Buildable t => Builder t a -> t a -> Builder t a
(++>) = bsnoc

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
unfoldr :: Buildable t => (b -> Maybe (a, b)) -> b -> Builder t a
unfoldr step b =
    case step b of
        Nothing -> nil
        Just (a, b1) -> mk a <> unfoldr step b1

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
fromFoldable :: (Foldable t1, Buildable t2) => t1 a -> Builder t2 a
fromFoldable = foldMap mk

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-- | Close a builder by appending a final container to it.
--
-- This is experimental. We can always 'extendR' and 'use instead.
--
-- /Internal/
--
{-# INLINE final #-}
final :: Builder t a -> t a -> t a
final (Builder k) = k

{-
-- XXX creates an intermediate structure, can it be fused?
-- The foldable instance essentially realizes the builder to underlying
-- container and folds it.  For simplicity, it is perhaps better to perform
-- operations on the container explicitly rather doing it on the builder.
instance (Buildable t, Foldable t) => Foldable (Builder t) where
    foldMap f = foldMap f . use
-}
