-- |
-- Module      : Streamly.Internal.Unicode.Utf8.Generate
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Unicode.Utf8.Generate
    (
    -- ** Accumulating maps
      mapAccumL
    , mapAccumR

    -- ** Generation and unfolding
    , replicateChar
    , replicate
    , unfoldr
    , unfoldrN
    )
where

#include "inline.hs"

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import qualified Streamly.Internal.Data.Stream.IsStream as Stream

import Streamly.Internal.Unicode.Utf8.Type

import Prelude hiding (replicate)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Streamly.Internal.Unicode.Utf8 as Utf8

--------------------------------------------------------------------------------
-- Building 'Utf8's
--------------------------------------------------------------------------------

-- | Like a combination of 'map' and 'foldl''. Applies a
-- function to each element of a 'Utf8', passing an accumulating
-- parameter from left to right, and returns a final 'Utf8'.  Performs
-- replacement on invalid scalar values.
--
-- /Time complexity:/ O(n)
--
-- /Unimplemented/
{-# INLINE mapAccumL #-}
mapAccumL :: (a -> Char -> (a,Char)) -> a -> Utf8 -> (a, Utf8)
mapAccumL = undefined

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- a strict 'foldr'; it applies a function to each element of a
-- 'Utf8', passing an accumulating parameter from right to left, and
-- returning a final value of this accumulator together with the new
-- 'Utf8'.
-- Performs replacement on invalid scalar values.
--
-- /Unimplemented/
{-# INLINE mapAccumR #-}
mapAccumR :: (a -> Char -> (a,Char)) -> a -> Utf8 -> (a, Utf8)
mapAccumR = undefined

--------------------------------------------------------------------------------
-- Generating and unfolding 'Utf8's
--------------------------------------------------------------------------------

-- | 'replicate' @n@ @t@ is a 'Utf8' consisting of the input
-- @t@ repeated @n@ times.
--
-- /Time complexity:/ O(n*m)
--
-- /Unimplemented/
{-# INLINE replicate #-}
replicate :: Int -> Utf8 -> Utf8
replicate = undefined

-- | 'replicateChar' @n@ @c@ is a 'Utf8' of length @n@ with @c@ the
-- value of every element.
--
-- /Time complexity:/ O(n)
{-# INLINE replicateChar #-}
replicateChar :: Int -> Char -> Utf8
replicateChar n c = fromStream (Stream.replicate n c)

-- | The 'unfoldr'
-- function is analogous to the List 'L.unfoldr'. 'unfoldr' builds a
-- 'Utf8' from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the 'Utf8', otherwise
-- 'Just' @(a,b)@.  In this case, @a@ is the next 'Char' in the
-- string, and @b@ is the seed value for further production.
-- Performs replacemsent on invalid scalar values.
--
-- /Time complexity:/ O(n), where @n@ is the length of the result.
{-# INLINE unfoldr #-}
unfoldr :: (a -> Maybe (Char, a)) -> a -> Utf8
unfoldr f s = fromStream (Stream.unfoldr f s)

-- | Like 'unfoldr', 'unfoldrN' builds a 'Utf8' from a seed
-- value. However, the length of the result should be limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the maximum length of the result is known and
-- correct, otherwise its performance is similar to 'unfoldr'.
-- Performs replacement on invalid scalar values.
--
-- /Time complexity:/ O(n)
--
-- /Unimplemented/
{-# INLINE unfoldrN #-}
unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> Utf8
unfoldrN = undefined
