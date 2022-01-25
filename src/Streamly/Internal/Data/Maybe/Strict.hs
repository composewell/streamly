-- |
-- Module      : Streamly.Internal.Data.Maybe.Strict
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- | Strict data types to be used as accumulator for strict left folds and
-- scans. For more comprehensive strict data types see
-- https://hackage.haskell.org/package/strict-base-types . The names have been
-- suffixed by a prime so that programmers can easily distinguish the strict
-- versions from the lazy ones.
--
-- One major advantage of strict data structures as accumulators in folds and
-- scans is that it helps the compiler optimize the code much better by
-- unboxing. In a big tight loop the difference could be huge.
--
module Streamly.Internal.Data.Maybe.Strict
    ( Maybe' (..)
    , toMaybe
    , isJust'
    , fromJust'
    )
where

-- | A strict 'Maybe'
data Maybe' a = Just' !a | Nothing' deriving Show

-- | Convert strict Maybe' to lazy Maybe
{-# INLINE toMaybe #-}
toMaybe :: Maybe' a -> Maybe a
toMaybe  Nothing' = Nothing
toMaybe (Just' a) = Just a

-- | Extract the element out of a Just' and throws an error if its argument is
-- Nothing'.
{-# INLINE fromJust' #-}
fromJust' :: Maybe' a -> a
fromJust' (Just' a) = a
fromJust' Nothing' = error "fromJust' cannot be run in Nothing'"

-- | Returns True iff its argument is of the form "Just' _".
{-# INLINE isJust' #-}
isJust' :: Maybe' a -> Bool
isJust' (Just' _) = True
isJust' Nothing' = False
