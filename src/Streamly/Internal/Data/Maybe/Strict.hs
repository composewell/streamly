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
    )
where

-- | A strict 'Maybe'
data Maybe' a = Just' !a | Nothing' deriving Show

-- | Convert strict Maybe' to lazy Maybe
{-# INLINABLE toMaybe #-}
toMaybe :: Maybe' a -> Maybe a
toMaybe  Nothing' = Nothing
toMaybe (Just' a) = Just a
