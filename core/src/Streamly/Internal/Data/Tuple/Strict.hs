-- |
-- Module      : Streamly.Internal.Data.Tuple.Strict
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
module Streamly.Internal.Data.Tuple.Strict
    (
      Tuple' (..)
    , Tuple3' (..)
    -- XXX Remove this type, use a fuse annotation in Tuple3' itself or use a
    -- custom type where fuse annotation is needed.
    , Tuple3Fused' (..)
    , Tuple4' (..)
    )
where

import Fusion.Plugin.Types (Fuse(..))

-- | A strict '(,)'
data Tuple' a b = Tuple' !a !b deriving Show

-- XXX Add TupleFused'

-- | A strict '(,,)'
{-# ANN type Tuple3Fused' Fuse #-}
data Tuple3' a b c = Tuple3' !a !b !c deriving Show

-- | A strict '(,,)'
data Tuple3Fused' a b c = Tuple3Fused' !a !b !c deriving Show

-- | A strict '(,,,)'
data Tuple4' a b c d = Tuple4' !a !b !c !d deriving Show
