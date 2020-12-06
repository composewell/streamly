-- |
-- Module      : Streamly.Internal.Data.Either.Strict
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
module Streamly.Internal.Data.Either.Strict
    ( Either' (..)
    , isLeft'
    , isRight'
    , fromLeft'
    , fromRight'
    )
where

-- | A strict 'Either'
data Either' a b = Left' !a | Right' !b deriving Show

-- | Return 'True' if the given value is a Left', 'False' otherwise.
{-# INLINABLE isLeft' #-}
isLeft' :: Either' a b -> Bool
isLeft' (Left'  _) = True
isLeft' (Right' _) = False

-- | Return 'True' if the given value is a Right', 'False' otherwise.
{-# INLINABLE isRight' #-}
isRight' :: Either' a b -> Bool
isRight' (Left'  _) = False
isRight' (Right' _) = True

-- XXX This is partial. We can use a default value instead.
-- | Return the contents of a Left'-value or errors out.
{-# INLINABLE fromLeft' #-}
fromLeft' :: Either' a b -> a
fromLeft' (Left' a) = a
fromLeft' _ = error "fromLeft' expecting a Left'-value"

-- | Return the contents of a Right'-value or errors out.
{-# INLINABLE fromRight' #-}
fromRight' :: Either' a b -> b
fromRight' (Right' b) = b
fromRight' _ = error "fromRight' expecting a Right'-value"
