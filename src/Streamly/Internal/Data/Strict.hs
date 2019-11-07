{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      : Streamly.Internal.Data.Strict
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
module Streamly.Internal.Data.Strict
    (
      Tuple' (..)
    , Tuple3' (..)
    , Tuple4' (..)
    , Maybe' (..)
    , fromStrictMaybe
    , Either' (..)
    )
where

-------------------------------------------------------------------------------
-- Tuples
-------------------------------------------------------------------------------
--
data Tuple' a b = Tuple' !a !b deriving Show
data Tuple3' a b c = Tuple3' !a !b !c deriving Show
data Tuple4' a b c d = Tuple4' !a !b !c !d deriving Show

-------------------------------------------------------------------------------
-- Maybe
-------------------------------------------------------------------------------
--
-- | A strict 'Maybe'
data Maybe' a = Just' !a | Nothing' deriving Show

-- XXX perhaps we can use a type class having fromStrict/toStrict operations.
--
-- | Convert strict Maybe' to lazy Maybe
{-# INLINABLE fromStrictMaybe #-}
fromStrictMaybe :: Monad m => Maybe' a -> m (Maybe a)
fromStrictMaybe  Nothing' = return $ Nothing
fromStrictMaybe (Just' a) = return $ Just a

-------------------------------------------------------------------------------
-- Either
-------------------------------------------------------------------------------
--
-- | A strict 'Either'
data Either' a b = Left' !a | Right' !b deriving Show
