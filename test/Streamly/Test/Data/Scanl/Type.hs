{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Scanl.Type
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Scanl.Type
    (main, check, checkApprox, checkPostscanl) where

import Data.Functor.Identity (Identity, runIdentity)
import qualified Streamly.Internal.Data.Scanl as F
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude hiding (last, length, take, filter, scanl, foldl', concatMap)
import qualified Prelude

import Streamly.Test.Common (chooseInt)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, forAll)

-------------------------------------------------------------------------------
-- Scanl and Fold test sharing mechanism
-------------------------------------------------------------------------------

-- Shared tests are written in CPP files that are included in both Fold tests
-- and Scanl tests. For example, "Streamly/Test/Data/Scanl/CommonType.hs"
-- contains tests that are common to Scanl/Type.hs module and the Fold module.
--
-- The including module must bring the following into scope BEFORE the #include:
--   * qualifier  F        -- Streamly.Internal.Data.{Fold,Scanl}
--   * qualifier  Stream   -- Streamly.Internal.Data.Stream
--   * type Op             -- F.Fold or F.Scanl
--   * check :: (Eq b, Show b) => Op IO a b -> [a] -> [b] -> Expectation
--   * checkApprox :: (Ord b, Fractional b, Show b)
--                       => Op IO a b -> [a] -> [b] -> Expectation
--     (epsilon-equality counterpart of 'check' for floating-point results)
--   plus the usual hspec / QuickCheck imports, Data.Functor.Identity, and
--   Streamly.Test.Common (chooseInt).
--
-- In every shared test the @expected@ value is the *inclusive prescan list*
-- (Prelude.scanl f z), i.e. exactly what Stream.scanl emits: the leading
-- initial value followed by one output per input (filtering scans emit the
-- unchanged accumulator for filtered elements; terminating scans truncate at,
-- and including, the terminating step). From this list:
--   * the Fold includer checks the final value  (Prelude.last expected)
--   * the Scanl includer checks the full Stream.scanl output (== expected) and
--     the Stream.postscanl output (== drop 1 expected, the initial omitted)
--
-- Combinators are grouped by their *Scanl* source submodule (these Common*.hs
-- fragments live under Scanl/, so Scanl is the authoritative source for
-- placement).

-- | A Scanl is exercised by scanning a stream and verifying the resulting
-- stream of intermediate outputs. The shared @expected@ value is the inclusive
-- prescan list (what 'Stream.scanl' emits); 'Stream.postscanl' is the same with
-- the leading initial value dropped.
type Op = F.Scanl

check :: (Eq b, Show b) => Op IO a b -> [a] -> [b] -> Expectation
check cons xs expected = do
    Stream.toList (Stream.scanl cons (Stream.fromList xs))
        `shouldReturn` expected
    Stream.toList (Stream.postscanl cons (Stream.fromList xs))
        `shouldReturn` drop 1 expected

-- | Epsilon-equality counterpart of 'check' for Fractional results whose
-- floating-point output is only approximately equal to the reference (e.g.
-- 'mean'). Each emitted value must be within 1e-4 of the expected prescan value.
checkApprox ::
    (Ord b, Fractional b, Show b) => Op IO a b -> [a] -> [b] -> Expectation
checkApprox cons xs expected = do
    out <- Stream.toList (Stream.scanl cons (Stream.fromList xs))
    out `shouldSatisfy` approxEqList expected
    pout <- Stream.toList (Stream.postscanl cons (Stream.fromList xs))
    pout `shouldSatisfy` approxEqList (drop 1 expected)

    where

    approxEqList as bs =
        Prelude.length as == Prelude.length bs
            && and (zipWith (\a b -> abs (a - b) < 1e-4) as bs)

-- | For combinators that only support postscan (their scanl initial value is
-- undefined, e.g. rollingMap), so 'check' cannot be used. @expected@ is the
-- Stream.postscanl output (one value per input, no leading initial).
checkPostscanl :: (Eq b, Show b) => Op IO a b -> [a] -> [b] -> Expectation
checkPostscanl cons xs expected =
    Stream.toList (Stream.postscanl cons (Stream.fromList xs))
        `shouldReturn` expected

#include "Streamly/Test/Data/Scanl/CommonType.hs"

-------------------------------------------------------------------------------
-- Scanl-only tests.
-------------------------------------------------------------------------------

scanlVsPostscanl :: Expectation
scanlVsPostscanl = do
    a <- Stream.toList (Stream.scanl F.sum (Stream.fromList [1, 2, 3 :: Int]))
    a `shouldBe` [0, 1, 3, 6]
    b <- Stream.toList (Stream.postscanl F.sum (Stream.fromList [1, 2, 3 :: Int]))
    b `shouldBe` [1, 3, 6]

-- 'Scanl.postscanl' composes two scans. Verify both the normal case and the
-- case where the inner scan is Done at the initial step (its value is dropped,
-- so the resulting scan is also Done at init and postscanl emits nothing).
postscanlCompose :: Expectation
postscanlCompose = do
    Stream.toList
        (Stream.postscanl (F.postscanl F.sum F.toList) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [[1], [1, 3], [1, 3, 6]]
    -- scanl additionally emits the initial value of the composed scan
    Stream.toList
        (Stream.scanl (F.postscanl F.sum F.toList) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [[], [1], [1, 3], [1, 3, 6]]
    -- done-at-init: postscanl emits nothing, scanl emits the default value
    Stream.toList
        (Stream.postscanl (F.postscanl (F.take 0 F.sum) F.toList) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` ([] :: [[Int]])
    Stream.toList
        (Stream.scanl (F.postscanl (F.take 0 F.sum) F.toList) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [[] :: [Int]]

postscanlMaybeCompose :: Expectation
postscanlMaybeCompose = do
    Stream.toList
        (Stream.postscanl (F.postscanlMaybe (F.filtering even) F.length) (Stream.fromList [1 .. 6 :: Int]))
        `shouldReturn` [0, 1, 1, 2, 2, 3]
    Stream.toList
        (Stream.postscanl (F.postscanlMaybe (fmap Just (F.take 0 F.sum)) F.length) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` ([] :: [Int])

moduleName :: String
moduleName = "Data.Scanl.Type"

main :: IO ()
main = hspec $
    describe moduleName $ do
        describe "common" commonTypeSpec

        -- Before adding any tests here consider if it can be added to the
        -- common tests above.
        it "scanl emits initial, postscanl omits it" scanlVsPostscanl
        it "postscanl (compose)" postscanlCompose
        it "postscanlMaybe (compose)" postscanlMaybeCompose
