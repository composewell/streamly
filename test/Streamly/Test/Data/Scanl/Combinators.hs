{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Scanl.Combinators
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Scanl.Combinators (main) where

import Data.Int (Int64)
import Data.Semigroup (Sum(..))
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Pipe as Pipe
import qualified Streamly.Internal.Data.Scanl as F
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import qualified Prelude
import Prelude hiding (maximum, minimum, product, sum, mconcat, foldMap, maybe)

import Streamly.Test.Common (withNumTests)
import Streamly.Test.Data.Scanl.Type (check, checkApprox, checkPostscanl)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, choose, forAll, listOf1)

#include "Streamly/Test/Data/Scanl/CommonCombinators.hs"

-------------------------------------------------------------------------------
-- Scanl-only tests (these combinators are not exported by the Fold module, or
-- their scan output cannot be shared via the common 'check' harness).
-------------------------------------------------------------------------------

-- 'compose' scans the input through the left scan and feeds each of its outputs
-- (including the initial extract) to the right scan.
composeS :: Expectation
composeS =
    check (F.compose F.sum F.toList) ([1, 2, 3] :: [Int])
        [[0], [0, 1], [0, 1, 3], [0, 1, 3, 6]]

-- 'composeMany' restarts the left scan with a fresh state each time it
-- terminates. Here the left scan (take 2 sum) emits a running sum of every two
-- inputs which the right scan (sum) accumulates.
composeManyS :: Expectation
composeManyS =
    check (F.composeMany (F.take 2 F.sum) F.sum) ([1, 2, 3, 4, 5] :: [Int])
        [0, 1, 4, 7, 14, 19]

-- 'with' adapts a stateful combinator (here 'indexed') so that the supplied
-- predicate also sees the state (the index). This keeps elements at even
-- indices.
withS :: Expectation
withS =
    check (F.with F.indexed F.filter (even . fst) F.toList) "abcde"
        ["", "a", "a", "ac", "ac", "ace"]

pipeS :: Expectation
pipeS =
    check (F.pipe (Pipe.map (* 2)) F.sum) ([1, 2, 3] :: [Int]) [0, 2, 6, 12]

topByS :: Expectation
topByS =
    check (F.rmapM MArray.toList (F.topBy compare 3)) ([5, 1, 4, 2, 3] :: [Int])
        [[], [5], [5, 1], [5, 4, 1], [5, 4, 2], [5, 4, 3]]

bottomByS :: Expectation
bottomByS =
    check (F.rmapM MArray.toList (F.bottomBy compare 3)) ([5, 1, 4, 2, 3] :: [Int])
        [[], [5], [1, 5], [1, 4, 5], [1, 2, 4], [1, 2, 3]]

indexingWithS :: Expectation
indexingWithS =
    check (F.indexingWith 0 (+ 2)) "abc"
        [Nothing, Just (0, 'a'), Just (2, 'b'), Just (4, 'c')]

indexingS :: Expectation
indexingS =
    check F.indexing "abc"
        [Nothing, Just (0, 'a'), Just (1, 'b'), Just (2, 'c')]

indexingRevS :: Expectation
indexingRevS =
    check (F.indexingRev 5) "abc"
        [Nothing, Just (5, 'a'), Just (4, 'b'), Just (3, 'c')]

takingEndByUS :: Expectation
takingEndByUS =
    check (F.takingEndBy_ (== 3)) ([1, 2, 3, 4, 5] :: [Int])
        [Nothing, Just 1, Just 2, Nothing]

mapMaybeMS :: Expectation
mapMaybeMS =
    check
        (F.mapMaybeM (\x -> return (if even x then Just x else Nothing)) F.toList)
        ([1, 2, 3, 4] :: [Int])
        [[], [], [2], [2], [2, 4]]

-- An Unfold that streams the elements of an input list.
unfoldList :: Monad m => Unfold.Unfold m [a] a
unfoldList =
    Unfold.unfoldrM
        (\xs -> return (case xs of { [] -> Nothing; (y:ys) -> Just (y, ys) }))

unfoldEachS :: Expectation
unfoldEachS =
    check (F.unfoldEach unfoldList F.toList) ([[1, 2], [3], [4, 5]] :: [[Int]])
        [[], [1, 2], [1, 2, 3], [1, 2, 3, 4, 5]]

unfoldManyS :: Expectation
unfoldManyS =
    check (F.unfoldMany unfoldList F.toList) ([[1, 2], [3], [4, 5]] :: [[Int]])
        [[], [1, 2], [1, 2, 3], [1, 2, 3, 4, 5]]

-- 'defaultSalt' is the default salt used by 'rollingHash'. It is part of the
-- output contract, so the test duplicates the constant rather than importing it.
defaultSaltS :: Expectation
defaultSaltS = F.defaultSalt `shouldBe` (-2578643520546668380 :: Int64)

teeS :: Expectation
teeS =
    check (F.tee F.sum F.length) ([1, 2, 3] :: [Int])
        [(0, 0), (1, 1), (3, 2), (6, 3)]

-- Unlike the Fold 'partition' which returns the tuple of both branch results, a
-- Scanl emits a single interleaved value per input: the just-updated branch.
partitionByS :: Expectation
partitionByS =
    check
        (F.partitionBy (\x -> if odd x then Left x else Right x) F.length F.length)
        ([1, 2, 3, 4, 5] :: [Int])
        [0, 1, 1, 2, 2, 3]

partitionByMS :: Expectation
partitionByMS =
    check
        (F.partitionByM
            (\x -> return (if odd x then Left x else Right x)) F.length F.length)
        ([1, 2, 3, 4, 5] :: [Int])
        [0, 1, 1, 2, 2, 3]

partitionS :: Expectation
partitionS =
    check (F.partition F.toList F.toList)
        ([Left 1, Right 2, Left 3, Right 4] :: [Either Int Int])
        [[], [1], [2], [1, 3], [2, 4]]

-------------------------------------------------------------------------------
-- Deprecated combinators (aliases for compose / composeMany)
-------------------------------------------------------------------------------

scanlS :: Expectation
scanlS =
    check (F.scanl F.sum F.toList) ([1, 2, 3] :: [Int])
        [[0], [0, 1], [0, 1, 3], [0, 1, 3, 6]]

scanlManyS :: Expectation
scanlManyS =
    check (F.scanlMany (F.take 2 F.sum) F.sum) ([1, 2, 3, 4, 5] :: [Int])
        [0, 1, 4, 7, 14, 19]

moduleName :: String
moduleName = "Data.Scanl.Combinators"

main :: IO ()
main = hspec $
    describe moduleName $ do
        describe "common" commonCombinatorsSpec

        -- Before adding any tests here consider if it can be added to the
        -- common tests above.
        it "compose" composeS
        it "composeMany" composeManyS
        it "with" withS
        it "pipe" pipeS
        it "topBy" topByS
        it "bottomBy" bottomByS
        it "indexingWith" indexingWithS
        it "indexing" indexingS
        it "indexingRev" indexingRevS
        it "takingEndBy_" takingEndByUS
        it "mapMaybeM" mapMaybeMS
        it "unfoldEach" unfoldEachS
        it "unfoldMany" unfoldManyS
        it "defaultSalt" defaultSaltS
        it "tee" teeS
        it "partitionBy" partitionByS
        it "partitionByM" partitionByMS
        it "partition" partitionS
        it "scanl" scanlS
        it "scanlMany" scanlManyS
