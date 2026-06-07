-- |
-- Module      : Streamly.Test.Data.Stream.Container
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Container (main) where

import Data.List (nub, sort)
import Data.Maybe (isNothing)
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , forAll
    , listOf
    )
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Hspec as H
import Test.Hspec.QuickCheck

min_value :: Int
min_value = 0

max_value :: Int
max_value = 10000

chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose

joinInnerMap :: Property
joinInnerMap =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $
                action
                (map (\a -> (a,a)) ls0)
                (map (\a -> (a,a)) ls1)

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ Stream.toList
                    $ Stream.innerOrdJoin (Stream.fromList ls0) (Stream.fromList ls1)
                let v2 = [
                            (fst i, fst i, fst j)
                            | i <- ls0, j <- nub ls1
                            , fst i == fst j
                          ]
                assert (sort v1 == sort v2)

joinOuterList :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Maybe Int, Maybe Int)]
joinOuterList ls0 ls1 =
    let v2 = do
            i <- ls0
            if i `elem` ls1
            then return (fst i, Just (fst i), Just (fst i))
            else return (fst i, Just (fst i), Nothing)
        v3 = do
            j <- ls1
            if j `elem` ls0
            then return (fst j, Just (fst j), Just (fst j))
            else return (fst j, Nothing, Just (fst j))
        v4 = filter (\(_, a2, _) -> isNothing a2)  v3
    in v2 ++ v4

{-
eq :: Int -> Int -> Bool
eq = (==)

joinOuter :: Property
joinOuter =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action ls0 (nub ls1)
            where
            action ls0 ls1 = do
                v1 <-
                    run
                    $ Stream.toList
                    $ Stream.outerJoin eq (Stream.fromList ls0) (Stream.fromList ls1)
                let v2 = joinOuterList
                         (map (\a -> (a, a)) ls0)
                         (map (\a -> (a, a)) ls1)
                    v3 = map (\(_, v10, v20) -> (v10, v20)) v2
                assert (sort v1 == sort v3)
-}

joinOuterMap :: Property
joinOuterMap =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $
                action
                (map (\a -> (a,a)) (nub ls0))
                (map (\a -> (a,a)) (nub ls1))

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ Stream.toList
                    $ Stream.outerOrdJoin (Stream.fromList ls0) (Stream.fromList ls1)
                let v2 = joinOuterList ls0 ls1
                assert (sort v1 == sort v2)

joinLeftList :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int, Maybe Int)]
joinLeftList ls0 ls1 =
    let v = do
            i <- ls0
            if i `elem` ls1
            then return (fst i, fst i, Just (fst i))
            else return (fst i, fst i, Nothing)
    in v

{-
joinLeft :: Property
joinLeft =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            -- nub the second list as no way to validate using list functions
            monadicIO $ action ls0 (nub ls1)

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ Stream.toList
                    $ Stream.leftJoin eq (Stream.fromList ls0) (Stream.fromList ls1)
                let v2 = joinLeftList (map (\a -> (a,a)) ls0) (map (\a -> (a,a)) ls1)
                    v3 = map (\ (_, x1, x2) -> (x1, x2)) v2
                assert (v1 == v3)
-}

joinLeftMap :: Property
joinLeftMap =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            -- nub the second list as no way to validate using list functions
            monadicIO $
                action (map (\a -> (a,a)) ls0) (map (\a -> (a,a)) (nub ls1))

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ Stream.toList
                    $ Stream.leftOrdJoin (Stream.fromList ls0) (Stream.fromList ls1)
                let v2 = joinLeftList ls0 ls1
                assert (v1 == v2)

moduleName :: String
moduleName = "Data.Stream.Container"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        prop "joinInnerMap" joinInnerMap
        -- prop "joinOuter" joinOuter
        prop "joinOuterMap" joinOuterMap
        -- prop "joinLeft" joinLeft
        prop "joinLeftMap" joinLeftMap
