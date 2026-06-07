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

-------------------------------------------------------------------------------
-- Deduplication
-------------------------------------------------------------------------------

testOrdNub :: Expectation
testOrdNub =
    Stream.toList (Stream.ordNub (Stream.fromList [1, 2, 1, 3, 2, 4 :: Int]))
        `shouldReturn` [1, 2, 3, 4]

testOrdNubEmpty :: Expectation
testOrdNubEmpty =
    Stream.toList (Stream.ordNub (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

testOrdNubAllSame :: Expectation
testOrdNubAllSame =
    Stream.toList (Stream.ordNub (Stream.fromList [1, 1, 1 :: Int]))
        `shouldReturn` [1]

testOrdNubNoDups :: Expectation
testOrdNubNoDups =
    Stream.toList (Stream.ordNub (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 3]

-------------------------------------------------------------------------------
-- Left Join
-------------------------------------------------------------------------------

testLeftJoin :: Expectation
testLeftJoin = do
    xs <- Stream.toList
            $ Stream.leftJoin (==)
                (Stream.fromList [1, 2, 3 :: Int])
                (Stream.fromList [2, 3, 4])
    xs `shouldBe` [(1, Nothing), (2, Just 2), (3, Just 3)]

testLeftJoinEmpty :: Expectation
testLeftJoinEmpty = do
    xs <- Stream.toList
            $ Stream.leftJoin (==)
                (Stream.fromList ([] :: [Int]))
                (Stream.fromList [1, 2, 3])
    xs `shouldBe` []

testLeftJoinNoMatch :: Expectation
testLeftJoinNoMatch = do
    xs <- Stream.toList
            $ Stream.leftJoin (==)
                (Stream.fromList [1, 2 :: Int])
                (Stream.fromList [3, 4])
    xs `shouldBe` [(1, Nothing), (2, Nothing)]

-------------------------------------------------------------------------------
-- Outer Join
-------------------------------------------------------------------------------

testOuterJoin :: Expectation
testOuterJoin = do
    xs <- fmap sort $ Stream.toList
            $ Stream.outerJoin (==)
                (Stream.fromList [1, 2, 3 :: Int])
                (Stream.fromList [2, 3, 4])
    xs `shouldBe` [(Nothing, Just 4), (Just 1, Nothing), (Just 2, Just 2), (Just 3, Just 3)]

testOuterJoinEmptyLeft :: Expectation
testOuterJoinEmptyLeft = do
    xs <- fmap sort $ Stream.toList
            $ Stream.outerJoin (==)
                (Stream.fromList ([] :: [Int]))
                (Stream.fromList [1, 2])
    xs `shouldBe` [(Nothing, Just 1), (Nothing, Just 2)]

testOuterJoinEmptyRight :: Expectation
testOuterJoinEmptyRight = do
    xs <- fmap sort $ Stream.toList
            $ Stream.outerJoin (==)
                (Stream.fromList [1, 2 :: Int])
                (Stream.fromList [])
    xs `shouldBe` [(Just 1, Nothing), (Just 2, Nothing)]

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
        describe "Deduplication" $ do
            it "ordNub" testOrdNub
            it "ordNub empty" testOrdNubEmpty
            it "ordNub all same" testOrdNubAllSame
            it "ordNub no duplicates" testOrdNubNoDups

        describe "Left Join" $ do
            it "leftJoin" testLeftJoin
            it "leftJoin empty left" testLeftJoinEmpty
            it "leftJoin no match" testLeftJoinNoMatch

        describe "Outer Join" $ do
            it "outerJoin" testOuterJoin
            it "outerJoin empty left" testOuterJoinEmptyLeft
            it "outerJoin empty right" testOuterJoinEmptyRight

        describe "Ord Joins" $ do
            prop "innerOrdJoin" joinInnerMap
            prop "outerOrdJoin" joinOuterMap
            prop "leftOrdJoin" joinLeftMap
