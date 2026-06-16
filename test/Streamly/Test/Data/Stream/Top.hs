module Streamly.Test.Data.Stream.Top (main) where

import Data.List (deleteFirstsBy, intersect, sort, unionBy)
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , forAll
    , listOf
    )
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude hiding
    (maximum, minimum, notElem, null, product, sum, head, last, take)
import Test.Hspec as H
import Test.Hspec.QuickCheck

min_value :: Int
min_value = 0

max_value :: Int
max_value = 10000

chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose

eq :: Int -> Int -> Bool
eq = (==)

joinInner :: Property
joinInner =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action ls0 ls1

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ Stream.toList
                    $ Stream.innerJoin eq (Stream.fromList ls0) (Stream.fromList $ sort ls1)
                let v2 = [ (i,j) | i <- ls0, j <- ls1, i == j ]
                assert (v1 == v2)


intersectBy ::
       ([Int] -> [Int])
    -> (   (Int -> Int -> a)
        -> Stream.Stream IO Int
        -> Stream.Stream IO Int
        -> Stream.Stream IO Int
       )
    -> (Int -> Int -> a)
    -> Property
intersectBy srt intersectFunc cmp =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action (srt ls0) (srt ls1)

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ Stream.toList
                    $ intersectFunc
                        cmp
                        (Stream.fromList ls0)
                        (Stream.fromList ls1)
                let v2 = ls0 `intersect` ls1
                assert (sort v1 == sort v2)

-------------------------------------------------------------------------------
-- deleteFirstsBy
-------------------------------------------------------------------------------

testDeleteFirstsBy :: Expectation
testDeleteFirstsBy = do
    xs <- Stream.toList $
              Stream.deleteFirstsBy eq
                  (Stream.fromList [1, 2, 3, 4 :: Int])
                  (Stream.fromList [2, 3])
    xs `shouldBe` [1, 4]

testDeleteFirstsByDups :: Expectation
testDeleteFirstsByDups = do
    xs <- Stream.toList $
              Stream.deleteFirstsBy eq
                  (Stream.fromList [1, 1, 2 :: Int])
                  (Stream.fromList [1, 1])
    xs `shouldBe` [2]

testDeleteFirstsByEmpty :: Expectation
testDeleteFirstsByEmpty = do
    xs <- Stream.toList $
              Stream.deleteFirstsBy eq
                  (Stream.fromList [1, 2, 3 :: Int])
                  (Stream.fromList [])
    xs `shouldBe` [1, 2, 3]

deleteFirstsByMatchesList :: Property
deleteFirstsByMatchesList =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ do
                v1 <- run $ Stream.toList $
                                Stream.deleteFirstsBy eq
                                    (Stream.fromList ls0)
                                    (Stream.fromList ls1)
                let v2 = deleteFirstsBy eq ls0 ls1
                assert (v1 == v2)

-------------------------------------------------------------------------------
-- unionBy
-------------------------------------------------------------------------------

testUnionBy :: Expectation
testUnionBy = do
    xs <- Stream.toList $
              Stream.unionBy eq
                  (Stream.fromList [1, 2, 2, 4 :: Int])
                  (Stream.fromList [1, 1, 2, 3, 3])
    xs `shouldBe` [1, 2, 2, 4, 3]

testUnionByDisjoint :: Expectation
testUnionByDisjoint = do
    xs <- Stream.toList $
              Stream.unionBy eq
                  (Stream.fromList [1, 2 :: Int])
                  (Stream.fromList [3, 4])
    xs `shouldBe` [1, 2, 3, 4]

testUnionBySubset :: Expectation
testUnionBySubset = do
    xs <- Stream.toList $
              Stream.unionBy eq
                  (Stream.fromList [1, 2, 3 :: Int])
                  (Stream.fromList [1, 2, 3])
    xs `shouldBe` [1, 2, 3]

unionByMatchesList :: Property
unionByMatchesList =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ do
                v1 <- run $ Stream.toList $
                                Stream.unionBy eq
                                    (Stream.fromList ls0)
                                    (Stream.fromList ls1)
                let v2 = unionBy eq ls0 ls1
                assert (v1 == v2)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Stream"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        -- Joins
        prop "joinInner" joinInner
        -- intersect
        prop "intersectBy"
            (intersectBy id Stream.intersectBy (==))
        prop "intersectBySorted"
            (intersectBy sort Stream.sortedIntersectBy compare)
        -- deleteFirstsBy
        it "deleteFirstsBy basic" testDeleteFirstsBy
        it "deleteFirstsBy duplicates" testDeleteFirstsByDups
        it "deleteFirstsBy empty delete list" testDeleteFirstsByEmpty
        prop "deleteFirstsBy matches Data.List" deleteFirstsByMatchesList
        -- unionBy
        it "unionBy overlapping streams" testUnionBy
        it "unionBy disjoint streams" testUnionByDisjoint
        it "unionBy identical streams" testUnionBySubset
        prop "unionBy matches Data.List" unionByMatchesList
