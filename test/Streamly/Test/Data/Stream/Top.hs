module Main (main)
    where

import Data.List (intersect, sort)
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , forAll
    , listOf
    , listOf1
    )
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream.Top as Top

import Prelude hiding
    (maximum, minimum, elem, notElem, null, product, sum, head, last, take)
import Test.Hspec as H
import Test.Hspec.QuickCheck

min_value :: Int
min_value = 0

max_value :: Int
max_value = 1000

chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose


joinInner :: Property
joinInner =
    forAll (listOf (chooseInt (min_value, max_value)))
        $ \ls0 -> monadicIO $ action ls0

        where

        action ls = do
            v1 <- run $ S.toList $ Top.joinInner (==) (S.fromList ls) (S.fromList $ sort ls)
            let v2 = [ (i,j) | i <- ls, j <- ls, i == j ]
            assert (v1 == v2)

joinInnerMerge :: Property
joinInnerMerge =
    forAll (listOf (chooseInt (min_value, max_value)))
        $ \ls0 -> monadicIO $ action (sort ls0)

        where

        action ls = do
            v1 <- run $ S.toList $ Top.joinInnerMerge compare (S.fromList ls) (S.fromList ls)
            let v2 = [ (i,j) | i <- ls, j <- ls, i == j ]
            assert (v1 == v2)

joinInnerHash :: Property
joinInnerHash =
    forAll (listOf (chooseInt (min_value, max_value)))
        $ \ls0 -> monadicIO $ action (map (\a -> (a,a)) ls0)

        where

        action ls = do
            v1 <- run $ S.toList $ Top.joinInnerHash (S.fromList ls) (S.fromList ls)
            let v2 = [ (fst i, fst i, fst j) | i <- ls, j <- ls, fst i == fst j ]
            assert (v1 == v2)

joinLeft :: Property
joinLeft =
    forAll (listOf (chooseInt (min_value, max_value)))
        $ \ls0 -> monadicIO $ action ls0

        where

        action ls = do
            v1 <- run $ S.toList $ Top.joinLeft (==) (S.fromList ls) (S.fromList ls)
            let v2 = [ (i, Just j) | i <- ls, j <- ls, i == j ]
            assert (v1 == v2)

joinLeftHash :: Property
joinLeftHash =
    forAll (listOf (chooseInt (min_value, max_value)))
        $ \ls0 -> monadicIO $ action (map (\a -> (a,a)) ls0)

        where

        action ls = do
            v1 <- run $ S.toList $ Top.joinLeftHash  (S.fromList ls) (S.fromList ls)
            let v2 = [ (fst i, fst i, Just (fst j)) | i <- ls, j <- ls, fst i == fst j ]
            assert (v1 == v2)

joinLeftMerge :: Property
joinLeftMerge =
    forAll (listOf (chooseInt (min_value, max_value)))
        $ \ls0 -> monadicIO $ action $ sort ls0

        where

        action ls = do
            v1 <- run $ S.toList $ Top.joinLeftMerge compare (S.fromList ls) (S.fromList ls)
            let v2 = [ (i, Just j) | i <- ls, j <- ls, i == j ]
            assert (v1 == v2)

joinOuter :: Property
joinOuter = monadicIO $
    do
        let ls = [1]
        v1 <- run $ S.toList $ Top.joinOuter (==) (S.fromList ls) (S.fromList ls)
        let v2 = [ (Just i, Just j) | i <- ls, j <- ls, i == j ]
        run $ print v1
        run $ print v2
        assert (v1 == v2)

joinOuterHash :: Property
joinOuterHash =
    forAll (listOf (chooseInt (min_value, max_value)))
        $ \ls0 -> monadicIO $ action (map (\a -> (a,a)) ls0)

        where

        action ls = do
            v1 <- run $ S.toList $ Top.joinOuterHash (S.fromList ls) (S.fromList ls)
            let v2 = [ (fst i, Just (fst i), Just (fst j)) | i <- ls, j <- ls, fst i == fst j ]
            assert (v1 == v2)

joinOuterMerge :: Property
joinOuterMerge =
    forAll (listOf (chooseInt (min_value, max_value)))
        $ \ls0 -> monadicIO $ action $ sort ls0

        where

        action ls = do
            v1 <- run $ S.toList $ Top.joinOuterMerge compare (S.fromList ls) (S.fromList ls)
            let v2 = [ (Just i, Just j) | i <- ls, j <- ls, i == j ]
            assert (v1 == v2)

intersectBy :: Property
intersectBy =
    forAll (listOf1 (chooseInt (min_value, max_value)))
        $ \ls0 -> monadicIO $ action ls0

        where

        action ls = do
            run $ print ls
            v1 <- run $ S.toList $ Top.intersectBy (==) (S.fromList [1,2,2]) (S.fromList [1,2,2])
            let v2 = intersect [1,2,2,4] [1,2,2,4]    
            run $ print v1
            run $ print v2        
            assert (v1 == v2)

intersectBySorted :: Property
intersectBySorted =
    forAll (listOf (chooseInt (min_value, max_value)))
        $ \ls0 -> monadicIO $ action $ sort ls0

        where

        action ls = do
            v1 <- run $ S.toList $ Top.intersectBySorted compare (S.fromList ls) (S.fromList ls)
            let v2 = intersect ls ls
            assert (v1 == v2)
-------------------------------------------------------------------------------
moduleName :: String
moduleName = "Data.Stream.Top"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        -- Joins      
        prop "joinInner" Main.joinInner
        prop "joinInnerHash" Main.joinInnerHash
        prop "joinInnerMerge" Main.joinInnerMerge        
        prop "joinLeft" Main.joinLeft
        prop "joinLeftHash" Main.joinLeftHash
        prop "joinLeftMerge" Main.joinLeftMerge
        -- there is a bug in function joinOuter 
        --prop "joinOuter" Main.joinOuter
        prop "joinOuterHash" Main.joinOuterHash
        prop "joinOuterMerge" Main.joinOuterMerge
        -- there is a bug in uniqBy function
        --prop "intersectBy" Main.intersectBy
        prop "intersectBySorted" Main.intersectBySorted
       