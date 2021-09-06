module Main (main)
    where

import Data.Maybe (isNothing)
--import Data.Void (Void(..))
import Data.List (elem, intersect, nub, sort, union, (\\))
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , forAll
    , listOf
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
                    $ S.toList
                    $ Top.joinInner eq (S.fromList ls0) (S.fromList $ sort ls1)
                let v2 = [ (i,j) | i <- ls0, j <- ls1, i == j ]
                assert (v1 == v2)

joinInnerMerge :: Property
joinInnerMerge =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action (sort ls0) (sort (ls1))

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.joinInnerMerge
                        compare
                        (S.fromList ls0)
                        (S.fromList ls1)
                let v2 = [ (i,j) | i <- ls0, j <- ls1, i == j ]
                assert (v1 == v2)

joinInnerHash :: Property
joinInnerHash =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action (map (\a -> (a,a)) ls0) (map (\a -> (a,a)) ls1)

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.joinInnerHash (S.fromList ls0) (S.fromList ls1)
                let v2 = [
                            (fst i, fst i, fst j)
                            | i <- ls0, j <- ls1
                            , fst i == fst j
                          ]
                assert (v1 == v2)

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
                    $ S.toList
                    $ Top.joinLeft eq (S.fromList ls0) (S.fromList ls1)
                let v2 = do
                        i <- ls0
                        if (elem i ls1)
                        then return (i, Just i)
                        else return (i, Nothing)
                assert (v1 == v2)

joinLeftHash :: Property
joinLeftHash =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            -- nub the second list as no way to validate using list functions
            monadicIO $
                action (map (\a -> (a,a)) ls0) (map (\a -> (a,a)) (nub ls1))

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.joinLeftHash  (S.fromList ls0) (S.fromList ls1)
                let v2 = do
                        i <- ls0
                        if (elem i ls1)
                        then return (fst i, fst i, Just (fst i))
                        else return (fst i, fst i, Nothing)
                assert (v1 == v2)

joinLeftMerge :: Property
joinLeftMerge =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            -- nub the second list as no way to validate using list functions
            monadicIO $ action (sort ls0) (sort (nub ls1))

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.joinLeftMerge
                        compare
                        (S.fromList ls0)
                        (S.fromList ls1)
                let v2 = do
                        i <- ls0
                        if (elem i ls1)
                        then return (i, Just i)
                        else return (i, Nothing)
                assert (v1 == v2)
{- XXX A bug need to be fixed in joinOuter function
joinOuter :: Property
joinOuter =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action ls0 ls1

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.joinOuter eq (S.fromList ls0) (S.fromList ls1)
                let v2 = [ (Just i, Just j) | i <- ls0, j <- ls1, i == j ]
                assert (v1 == v2)
-}                

joinOuterHash :: Property
joinOuterHash =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $
                action (map (\a -> (a,a)) ls0) (map (\a -> (a,a)) (nub ls1))

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.joinOuterHash (S.fromList ls0) (S.fromList ls1)
                let v2 = do
                        i <- ls0
                        if (elem i ls1)
                        then return (fst i, Just (fst i), Just (fst i))
                        else return (fst i, Just (fst i), Nothing)
                    v3 = do
                        j <- ls1
                        if (elem j ls0)
                        then return (fst j, Just (fst j), Just (fst j))
                        else return (fst j, Nothing, Just (fst j))
                    v4 = filter (\(_, a2, _) -> isNothing a2)  v3
                assert (v1 == v2 ++ v4)

joinOuterMerge :: Property
joinOuterMerge =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action (sort ls0) (sort (nub ls1))

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.joinOuterMerge
                        compare
                        (S.fromList ls0)
                        (S.fromList ls1)
                let v2 = do
                        i <- ls0
                        if (elem i ls1)
                        then return (Just i, Just i)
                        else return (Just i, Nothing)
                    v3 = do
                        j <- ls1
                        if (elem j ls0)
                        then return (Just j, Just j)
                        else return (Nothing, Just j)
                    v4 = filter (\(a1, _) -> isNothing a1) v3

                assert (sort v1 == sort (v2 ++ v4))

{- XXX A bug need to be fixed in uniqBy function
intersectBy :: Property
intersectBy =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action ls0 ls1

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.intersectBy eq (S.fromList ls0) (S.fromList ls1)
                let v2 = intersect ls0 ls1
                assert (sort v1 == sort v2)
-}                

intersectBySorted :: Property
intersectBySorted =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action (sort ls0) (sort ls1)

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.intersectBySorted
                        compare
                        (S.fromList ls0)
                        (S.fromList ls1)
                let v2 = intersect ls0 ls1
                assert (v1 == sort v2)

differenceBy :: Property
differenceBy =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action ls0 ls1

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.differenceBy eq (S.fromList ls0) (S.fromList ls1)
                let v2 = ls0 \\ ls1
                assert (sort v1 == sort v2)

differenceBySorted :: Property
differenceBySorted =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action (sort ls0) (sort ls1)

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.differenceBySorted
                        compare
                        (S.fromList ls0)
                        (S.fromList ls1)
                let v2 = ls0 \\ ls1
                assert (v1 == sort v2)

unionBy :: Property
unionBy =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action ls0 ls1

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.unionBy eq (S.fromList ls0) (S.fromList ls1)
                let v2 = union ls0 ls1
                assert (sort v1 == sort v2)

unionBySorted :: Property
unionBySorted =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $ action (sort ls0) (sort ls1)

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.unionBySorted
                        compare
                        (S.fromList ls0)
                        (S.fromList ls1)
                let v2 = sort $ union ls0 ls1
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
        prop "differenceBy" Main.differenceBy
        prop "differenceBySorted" Main.differenceBySorted
        prop "unionBy" Main.unionBy
        prop "unionBySorted" Main.unionBySorted
