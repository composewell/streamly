module Main (main) where

import Data.List (elem, nub, sort)
import Data.Maybe (isNothing)
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
                    $ S.toList
                    $ Top.joinInnerMap (S.fromList ls0) (S.fromList ls1)
                let v2 = [
                            (fst i, fst i, fst j)
                            | i <- ls0, j <- nub ls1
                            , fst i == fst j
                          ]
                assert (sort v1 == sort v2)

-- XXX A bug need to be fixed in joinOuter function
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

joinOuterMap :: Property
joinOuterMap =
    forAll (listOf (chooseInt (min_value, max_value))) $ \ls0 ->
        forAll (listOf (chooseInt (min_value, max_value))) $ \ls1 ->
            monadicIO $
                action (map (\a -> (a,a)) ls0) (map (\a -> (a,a)) (nub ls1))

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.joinOuterMap (S.fromList ls0) (S.fromList ls1)
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

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Prelude.Top"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        -- Joins

        prop "joinInner" Main.joinInner
        prop "joinInnerMap" Main.joinInnerMap
        prop "joinOuter" Main.joinOuter
        prop "joinOuterMap" Main.joinOuterMap
