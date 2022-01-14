module Main (main) where

import Data.List (sort)
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
            monadicIO $ action (map (\a -> (a,a)) ls0) (map (\a -> (a,a)) ls1)

            where

            action ls0 ls1 = do
                v1 <-
                    run
                    $ S.toList
                    $ Top.joinInnerMap (S.fromList ls0) (S.fromList ls1)
                let v2 = [
                            (fst i, fst i, fst j)
                            | i <- ls0, j <- ls1
                            , fst i == fst j
                          ]
                assert (v1 == v2)

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
