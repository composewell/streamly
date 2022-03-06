module Main (main)
    where

import Data.List (intersect, sort, union)
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
                let v2 = ls0 `intersect` ls1
                assert (v1 == sort v2)

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
        -- intersect
        prop "intersectBySorted" Main.intersectBySorted
        prop "unionBySorted" Main.unionBySorted
