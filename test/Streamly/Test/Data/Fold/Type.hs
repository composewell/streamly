{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Fold.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Fold.Type (main) where

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude hiding (last, length, take)
import qualified Prelude

import Streamly.Test.Common (checkListEqual, chooseInt, listEquals)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, listOf, listOf1, property)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

intMin :: Int
intMin = minBound

intMax :: Int
intMax = maxBound

min_value :: Int
min_value = 0

max_value :: Int
max_value = 10000

headl :: [a] -> Maybe a
headl [] = Nothing
headl (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

drain :: [Int] -> Expectation
drain ls = Stream.fold Fold.drain (Stream.fromList ls) `shouldReturn` ()

length :: [Int] -> Expectation
length ls =
    Stream.fold Fold.length (Stream.fromList ls)
        `shouldReturn` Prelude.length ls

toList :: [Int] -> Expectation
toList ls = Stream.fold Fold.toList (Stream.fromList ls) `shouldReturn` ls

toListRev :: [Int] -> Expectation
toListRev ls =
    Stream.fold Fold.toListRev (Stream.fromList ls) `shouldReturn` reverse ls

last :: [String] -> Expectation
last ls = Stream.fold Fold.last (Stream.fromList ls) `shouldReturn` safeLast ls

rmapM :: Property
rmapM =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        let addLen x = return $ x + Prelude.length ls
            fld = Fold.rmapM addLen Fold.sum
            v2 = foldl (+) (Prelude.length ls) ls
        v1 <- run $ Stream.fold fld $ Stream.fromList ls
        assert (v1 == v2)

scan :: Property
scan = forAll (listOf (chooseInt (0, 100))) $ \lst ->
    monadicIO $ do
    v1 :: [Int] <-
        run
            $ Stream.fold (Fold.scan Fold.sum Fold.toList)
            $ Stream.fromList lst
    let v2 = Prelude.scanl (+) 0 lst
    assert (v1 == v2)

postscan :: Property
postscan = forAll (listOf (chooseInt (intMin, intMax))) $ \ls ->
    monadicIO $ do
    v1 :: [Int] <-
        run
            $ Stream.fold (Fold.postscan Fold.sum Fold.toList)
            $ Stream.fromList ls
    let v2 = Prelude.scanl1 (+) ls
    assert (v1 == v2)

take :: [Int] -> Property
take ls =
    forAll (chooseInt (-1, Prelude.length ls + 2)) $ \n ->
            Stream.fold (Fold.take n Fold.toList) (Stream.fromList ls)
                `shouldReturn` Prelude.take n ls

takeEndBy_ :: Property
takeEndBy_ =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let p = (== 1)
            f = Fold.takeEndBy_ p Fold.toList
            ys = Prelude.takeWhile (not . p) ls
         in case Stream.fold f (Stream.fromList ls) of
            Right xs -> checkListEqual xs ys
            Left _ -> property False

takeEndByOrMax :: Property
takeEndByOrMax =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            let p = (== 1)
                f = Fold.takeEndBy_ p (Fold.take n Fold.toList)
                ys = Prelude.take n (Prelude.takeWhile (not . p) ls)
             in case Stream.fold f (Stream.fromList ls) of
                    Right xs -> checkListEqual xs ys
                    Left _ -> property False

teeWithFstLength :: Property
teeWithFstLength =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <-
            run
                $ Stream.fold (Fold.teeWithFst (,) (Fold.take 5 Fold.sum) Fold.length)
                $ Stream.fromList ls
        let v2 = Prelude.sum (Prelude.take 5 ls)
            v3 = Prelude.length (Prelude.take 5 ls)
        assert (v1 == (v2, v3))

teeWithMinLength1 :: Property
teeWithMinLength1 =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <-
            run
                $ Stream.fold (Fold.teeWithMin (,) (Fold.take 5 Fold.sum) Fold.length)
                $ Stream.fromList ls
        let v2 = Prelude.sum (Prelude.take 5 ls)
            v3 = Prelude.length (Prelude.take 5 ls)
        assert (v1 == (v2, v3))

teeWithMinLength2 :: Property
teeWithMinLength2 =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <-
            run
                $ Stream.fold (Fold.teeWithMin (,) Fold.sum (Fold.take 5 Fold.length))
                $ Stream.fromList ls
        let v2 = Prelude.sum (Prelude.take 5 ls)
            v3 = Prelude.length (Prelude.take 5 ls)
        assert (v1 == (v2, v3))

many :: Property
many =
    forAll (listOf (chooseInt (0, 100))) $ \lst ->
    forAll (chooseInt (1, 100)) $ \i ->
        monadicIO $ do
            let strm = Stream.fromList lst
            r1 <- run $ Stream.fold (Fold.many (split i) Fold.toList) strm
            r2 <- run $ Stream.fold Fold.toList $ Stream.foldMany (split i) strm
            assert $ r1 == r2

    where

    split i = Fold.take i Fold.toList

foldBreak :: [Int] -> Property
foldBreak ls = monadicIO $ do
    (mbh, rest) <- run $ Stream.foldBreak Fold.one (Stream.fromList ls)
    rests <- run $ Stream.fold Fold.toList rest
    assert (mbh == headl ls)
    listEquals (==) rests (taill ls)

    where

    taill :: [a] -> [a]
    taill [] = []
    taill (_:xs) = xs

moduleName :: String
moduleName = "Data.Fold.Type"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        prop "drain" drain
        prop "length" length
        prop "toList" toList
        prop "toListRev" toListRev
        prop "last" last
        prop "rmapM" rmapM
        prop "scan" scan
        prop "postscan" postscan
        prop "take" take
        prop "takeEndBy_" takeEndBy_
        prop "takeEndByOrMax" takeEndByOrMax
        prop "teeWithFstLength" teeWithFstLength
        prop "teeWithMinLength1" teeWithMinLength1
        prop "teeWithMinLength2" teeWithMinLength2
        prop "many" many
        prop "foldBreak" foldBreak
