{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Fold.Combinators
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Fold.Combinators (main) where

import Data.IORef (newIORef, atomicModifyIORef)
import Data.Int (Int64)
import Data.Semigroup (Sum(..))

import qualified Prelude
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as F
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude hiding
    ( head, maximum, minimum, elem, notElem, null, product, sum
    , mconcat, foldMap, lookup, all, any, and, or, unzip, splitAt
    , maybe
    )
import Streamly.Test.Common (chooseInt, withNumTests)
import Streamly.Test.Data.Fold.Type
    (check, checkApprox, checkPostscanl, checkNoLaw)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , forAll
    , listOf1
    )
import Test.QuickCheck.Monadic (monadicIO, assert, run)

-------------------------------------------------------------------------------
-- Shared Fold/Scanl tests
-------------------------------------------------------------------------------

#include "Streamly/Test/Data/Scanl/CommonCombinators.hs"

-------------------------------------------------------------------------------
-- Fold specific tests
-------------------------------------------------------------------------------

intMin :: Int
intMin = minBound

intMax :: Int
intMax = maxBound

greater :: (a -> a -> Ordering) -> a -> a -> a
greater f x y = if f x y == GT then x else y

foldMaybe :: (b -> a -> b) -> b -> [a] -> Maybe b
foldMaybe f acc ls =
    case ls of
        [] -> Nothing
        _ -> Just (foldl f acc ls)

headl :: [a] -> Maybe a
headl [] = Nothing
headl (x:_) = Just x

nth :: Int -> [a] -> Maybe a
nth idx (x : xs)
    | idx == 0 = Just x
    | idx < 0 = Nothing
    | otherwise = nth (idx - 1) xs
nth _ [] = Nothing

neg :: (a -> Bool) -> a -> Bool
neg f x = not (f x)

predicate :: Int -> Bool
predicate x = x * x < 100

head :: [Int] -> Expectation
head ls = Stream.fold Fold.one (Stream.fromList ls) `shouldReturn` headl ls

stdDev :: Property
stdDev =
    forAll (listOf1 (chooseFloat (-100.0, 100.0)))
        $ \ls0 -> withNumTests 1000 $ monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold Fold.stdDev (Stream.fromList ls)
        let avg = Prelude.sum ls / fromIntegral (Prelude.length ls)
            se = Prelude.sum (fmap (\x -> (x - avg) * (x - avg)) ls)
            sd = sqrt $ se / fromIntegral (Prelude.length ls)
        assert (abs (v1 - sd) < 0.0001)

variance :: Property
variance =
    forAll (listOf1 (chooseFloat (-100.0, 100.0)))
        $ \ls0 -> withNumTests 1000 $ monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold Fold.variance (Stream.fromList ls)
        let avg = Prelude.sum ls / fromIntegral (Prelude.length ls)
            se = Prelude.sum (fmap (\x -> (x - avg) * (x - avg)) ls)
            vr = se / fromIntegral (Prelude.length ls)
        assert (abs (v1 - vr) < 0.01)

drainBy :: [Int] -> Expectation
drainBy ls =
    Stream.fold (Fold.drainBy return) (Stream.fromList ls) `shouldReturn` ()

lookup :: Property
lookup =
    forAll (chooseInt (1, 15))
        $ \key0 -> monadicIO $ action key0

    where

    action key = do
        let ls = [ (1, "first"), (2, "second"), (3, "third"), (4, "fourth")
                 , (5, "fifth"), (6, "fifth+first"), (7, "fifth+second")
                 , (8, "fifth+third"), (9, "fifth+fourth")
                 , (10, "fifth+fifth")
                 ]
        v1 <- run $ Stream.fold (Fold.lookup key) $ Stream.fromList ls
        let v2 = Prelude.lookup key ls
        assert (v1 == v2)

index :: Int -> [String] -> Expectation
index idx ls =
    let x = Stream.fold (Fold.index idx) (Stream.fromList ls)
    in x `shouldReturn` nth idx ls

find :: (Show a, Eq a) => (a -> Bool) -> [a] -> Expectation
find f ls = do
    y <- Stream.fold (Fold.findIndex f) (Stream.fromList ls)
    case y of
        Nothing ->
            let fld = Stream.fold (Fold.find f) (Stream.fromList ls)
            in fld `shouldReturn` Nothing
        Just idx ->
            let fld = Stream.fold (Fold.any f) (Stream.fromList $ Prelude.take idx ls)
            in fld `shouldReturn` False

findIndex :: (a -> Bool) -> [a] -> Expectation
findIndex f ls = do
    y <- Stream.fold (Fold.findIndex f) (Stream.fromList ls)
    case y of
        Nothing  ->
            let fld = Stream.fold (Fold.all $ neg f) (Stream.fromList ls)
            in fld `shouldReturn` True
        Just idx ->
            if idx == 0
            then
                Stream.fold (Fold.all f) (Stream.fromList []) `shouldReturn` True
            else
                Stream.fold (Fold.all f) (Stream.fromList $ Prelude.take idx ls)
                    `shouldReturn` False

elemIndex :: Int -> [Int] -> Expectation
elemIndex elm ls = do
    y <- Stream.fold (Fold.elemIndex elm) (Stream.fromList ls)
    case y of
        Nothing ->
            let fld = Stream.fold (Fold.any (== elm)) (Stream.fromList ls)
            in fld `shouldReturn` False
        Just idx ->
            let fld =
                    Stream.fold
                        (Fold.any (== elm))
                        (Stream.fromList $ Prelude.take idx ls)
            in fld `shouldReturn` False

null :: [Int] -> Expectation
null ls =
    Stream.fold Fold.null (Stream.fromList ls)
        `shouldReturn`
            case ls of
                [] -> True
                _ -> False

elem :: Int -> [Int] -> Expectation
elem elm ls = do
    y <- Stream.fold (Fold.elem elm) (Stream.fromList ls)
    let fld = Stream.fold (Fold.any (== elm)) (Stream.fromList ls)
    fld `shouldReturn` y

notElem :: Int -> [Int] -> Expectation
notElem elm ls = do
    y <- Stream.fold (Fold.notElem elm) (Stream.fromList ls)
    let fld = Stream.fold (Fold.any (== elm)) (Stream.fromList ls)
    fld `shouldReturn` not y

all :: (a -> Bool) -> [a] -> Expectation
all f ls =
    Stream.fold (Fold.all f) (Stream.fromList ls)
        `shouldReturn` Prelude.all f ls

any :: (a -> Bool) -> [a] -> Expectation
any f ls =
    Stream.fold (Fold.any f) (Stream.fromList ls)
        `shouldReturn` Prelude.any f ls

and :: [Bool] -> Expectation
and ls = Stream.fold Fold.and (Stream.fromList ls) `shouldReturn` Prelude.and ls

or :: [Bool] -> Expectation
or ls = Stream.fold Fold.or (Stream.fromList ls) `shouldReturn` Prelude.or ls

teeWithLength :: Property
teeWithLength =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold (Fold.tee Fold.sum Fold.length) $ Stream.fromList ls
        let v2 = Prelude.sum ls
            v3 = Prelude.length ls
        assert (v1 == (v2, v3))

teeWithMax :: Property
teeWithMax =
    forAll (listOf1 (chooseInt (intMin, intMax)))
       $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold (Fold.tee Fold.sum Fold.maximum) $ Stream.fromList ls
        let v2 = Prelude.sum ls
            v3 = foldMaybe (greater compare) intMin ls
        assert (v1 == (v2, v3))

-- In Scanl but not shared: partitionBy/partitionByM/partition diverge
-- semantically between Fold and Scanl. Fold returns the tuple of both branch
-- results; Scanl emits a single interleaved value per input (the just-updated
-- branch), so they are not the same combinator and cannot share a test.
partitionByM :: Property
partitionByM =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        let f x = if odd x then return (Left x) else return (Right x)
        v1 <-
            run
                $ Stream.fold (Fold.partitionByM f Fold.length Fold.length)
                $ Stream.fromList ls
        let v2 = foldl (\b a -> if odd a then b+1 else b) 0 ls
            v3 = foldl (\b a -> if even a then b+1 else b) 0 ls
        assert (v1 == (v2, v3))

partitionByFstM :: Property
partitionByFstM =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action _ = do
        let f x = if odd x then return (Left x) else return (Right x)
        v1 <-
            run
                $ Stream.fold
                      (Fold.partitionByFstM f (Fold.take 25 Fold.length) Fold.length)
                      (Stream.fromList ([1 .. 100] :: [Int]))
        let v2 = foldl (\b a -> if odd a then b+1 else b) 0 ([1..49] :: [Int])
            v3 = foldl (\b a -> if even a then b+1 else b) 0 ([1..49] :: [Int])
        assert (v1 == (v2, v3))

partitionByMinM1 :: Property
partitionByMinM1 =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action _ = do
        let f x = if odd x then return (Left x) else return (Right x)
        v1 <-
            run
                $ Stream.fold
                      (Fold.partitionByMinM f Fold.length (Fold.take 25 Fold.length))
                      (Stream.fromList ([1 .. 100] :: [Int]))
        let v2 = foldl (\b a -> if odd a then b+1 else b) 0 ([1..50] :: [Int])
            v3 = foldl (\b a -> if even a then b+1 else b) 0 ([1..50] :: [Int])
        assert (v1 == (v2, v3))

partitionByMinM2 :: Property
partitionByMinM2 =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action _ = do
        let f x = if odd x then return (Left x) else return (Right x)
        v1 <-
            run
                $ Stream.fold
                      (Fold.partitionByMinM f (Fold.take 25 Fold.length) Fold.length)
                      (Stream.fromList ([1 .. 100] :: [Int]))
        let v2 = foldl (\b a -> if odd a then b+1 else b) 0 ([1..49] :: [Int])
            v3 = foldl (\b a -> if even a then b+1 else b) 0 ([1..49] :: [Int])
        assert (v1 == (v2, v3))

partition :: Property
partition =
    monadicIO $ do
        v1 :: (Int, [String]) <-
            run
                $ Stream.fold (Fold.partition Fold.sum Fold.toList)
                $ Stream.fromList
                    [Left 1, Right "abc", Left 3, Right "xy", Right "pp2"]
        let v2 = (4,["abc","xy","pp2"])
        assert (v1 == v2)

splitAt :: Expectation
splitAt =
    Stream.fold
    (Fold.splitAt 6 Fold.toList Fold.toList)
    (Stream.fromList "Hello World!")
    `shouldReturn`
    ("Hello ","World!")

-------------------------------------------------------------------------------
-- Singleton folds
-------------------------------------------------------------------------------

satisfy :: Expectation
satisfy = do
    Stream.fold (Fold.satisfy even) (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` Nothing
    Stream.fold (Fold.satisfy even) (Stream.fromList [2,3,4 :: Int])
        `shouldReturn` Just 2

maybe :: Expectation
maybe = do
    let f x = if even x then Just (x * 2) else Nothing
    Stream.fold (Fold.maybe f) (Stream.fromList [2,3,4 :: Int])
        `shouldReturn` Just 4
    Stream.fold (Fold.maybe f) (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` Nothing

genericIndex :: Expectation
genericIndex = do
    Stream.fold (Fold.genericIndex (2 :: Int)) (Stream.fromList [1,2,3,4,5 :: Int])
        `shouldReturn` Just 3
    Stream.fold (Fold.genericIndex (10 :: Int)) (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` Nothing

findM :: Expectation
findM = do
    Stream.fold (Fold.findM (return . even)) (Stream.fromList [1,2,3,4 :: Int])
        `shouldReturn` Just 2
    Stream.fold (Fold.findM (return . even)) (Stream.fromList [1,3,5 :: Int])
        `shouldReturn` Nothing

-------------------------------------------------------------------------------
-- Running a fold
-------------------------------------------------------------------------------

drive :: [Int] -> Expectation
drive ls =
    Fold.drive (Stream.fromList ls) Fold.sum
        `shouldReturn` Prelude.sum ls

addStream :: Expectation
addStream = do
    fld0 <- Fold.reduce Fold.sum
    fld1 <- Fold.addStream (Stream.fromList [1,2,3 :: Int]) fld0
    result <- Fold.finalM fld1
    result `shouldBe` 6

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

slide2 :: Expectation
slide2 =
    Stream.fold
        (Fold.slide2 (Fold.foldl' (\_ (cur, prev) -> Prelude.maybe 0 (cur -) prev) 0))
        (Stream.fromList [1,3,6,10 :: Int])
    `shouldReturn` 4

takeEndBySeq :: Expectation
takeEndBySeq =
    Stream.fold
        (Fold.takeEndBySeq (Array.fromList [0,0 :: Int]) Fold.toList)
        (Stream.fromList [1,2,0,0,3,4])
    `shouldReturn` [1,2,0,0]

takeEndBySeq_ :: Expectation
takeEndBySeq_ =
    Stream.fold
        (Fold.takeEndBySeq_ (Array.fromList [0,0 :: Int]) Fold.toList)
        (Stream.fromList [1,2,0,0,3,4])
    `shouldReturn` [1,2]

distributeScan :: Expectation
distributeScan = do
    ref <- newIORef [Fold.take 2 Fold.sum, Fold.take 2 Fold.length :: Fold.Fold IO Int Int]
    let gen = atomicModifyIORef ref (\xs -> ([], xs))
    r <- Stream.fold Fold.toList
             $ Stream.postscanl (Fold.distributeScan gen)
             $ Stream.fromList [1..5 :: Int]
    r `shouldBe` [[], [], [2, 3], [], []]

unzipWithFstM :: Property
unzipWithFstM =
    monadicIO $ do
    v1 <-
        run
            $ Stream.fold
                  (Fold.unzipWithFstM
                      (\x -> return (x, x))
                      (Fold.take 3 Fold.sum)
                      Fold.sum)
                  (Stream.fromList [1..10 :: Int])
    let v2 = Prelude.sum [1..3]
        v3 = Prelude.sum [1..3]
    assert (v1 == (v2, v3))

unzipWithMinM :: Property
unzipWithMinM =
    monadicIO $ do
    v1 <-
        run
            $ Stream.fold
                  (Fold.unzipWithMinM
                      (\x -> return (x, x))
                      Fold.sum
                      (Fold.take 3 Fold.sum))
                  (Stream.fromList [1..10 :: Int])
    let v2 = Prelude.sum [1..3]
        v3 = Prelude.sum [1..3]
    assert (v1 == (v2, v3))

partitionBy :: Expectation
partitionBy = do
    let f x = if odd x then Left x else Right x
    r <- Stream.fold (Fold.partitionBy f Fold.length Fold.length)
             (Stream.fromList [1,2,3,4,5 :: Int])
    r `shouldBe` (3, 2)

moduleName :: String
moduleName = "Data.Fold.Combinators"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        -- Tests shared with the Scanl suite (see Scanl/CommonCombinators.hs)
        describe "common" commonCombinatorsSpec

        -- Before adding any tests here consider if they can be added to the
        -- common tests above.
        prop "drainBy" drainBy
        prop "head" head
        prop "stdDev" stdDev
        prop "variance" variance
        prop "index" index
        prop "find" $ find predicate
        prop "lookup" lookup
        prop "findIndex" $ findIndex predicate
        prop "elemIndex" $ elemIndex 10
        prop "null" null
        prop "elem" $ elem 10
        prop "notElem" $ notElem 10
        prop "all" $ all predicate
        prop "any" $ any predicate
        prop "and" and
        prop "or" or
        prop "teeWithLength" teeWithLength
        prop "teeWithMax" teeWithMax
        prop "partitionByM" partitionByM
        prop "partitionByFstM" partitionByFstM
        prop "partitionByMinM1" partitionByMinM1
        prop "partitionByMinM2" partitionByMinM2
        prop "partition" partition
        prop "splitAt" splitAt
        it "satisfy" satisfy
        it "maybe" maybe
        it "genericIndex" genericIndex
        it "findM" findM
        prop "drive" drive
        it "addStream" addStream
        it "slide2" slide2
        it "takeEndBySeq" takeEndBySeq
        it "takeEndBySeq_" takeEndBySeq_
        it "distributeScan" distributeScan
        prop "unzipWithFstM" unzipWithFstM
        prop "unzipWithMinM" unzipWithMinM
        it "partitionBy" partitionBy
