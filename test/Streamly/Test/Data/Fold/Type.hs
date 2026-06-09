{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Fold.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Fold.Type (main) where

import Data.Functor.Identity (Identity(..), runIdentity)
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude hiding (last, length, take, filter, scanl, foldl', concatMap)
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

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

foldl' :: [Int] -> Expectation
foldl' ls =
    Stream.fold (Fold.foldl' (+) 0) (Stream.fromList ls)
        `shouldReturn` Prelude.sum ls

foldlM' :: [Int] -> Expectation
foldlM' ls =
    Stream.fold
        (Fold.foldlM' (\b a -> return (b + a)) (return 0))
        (Stream.fromList ls)
        `shouldReturn` Prelude.sum ls

foldl1' :: Expectation
foldl1' = do
    Stream.fold (Fold.foldl1' (+)) (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` Just 6
    Stream.fold (Fold.foldl1' (+)) (Stream.fromList ([] :: [Int]))
        `shouldReturn` Nothing

foldl1M' :: Expectation
foldl1M' = do
    Stream.fold
        (Fold.foldl1M' (\b a -> return (b + a)))
        (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` Just 6
    Stream.fold
        (Fold.foldl1M' (\b a -> return (b + a)))
        (Stream.fromList ([] :: [Int]))
        `shouldReturn` Nothing

foldr' :: [Int] -> Expectation
foldr' ls =
    Stream.fold (Fold.foldr' (:) []) (Stream.fromList ls)
        `shouldReturn` ls

foldrM' :: [Int] -> Expectation
foldrM' ls =
    Stream.fold (Fold.foldrM' (\a b -> return (a : b)) (return [])) (Stream.fromList ls)
        `shouldReturn` ls

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

drain :: [Int] -> Expectation
drain ls = Stream.fold Fold.drain (Stream.fromList ls) `shouldReturn` ()

fromPure :: Expectation
fromPure =
    Stream.fold (Fold.fromPure (42 :: Int)) (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` 42

fromEffect :: Expectation
fromEffect =
    Stream.fold
        (Fold.fromEffect (return 42 :: IO Int))
        (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` 42

fromScanl :: [Int] -> Expectation
fromScanl ls =
    Stream.fold (Fold.fromScanl (Scanl.scanl' (+) 0)) (Stream.fromList ls)
        `shouldReturn` Prelude.sum ls

length :: [Int] -> Expectation
length ls =
    Stream.fold Fold.length (Stream.fromList ls)
        `shouldReturn` Prelude.length ls

toList :: [Int] -> Expectation
toList ls = Stream.fold Fold.toList (Stream.fromList ls) `shouldReturn` ls

toListRev :: [Int] -> Expectation
toListRev ls =
    Stream.fold Fold.toListRev (Stream.fromList ls) `shouldReturn` reverse ls

toStreamK :: [Int] -> Expectation
toStreamK ls = do
    sk <- Stream.fold Fold.toStreamK (Stream.fromList ls)
    result <- Stream.fold Fold.toList (Stream.fromStreamK sk)
    result `shouldBe` ls

toStreamKRev :: [Int] -> Expectation
toStreamKRev ls = do
    sk <- Stream.fold Fold.toStreamKRev (Stream.fromList ls)
    result <- Stream.fold Fold.toList (Stream.fromStreamK sk)
    result `shouldBe` reverse ls

genericLength :: [Int] -> Expectation
genericLength ls =
    Stream.fold (Fold.genericLength :: Fold.Fold IO Int Int) (Stream.fromList ls)
        `shouldReturn` Prelude.length ls

latest :: Expectation
latest = do
    Stream.fold Fold.latest (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` Just 3
    Stream.fold Fold.latest (Stream.fromList ([] :: [Int]))
        `shouldReturn` Nothing

last :: [String] -> Expectation
last ls = Stream.fold Fold.last (Stream.fromList ls) `shouldReturn` safeLast ls

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

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

lmap :: [Int] -> Expectation
lmap ls =
    Stream.fold (Fold.lmap (* 2) Fold.sum) (Stream.fromList ls)
        `shouldReturn` Prelude.sum (fmap (* 2) ls)

lmapM :: [Int] -> Expectation
lmapM ls =
    Stream.fold (Fold.lmapM (\x -> return (x * 2)) Fold.sum) (Stream.fromList ls)
        `shouldReturn` Prelude.sum (fmap (* 2) ls)

-------------------------------------------------------------------------------
-- Scanning
-------------------------------------------------------------------------------

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

postscanl :: Property
postscanl = forAll (listOf (chooseInt (0, 100))) $ \lst ->
    monadicIO $ do
    v1 :: [Int] <-
        run
            $ Stream.fold
                (Fold.postscanl (Scanl.scanl' (+) 0) Fold.toList)
            $ Stream.fromList lst
    let v2 = Prelude.scanl1 (+) lst
    assert (v1 == v2)

scanlFold :: Property
scanlFold = forAll (listOf (chooseInt (0, 100))) $ \lst ->
    monadicIO $ do
    v1 :: [Int] <-
        run
            $ Stream.fold
                (Fold.scanl (Scanl.scanl' (+) 0) Fold.toList)
            $ Stream.fromList lst
    let v2 = Prelude.scanl (+) 0 lst
    assert (v1 == v2)

scanlMany :: Property
scanlMany = forAll (listOf (chooseInt (0, 100))) $ \lst ->
    monadicIO $ do
    v1 :: [Int] <-
        run
            $ Stream.fold
                (Fold.scanlMany (Scanl.scanl' (+) 0) Fold.toList)
            $ Stream.fromList lst
    let v2 = Prelude.scanl (+) 0 lst
    assert (v1 == v2)

postscanlMaybe :: Expectation
postscanlMaybe =
    Stream.fold
        (Fold.postscanlMaybe
            (Scanl.scanl' (\_ a -> if even a then Just a else Nothing) Nothing)
            Fold.toList)
        (Stream.fromList [1,2,3,4,5,6 :: Int])
    `shouldReturn` [2,4,6]

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

catMaybes :: Expectation
catMaybes =
    Stream.fold
        (Fold.catMaybes Fold.toList)
        (Stream.fromList [Just 1, Nothing, Just 3, Nothing, Just 5 :: Maybe Int])
    `shouldReturn` [1,3,5]

scanMaybe :: Expectation
scanMaybe =
    Stream.fold
        (Fold.scanMaybe
            (Fold.filtering even)
            Fold.toList)
        (Stream.fromList [1,2,3,4,5,6 :: Int])
    `shouldReturn` [2,4,6]

filter :: [Int] -> Expectation
filter ls =
    Stream.fold (Fold.filter even Fold.toList) (Stream.fromList ls)
        `shouldReturn` Prelude.filter even ls

filtering :: Expectation
filtering = do
    Stream.fold (Fold.filtering even) (Stream.fromList [1,2,3,4,5 :: Int])
        `shouldReturn` Nothing
    Stream.fold (Fold.filtering even) (Stream.fromList [1,2,3,4 :: Int])
        `shouldReturn` Just 4
    Stream.fold (Fold.filtering even) (Stream.fromList ([] :: [Int]))
        `shouldReturn` Nothing

filterM :: [Int] -> Expectation
filterM ls =
    Stream.fold (Fold.filterM (return . even) Fold.toList) (Stream.fromList ls)
        `shouldReturn` Prelude.filter even ls

catLefts :: Expectation
catLefts =
    Stream.fold
        (Fold.catLefts Fold.toList)
        (Stream.fromList [Left 1, Right "a", Left 3, Right "b" :: Either Int String])
    `shouldReturn` [1,3]

catRights :: Expectation
catRights =
    Stream.fold
        (Fold.catRights Fold.toList)
        (Stream.fromList [Left "a", Right 2, Left "b", Right 4 :: Either String Int])
    `shouldReturn` [2,4]

catEithers :: Expectation
catEithers =
    Stream.fold
        (Fold.catEithers Fold.toList)
        (Stream.fromList [Left 1, Right 2, Left 3, Right 4 :: Either Int Int])
    `shouldReturn` [1,2,3,4]

-------------------------------------------------------------------------------
-- Trimming
-------------------------------------------------------------------------------

take :: [Int] -> Property
take ls =
    forAll (chooseInt (-1, Prelude.length ls + 2)) $ \n ->
            Stream.fold (Fold.take n Fold.toList) (Stream.fromList ls)
                `shouldReturn` Prelude.take n ls

taking :: Expectation
taking = do
    Stream.fold (Fold.taking 3) (Stream.fromList [1,2,3,4,5 :: Int])
        `shouldReturn` Just 3
    Stream.fold (Fold.taking 0) (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` Nothing
    Stream.fold (Fold.taking 3) (Stream.fromList ([] :: [Int]))
        `shouldReturn` Nothing

dropping :: Expectation
dropping = do
    Stream.fold (Fold.dropping 2) (Stream.fromList [1,2,3,4,5 :: Int])
        `shouldReturn` Just 5
    Stream.fold (Fold.dropping 0) (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` Just 3
    Stream.fold (Fold.dropping 10) (Stream.fromList [1,2,3 :: Int])
        `shouldReturn` Nothing

takeEndBy_ :: Property
takeEndBy_ =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let p = (== 1)
            f = Fold.takeEndBy_ p Fold.toList
            ys = Prelude.takeWhile (not . p) ls
         in case Stream.fold f (Stream.fromList ls) of
            Right xs -> checkListEqual xs ys
            Left _ -> property False

takeEndBy :: Expectation
takeEndBy = do
    r <- Stream.fold (Fold.takeEndBy (== (0 :: Int)) Fold.toList) (Stream.fromList [1,2,0,3,4])
    r `shouldBe` [1,2,0]

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

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

foldMaybes :: Expectation
foldMaybes = do
    Stream.fold (Fold.foldMaybes Fold.sum)
        (Stream.fromList [Just 1, Just 3 :: Maybe Int])
        `shouldReturn` Just 4
    Stream.fold (Fold.foldMaybes Fold.sum)
        (Stream.fromList [Just 1, Nothing, Just 3 :: Maybe Int])
        `shouldReturn` Nothing
    Stream.fold (Fold.foldMaybes Fold.sum)
        (Stream.fromList [Nothing, Nothing :: Maybe Int])
        `shouldReturn` Nothing

foldEithers :: Expectation
foldEithers = do
    r <- Stream.fold (Fold.foldEithers Fold.sum)
             (Stream.fromList [Right 1, Right 2, Right 3 :: Either String Int])
    r `shouldBe` Right 6

-------------------------------------------------------------------------------
-- Condition
-------------------------------------------------------------------------------

ifThen :: Expectation
ifThen = do
    r1 <- Stream.fold
              (Fold.ifThen (return True) Fold.sum Fold.length)
              (Stream.fromList [1,2,3 :: Int])
    r1 `shouldBe` 6
    r2 <- Stream.fold
              (Fold.ifThen (return False) Fold.sum Fold.length)
              (Stream.fromList [1,2,3 :: Int])
    r2 `shouldBe` 3

-------------------------------------------------------------------------------
-- Sequential application
-------------------------------------------------------------------------------

split_ :: Expectation
split_ =
    Stream.fold (Fold.split_ (Fold.take 3 Fold.drain) Fold.toList)
        (Stream.fromList [1,2,3,4,5 :: Int])
    `shouldReturn` [4,5]

-------------------------------------------------------------------------------
-- Repeated application
-------------------------------------------------------------------------------

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

manyPost :: Expectation
manyPost = do
    r <- Stream.fold
             (Fold.manyPost (Fold.take 2 Fold.toList) Fold.toList)
             (Stream.fromList [1,2,3,4 :: Int])
    r `shouldBe` [[1,2],[3,4],[]]

groupsOf :: Expectation
groupsOf = do
    r <- Stream.fold
             (Fold.groupsOf 3 Fold.sum Fold.toList)
             (Stream.fromList [1,2,3,4,5,6,7 :: Int])
    r `shouldBe` [6,15,7]

-------------------------------------------------------------------------------
-- Nested application
-------------------------------------------------------------------------------

concatMap :: Expectation
concatMap = do
    r <- Stream.fold
             (Fold.concatMap (\n -> Fold.take n Fold.toList) (Fold.take 2 Fold.length))
             (Stream.fromList [1,2,3,4,5 :: Int])
    r `shouldBe` [3,4]

duplicate :: Expectation
duplicate = do
    partialFold <- Stream.fold
                      (Fold.duplicate Fold.sum)
                      (Stream.fromList [1,2,3 :: Int])
    result <- Fold.finalM partialFold
    result `shouldBe` 6

-------------------------------------------------------------------------------
-- Parallel distribution
-------------------------------------------------------------------------------

teeWith :: Property
teeWith =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <-
            run
                $ Stream.fold (Fold.teeWith (,) Fold.sum Fold.length)
                $ Stream.fromList ls
        let v2 = Prelude.sum ls
            v3 = Prelude.length ls
        assert (v1 == (v2, v3))

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

-------------------------------------------------------------------------------
-- Parallel alternative
-------------------------------------------------------------------------------

shortest :: Expectation
shortest = do
    r <- Stream.fold
             (Fold.shortest (Fold.take 3 Fold.sum) (Fold.take 5 Fold.sum))
             (Stream.fromList [1..10 :: Int])
    r `shouldBe` Left 6

longest :: Expectation
longest = do
    r <- Stream.fold
             (Fold.longest (Fold.take 3 Fold.sum) (Fold.take 5 Fold.sum))
             (Stream.fromList [1..10 :: Int])
    r `shouldBe` Right 15

-------------------------------------------------------------------------------
-- Running a fold
-------------------------------------------------------------------------------

reduce :: Expectation
reduce = do
    fld <- Fold.reduce Fold.sum
    result <- Fold.finalM fld
    result `shouldBe` (0 :: Int)

snoc :: Expectation
snoc = do
    fld0 <- Fold.reduce Fold.sum
    fld1 <- Fold.snoc fld0 (1 :: Int)
    fld2 <- Fold.snoc fld1 2
    fld3 <- Fold.snoc fld2 3
    result <- Fold.finalM fld3
    result `shouldBe` 6

addOne :: Expectation
addOne = do
    fld0 <- Fold.reduce Fold.sum
    fld1 <- Fold.addOne (1 :: Int) fld0
    fld2 <- Fold.addOne 2 fld1
    result <- Fold.finalM fld2
    result `shouldBe` 3

snocM :: Expectation
snocM = do
    fld0 <- Fold.reduce Fold.sum
    fld1 <- Fold.snocM fld0 (return (5 :: Int))
    result <- Fold.finalM fld1
    result `shouldBe` 5

snocl :: Expectation
snocl = do
    result <- Fold.finalM $ Fold.snocl (Fold.snocl (Fold.snocl Fold.toList 1) 2) (3 :: Int)
    result `shouldBe` [1,2,3]

snoclM :: Expectation
snoclM = do
    result <- Fold.finalM $ Fold.snoclM (Fold.snoclM Fold.sum (return 1)) (return (2 :: Int))
    result `shouldBe` 3

finalM :: Expectation
finalM = do
    result <- Fold.finalM Fold.sum
    result `shouldBe` (0 :: Int)

closeFold :: Expectation
closeFold = do
    let closed = Fold.close Fold.sum
    result <- Stream.fold closed (Stream.fromList [1,2,3 :: Int])
    result `shouldBe` 0

isClosed :: Expectation
isClosed = do
    let open = Fold.sum :: Fold.Fold IO Int Int
        closed = Fold.close open
    b1 <- Fold.isClosed open
    b1 `shouldBe` False
    b2 <- Fold.isClosed closed
    b2 `shouldBe` True

-------------------------------------------------------------------------------
-- Transforming inner monad
-------------------------------------------------------------------------------

morphInner :: Expectation
morphInner = do
    let identityFold = Fold.foldl' (+) (0 :: Int)
        ioFold = Fold.morphInner (return . runIdentity) identityFold
    result <- Stream.fold ioFold (Stream.fromList [1,2,3])
    result `shouldBe` 6

generalizeInner :: Expectation
generalizeInner = do
    let identityFold = Fold.foldl' (+) (0 :: Int) :: Fold.Fold Identity Int Int
        ioFold = Fold.generalizeInner identityFold
    result <- Stream.fold ioFold (Stream.fromList [1,2,3])
    result `shouldBe` 6

-------------------------------------------------------------------------------
-- foldBreak
-------------------------------------------------------------------------------

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
        prop "foldl'" foldl'
        prop "foldlM'" foldlM'
        it "foldl1'" foldl1'
        it "foldl1M'" foldl1M'
        prop "foldr'" foldr'
        prop "foldrM'" foldrM'
        it "fromPure" fromPure
        it "fromEffect" fromEffect
        prop "fromScanl" fromScanl
        prop "drain" drain
        prop "length" length
        prop "toList" toList
        prop "toListRev" toListRev
        prop "toStreamK" toStreamK
        prop "toStreamKRev" toStreamKRev
        prop "genericLength" genericLength
        it "latest" latest
        prop "last" last
        prop "rmapM" rmapM
        prop "lmap" lmap
        prop "lmapM" lmapM
        prop "scan" scan
        prop "postscan" postscan
        prop "postscanl" postscanl
        prop "scanl" scanlFold
        prop "scanlMany" scanlMany
        it "postscanlMaybe" postscanlMaybe
        it "catMaybes" catMaybes
        it "scanMaybe" scanMaybe
        prop "filter" filter
        it "filtering" filtering
        prop "filterM" filterM
        it "catLefts" catLefts
        it "catRights" catRights
        it "catEithers" catEithers
        prop "take" take
        it "taking" taking
        it "dropping" dropping
        prop "takeEndBy_" takeEndBy_
        it "takeEndBy" takeEndBy
        prop "takeEndByOrMax" takeEndByOrMax
        it "foldMaybes" foldMaybes
        it "foldEithers" foldEithers
        it "ifThen" ifThen
        it "split_" split_
        prop "many" many
        it "manyPost" manyPost
        it "groupsOf" groupsOf
        it "concatMap" concatMap
        it "duplicate" duplicate
        prop "teeWith" teeWith
        prop "teeWithFstLength" teeWithFstLength
        prop "teeWithMinLength1" teeWithMinLength1
        prop "teeWithMinLength2" teeWithMinLength2
        it "shortest" shortest
        it "longest" longest
        it "reduce" reduce
        it "snoc" snoc
        it "addOne" addOne
        it "snocM" snocM
        it "snocl" snocl
        it "snoclM" snoclM
        it "finalM" finalM
        it "close" closeFold
        it "isClosed" isClosed
        it "morphInner" morphInner
        it "generalizeInner" generalizeInner
        prop "foldBreak" foldBreak
