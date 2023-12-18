{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Main (main) where

import Data.List (sort, sortBy)
import Data.Ord (comparing, Down(..))
import Data.Semigroup (Sum(..), getSum)
import Streamly.Test.Common (checkListEqual, listEquals)
import Test.QuickCheck
    ( Gen
    , Property
    , arbitrary
    , choose
    , forAll
    , listOf
    , listOf1
    , property
    , vectorOf
    , withMaxSuccess
    , generate
    )
import Control.Monad.IO.Class (liftIO)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Data.Map
import qualified Prelude
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude hiding
    (maximum, minimum, elem, notElem, null, product, sum, head, last, take)
import Test.Hspec as H
import Test.Hspec.QuickCheck

maxStreamLen :: Int
maxStreamLen = 1000

intMin :: Int
intMin = minBound

intMax :: Int
intMax = maxBound

min_value :: Int
min_value = 0

max_value :: Int
max_value = 10000

chooseInt :: (Int, Int) -> Gen Int
chooseInt = choose

{-# INLINE maxStreamLen #-}
{-# INLINE intMin #-}
{-# INLINE intMax #-}

rollingHashFirstN :: Property
rollingHashFirstN =
    forAll (choose (0, maxStreamLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \vec ->
                monadicIO $ do
                a <- run
                    $ Stream.fold Fold.rollingHash
                    $ Stream.take n
                    $ Stream.fromList  vec
                b <- run
                    $ Stream.fold (Fold.rollingHashFirstN n)
                    $ Stream.fromList  vec
                assert $ a == b

head :: [Int] -> Expectation
head ls = Stream.fold Fold.one (Stream.fromList  ls) `shouldReturn` headl ls

headl :: [a] -> Maybe a
headl [] = Nothing
headl (x:_) = Just x

length :: [Int] -> Expectation
length ls = Stream.fold Fold.length (Stream.fromList  ls) `shouldReturn` Prelude.length ls

sum :: [Int] -> Expectation
sum ls = Stream.fold Fold.sum (Stream.fromList  ls) `shouldReturn` Prelude.sum ls

product :: [Int] -> Expectation
product ls =
    Stream.fold Fold.product (Stream.fromList  ls) `shouldReturn` Prelude.product ls

lesser :: (a -> a -> Ordering) -> a -> a -> a
lesser f x y = if f x y == LT then x else y

greater :: (a -> a -> Ordering) -> a -> a -> a
greater f x y = if f x y == GT then x else y

foldMaybe :: (b -> a -> b) -> b -> [a] -> Maybe b
foldMaybe f acc ls =
    case ls of
        [] -> Nothing
        _ -> Just (foldl f acc ls)

maximumBy :: (Ord a, Show a) => a -> (a -> a -> Ordering) -> [a] -> Expectation
maximumBy genmin f ls =
    Stream.fold (Fold.maximumBy f) (Stream.fromList  ls)
        `shouldReturn` foldMaybe (greater f) genmin ls

maximum :: (Show a, Ord a) => a -> [a] -> Expectation
maximum genmin ls =
    Stream.fold Fold.maximum (Stream.fromList  ls)
        `shouldReturn` foldMaybe (greater compare) genmin ls

minimumBy :: (Ord a, Show a) => a -> (a -> a -> Ordering) -> [a] -> Expectation
minimumBy genmax f ls =
    Stream.fold (Fold.minimumBy f) (Stream.fromList  ls)
        `shouldReturn` foldMaybe (lesser f) genmax ls

minimum :: (Show a, Ord a) => a -> [a] -> Expectation
minimum genmax ls =
    Stream.fold Fold.minimum (Stream.fromList  ls)
        `shouldReturn` foldMaybe (lesser compare) genmax ls

toList :: [Int] -> Expectation
toList ls = Stream.fold Fold.toList (Stream.fromList  ls) `shouldReturn` ls

toListRev :: [Int] -> Expectation
toListRev ls = Stream.fold Fold.toListRev (Stream.fromList  ls) `shouldReturn` reverse ls

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

last :: [String] -> Expectation
last ls = Stream.fold Fold.last (Stream.fromList  ls) `shouldReturn` safeLast ls

mapMaybe :: [Int] -> Expectation
mapMaybe ls =
    let maybeEven x =
            if even x
            then Just x
            else Nothing
        f = Fold.mapMaybe maybeEven Fold.toList
     in Stream.fold f (Stream.fromList  ls) `shouldReturn` filter even ls

nth :: Int -> [a] -> Maybe a
nth idx (x : xs)
    | idx == 0 = Just x
    | idx < 0 = Nothing
    | otherwise = nth (idx - 1) xs
nth _ [] = Nothing

index :: Int -> [String] -> Expectation
index idx ls =
    let x = Stream.fold (Fold.index idx) (Stream.fromList  ls)
    in x `shouldReturn` nth idx ls

find :: (Show a, Eq a) => (a -> Bool) -> [a] -> Expectation
find f ls = do
    y <- Stream.fold (Fold.findIndex f) (Stream.fromList  ls)
    case y of
        Nothing ->
            let fld = Stream.fold (Fold.find f) (Stream.fromList  ls)
            in fld `shouldReturn` Nothing
        Just idx ->
            let fld = Stream.fold (Fold.any f) (Stream.fromList  $ Prelude.take idx ls)
            in fld `shouldReturn` False

neg :: (a -> Bool) -> a -> Bool
neg f x = not (f x)

findIndex :: (a -> Bool) -> [a] -> Expectation
findIndex f ls = do
    y <- Stream.fold (Fold.findIndex f) (Stream.fromList  ls)
    case y of
        Nothing  ->
            let fld = Stream.fold (Fold.all $ neg f) (Stream.fromList  ls)
            in fld `shouldReturn` True
        Just idx ->
            if idx == 0
            then
                Stream.fold (Fold.all f) (Stream.fromList  []) `shouldReturn` True
            else
                Stream.fold (Fold.all f) (Stream.fromList  $ Prelude.take idx ls)
                    `shouldReturn` False

predicate :: Int -> Bool
predicate x = x * x < 100

elemIndex :: Int -> [Int] -> Expectation
elemIndex elm ls = do
    y <- Stream.fold (Fold.elemIndex elm) (Stream.fromList  ls)
    case y of
        Nothing ->
            let fld = Stream.fold (Fold.any (== elm)) (Stream.fromList  ls)
            in fld `shouldReturn` False
        Just idx ->
            let fld =
                    Stream.fold
                        (Fold.any (== elm))
                        (Stream.fromList  $ Prelude.take idx ls)
            in fld `shouldReturn` False

null :: [Int] -> Expectation
null ls =
    Stream.fold Fold.null (Stream.fromList  ls)
        `shouldReturn`
            case ls of
                [] -> True
                _ -> False

elem :: Int -> [Int] -> Expectation
elem elm ls = do
    y <- Stream.fold (Fold.elem elm) (Stream.fromList  ls)
    let fld = Stream.fold (Fold.any (== elm)) (Stream.fromList  ls)
    fld `shouldReturn` y

notElem :: Int -> [Int] -> Expectation
notElem elm ls = do
    y <- Stream.fold (Fold.notElem elm) (Stream.fromList  ls)
    let fld = Stream.fold (Fold.any (== elm)) (Stream.fromList  ls)
    fld `shouldReturn` not y

all :: (a -> Bool) -> [a] -> Expectation
all f ls =
    Stream.fold (Fold.all f) (Stream.fromList  ls) `shouldReturn` Prelude.all f ls

any :: (a -> Bool) -> [a] -> Expectation
any f ls = Stream.fold (Fold.any f) (Stream.fromList  ls) `shouldReturn` Prelude.any f ls

and :: [Bool] -> Expectation
and ls = Stream.fold Fold.and (Stream.fromList  ls) `shouldReturn` Prelude.and ls

or :: [Bool] -> Expectation
or ls = Stream.fold Fold.or (Stream.fromList  ls) `shouldReturn` Prelude.or ls

take :: [Int] -> Property
take ls =
    forAll (chooseInt (-1, Prelude.length ls + 2)) $ \n ->
            Stream.fold (Fold.take n Fold.toList) (Stream.fromList  ls)
                `shouldReturn` Prelude.take n ls

takeEndBy_ :: Property
takeEndBy_ =
    forAll (listOf (chooseInt (0, 1))) $ \ls ->
        let p = (== 1)
            f = Fold.takeEndBy_ p Fold.toList
            ys = Prelude.takeWhile (not . p) ls
         in case Stream.fold f (Stream.fromList  ls) of
            Right xs -> checkListEqual xs ys
            Left _ -> property False

takeEndByOrMax :: Property
takeEndByOrMax =
    forAll (chooseInt (min_value, max_value)) $ \n ->
        forAll (listOf (chooseInt (0, 1))) $ \ls ->
            let p = (== 1)
                f = Fold.takeEndBy_ p (Fold.take n Fold.toList)
                ys = Prelude.take n (Prelude.takeWhile (not . p) ls)
             in case Stream.fold f (Stream.fromList  ls) of
                    Right xs -> checkListEqual xs ys
                    Left _ -> property False

chooseFloat :: (Float, Float) -> Gen Float
chooseFloat = choose

drain :: [Int] -> Expectation
drain ls = Stream.fold Fold.drain (Stream.fromList  ls) `shouldReturn` ()

drainBy :: [Int] -> Expectation
drainBy ls = Stream.fold (Fold.drainBy return) (Stream.fromList  ls) `shouldReturn` ()

mean :: Property
mean =
    forAll (listOf1 (chooseFloat (-100.0, 100.0)))
        $ \ls0 -> withMaxSuccess 1000 $ monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold Fold.mean (Stream.fromList  ls)
        let v2 = Prelude.sum ls / fromIntegral (Prelude.length ls)
        assert (abs (v1 - v2) < 0.0001)

stdDev :: Property
stdDev =
    forAll (listOf1 (chooseFloat (-100.0, 100.0)))
        $ \ls0 -> withMaxSuccess 1000 $ monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold Fold.stdDev (Stream.fromList  ls)
        let avg = Prelude.sum ls / fromIntegral (Prelude.length ls)
            se = Prelude.sum (fmap (\x -> (x - avg) * (x - avg)) ls)
            sd = sqrt $ se / fromIntegral (Prelude.length ls)
        assert (abs (v1 - sd) < 0.0001 )

variance :: Property
variance =
    forAll (listOf1 (chooseFloat (-100.0, 100.0)))
        $ \ls0 -> withMaxSuccess 1000 $ monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold Fold.variance (Stream.fromList  ls)
        let avg = Prelude.sum ls / fromIntegral (Prelude.length ls)
            se = Prelude.sum (fmap (\x -> (x - avg) * (x - avg)) ls)
            vr = se / fromIntegral (Prelude.length ls)
        assert (abs (v1 - vr) < 0.01 )

mconcat :: Property
mconcat =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold Fold.mconcat (fmap Sum $ Stream.fromList  ls)
        let v2 = Prelude.sum ls
        assert (getSum v1 == v2)

foldMap :: Property
foldMap =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold (Fold.foldMap Sum) $ Stream.fromList  ls
        let v2 = Prelude.sum ls
        assert (getSum v1 == v2)

foldMapM :: Property
foldMapM =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold (Fold.foldMapM (return . Sum)) $ Stream.fromList  ls
        let v2 = Prelude.sum ls
        assert (getSum v1 == v2)

lookup :: Property
lookup =
    forAll (chooseInt (1, 15))
        $ \key0 ->monadicIO $ action key0

    where

    action key = do
        let ls = [ (1, "first"), (2, "second"), (3, "third"), (4, "fourth")
                 , (5, "fifth"), (6, "fifth+first"), (7, "fifth+second")
                 , (8, "fifth+third"), (9, "fifth+fourth")
                 , (10, "fifth+fifth")
                 ]
        v1 <- run $ Stream.fold (Fold.lookup key) $ Stream.fromList  ls
        let v2 = Prelude.lookup key ls
        assert (v1 == v2)

rmapM :: Property
rmapM =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        let addLen x = return $ x + Prelude.length ls
            fld = Fold.rmapM addLen Fold.sum
            v2 = foldl (+) (Prelude.length ls) ls
        v1 <- run $ Stream.fold fld $ Stream.fromList  ls
        assert (v1 == v2)

teeWithLength :: Property
teeWithLength =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold (Fold.tee Fold.sum Fold.length) $ Stream.fromList  ls
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
                $ Stream.fromList  ls
        let v2 = Prelude.sum (Prelude.take 5 ls)
            v3 = Prelude.length (Prelude.take 5 ls)
        assert (v1 == (v2, v3))

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
                $ Stream.fromList  ls
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
                      (Stream.fromList  ([1 .. 100] :: [Int]))
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
                      (Stream.fromList  ([1 .. 100] :: [Int]))
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
                      (Stream.fromList  ([1 .. 100] :: [Int]))
        let v2 = foldl (\b a -> if odd a then b+1 else b) 0 ([1..49] :: [Int])
            v3 = foldl (\b a -> if even a then b+1 else b) 0 ([1..49] :: [Int])
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
                $ Stream.fromList  ls
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
                $ Stream.fromList  ls
        let v2 = Prelude.sum (Prelude.take 5 ls)
            v3 = Prelude.length (Prelude.take 5 ls)
        assert (v1 == (v2, v3))

teeWithMax :: Property
teeWithMax =
    forAll (listOf1 (chooseInt (intMin, intMax)))
       $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <- run $ Stream.fold (Fold.tee Fold.sum Fold.maximum) $ Stream.fromList  ls
        let v2 = Prelude.sum ls
            v3 = foldMaybe (greater compare) intMin ls
        assert (v1 == (v2, v3))

distribute :: Property
distribute =
    forAll (listOf1 (chooseInt (intMin, intMax)))
        $ \ls0 -> monadicIO $ action ls0

    where

    action ls = do
        v1 <-
            run $ Stream.fold (Fold.distribute [Fold.sum, Fold.length]) $ Stream.fromList  ls
        let v2 = Prelude.sum ls
            v3 = Prelude.length ls
        assert (v1 == [v2, v3])

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

unzip :: Property
unzip =
    monadicIO $ do
    v1 :: (Int, [String]) <-
        run
            $ Stream.fold (Fold.unzip Fold.sum Fold.toList)
            $ Stream.fromList  [(1, "aa"), (2, "bb"), (3, "cc")]
    let v2 = (6, ["aa", "bb", "cc"])
    assert (v1 == v2)

postscan :: Property
postscan = forAll (listOf (chooseInt (intMin, intMax))) $ \ls ->
    monadicIO $ do
    v1 :: [Int] <-
        run
            $ Stream.fold (Fold.postscan Fold.sum Fold.toList)
            $ Stream.fromList  ls
    let v2 = scanl1 (+) ls
    assert (v1 == v2)

many :: Property
many =
    forAll (listOf (chooseInt (0, 100))) $ \lst ->
    forAll (chooseInt (1, 100)) $ \i ->
        monadicIO $ do
            let strm = Stream.fromList  lst
            r1 <- Stream.fold (Fold.many (split i) Fold.toList) strm
            r2 <- Stream.fold Fold.toList $ Stream.foldMany (split i) strm
            assert $ r1 == r2

    where

    split i = Fold.take i Fold.toList

foldBreak :: [Int] -> Property
foldBreak ls = monadicIO $ do
    (mbh, rest) <- run $ Stream.foldBreak Fold.one (Stream.fromList  ls)
    rests <- run $ Stream.fold Fold.toList rest
    assert (mbh == headl ls)
    listEquals (==) rests (taill ls)

    where

    taill :: [a] -> [a]
    taill [] = []
    taill (_:xs) = xs

demux :: Expectation
demux =
    let table "SUM" = return Fold.sum
        table "PRODUCT" = return Fold.product
        table _ = return Fold.length
        input = Stream.fromList  (
                [ ("SUM", 1)
                , ("abc", 1)
                , ("PRODUCT", 2)
                , ("abc", 2)
                , ("SUM",3)
                , ("xyz", 1)
                , ("PRODUCT", 4)
                , ("xyz", 2)
                , ("abc", 2)
                ] :: [(String, Int)])
    in Stream.fold
        (Fold.demuxKvToMap table)
        input
        `shouldReturn`
        Data.Map.fromList [("PRODUCT", 8),("SUM", 4),("abc",3),("xyz",2)]


demuxWith :: Expectation
demuxWith =
    let getKey x | even x = "SUM"
                 | otherwise = "PRODUCT"

        getFold "SUM" = return Fold.sum
        getFold "PRODUCT" = return Fold.product
        getFold _ = error "demuxWith: bug"

        input = Stream.fromList  [1, 2, 3, 4 :: Int]
    in Stream.fold
        (Fold.demuxToContainer getKey (getFold . getKey))
        input
        `shouldReturn`
        Data.Map.fromList  [("PRODUCT",3),("SUM",6)]

classifyWith :: Expectation
classifyWith =
    let input = Stream.fromList  [("ONE",1),("ONE",1.1),("TWO",2), ("TWO",2.2)]
    in Stream.fold
        (Fold.toContainer fst (Fold.lmap snd Fold.toList))
        input
        `shouldReturn`
        Data.Map.fromList
        [("ONE",[1.0, 1.1 :: Double]), ("TWO",[2.0, 2.2])]

classify :: Expectation
classify =
    let input =
            Stream.fromList
            [
              ("ONE", (1::Int, 1))
            , ("ONE", (1, 1.1:: Double))
            , ("TWO", (2, 2))
            , ("TWO",(2, 2.2))
            ]
    in Stream.fold
        (Fold.kvToMap (Fold.lmap snd Fold.toList))
        input
        `shouldReturn`
        Data.Map.fromList
        [("ONE",[1.0, 1.1 :: Double]), ("TWO",[2.0, 2.2])]

splitAt :: Expectation
splitAt =
    Stream.fold
    (Fold.splitAt 6 Fold.toList Fold.toList)
    (Stream.fromList  "Hello World!")
    `shouldReturn`
    ("Hello ","World!")

scan :: Property
scan = forAll (listOf (chooseInt (0, 100))) $ \lst ->
    monadicIO $ do
    v1 :: [Int] <-
        run
            $ Stream.fold (Fold.scan Fold.sum Fold.toList)
            $ Stream.fromList  lst
    let v2 = scanl (+) 0 lst
    assert (v1 == v2)

topBy :: Bool -> Property
topBy isTop = forAll (listOf (chooseInt (-50, 100))) $ \ls0 ->
        monadicIO $ action ls0

        where

        action ls = do

            let n0 = Prelude.length ls
            n <- liftIO $ generate $ chooseInt (-2, n0 + 2)
            if isTop
            then do
                lst <- Stream.fold (Fold.top n) (Stream.fromList  ls) >>= MArray.toList
                assert ((Prelude.take n . sortBy (comparing Down)) ls ==  lst)
            else do
                lst <- Stream.fold (Fold.bottom n) (Stream.fromList  ls) >>= MArray.toList
                assert ((Prelude.take n . sort) ls ==  lst)

top :: Property
top = topBy True

bottom :: Property
bottom = topBy False

nub :: Property
nub = monadicIO $ do
    vals <- Stream.fold Fold.toList
            $ Stream.catMaybes
            $ Stream.postscan Fold.nub
            $ Stream.fromList  [1::Int, 1, 2, 3, 4, 4, 5, 1, 5, 7]
    assert (vals == [1, 2, 3, 4, 5, 7])

moduleName :: String
moduleName = "Data.Fold"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        -- Folds
        -- Accumulators
        prop "mconcat" Main.mconcat
        prop "foldMap" Main.foldMap
        prop "foldMapM" Main.foldMapM

        prop "drain" Main.drain
        prop "drainBy" Main.drainBy
        prop "last" last
        prop "length" Main.length
        prop "sum" sum
        prop "product" product
        prop "maximumBy" $ maximumBy intMin compare
        prop "maximum" $ maximum intMin
        prop "minimumBy" $ minimumBy intMax compare
        prop "minimum" $ minimum intMax
        prop "mean" Main.mean
        prop "stdDev" Main.stdDev
        prop "variance" Main.variance
        prop "rollingHashFirstN" rollingHashFirstN

        prop "toList" toList
        prop "toListRev" toListRev
        prop "demux" demux
        prop "demuxWith" demuxWith
        prop "classifyWith" classifyWith
        prop "classify" classify

        -- Terminating folds
        prop "index" index
        prop "head" head
        prop "find" $ find predicate
        prop "lookup" Main.lookup
        prop "findIndex" $ findIndex predicate
        prop "elemIndex" $ elemIndex 10
        prop "null" null
        prop "elem" $ elem 10
        prop "notElem" $ notElem 10
        prop "all" $ Main.all predicate
        prop "any" $ Main.any predicate
        prop "and" Main.and
        prop "or" Main.or
        prop "top" Main.top
        prop "bottom" Main.bottom
        prop "nub" Main.nub

        -- Combinators

        -- Transformation
        prop "scan" scan
        prop "postscan" Main.postscan
        -- rsequence
        -- Functor instance
        prop "rmapM" Main.rmapM
        -- lmap/lmapM

        -- Filtering
        -- filter/filterM
        -- catMaybes
        prop "mapMaybe" mapMaybe

        -- Trimming
        prop "take" take
        -- takeEndBy
        prop "takeEndBy_" takeEndBy_
        prop "takeEndByOrMax" takeEndByOrMax

        -- Appending
        -- splitWith

        -- Distributing
        -- tee
        prop "teeWithLength" Main.teeWithLength
        prop "teeWithFstLength" Main.teeWithFstLength
        prop "teeWithMinLength1" Main.teeWithMinLength1
        prop "teeWithMinLength2" Main.teeWithMinLength2
        prop "teeWithMax" Main.teeWithMax
        prop "partitionByM" Main.partitionByM
        prop "partitionByFstM" Main.partitionByFstM
        prop "partitionByMinM1" Main.partitionByMinM1
        prop "partitionByMinM2" Main.partitionByMinM2
        prop "distribute" Main.distribute

        -- Partitioning
        prop "partition" Main.partition
        prop "partitionByM" partitionByM

        -- Unzipping
        prop "unzip" Main.unzip
        prop "splitAt" Main.splitAt

        -- Nesting
        prop "many" Main.many
        -- concatMap
        -- chunksOf

        prop "foldBreak" foldBreak
