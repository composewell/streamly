{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Fold.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Fold.Type
    (main, check, checkApprox, checkPostscanl, checkNoLaw) where

import Control.Exception (SomeException, evaluate, try)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.IORef (newIORef, readIORef, modifyIORef')
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as F
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude hiding (last, length, take, filter, scanl, foldl', concatMap)
import qualified Prelude

import Streamly.Test.Common (checkListEqual, chooseInt, listEquals)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, listOf, listOf1, property)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

-------------------------------------------------------------------------------
-- Shared Fold/Scanl tests (see Scanl/CommonType.hs)
-------------------------------------------------------------------------------

-- | A Fold is exercised by folding a stream to a single final value. The shared
-- @expected@ value is the inclusive prescan list; the fold result is its last
-- element.
type Op = F.Fold

check :: (Eq b, Show b, Show a) => Op IO a b -> [a] -> [b] -> Expectation
check cons xs expected = do
    Stream.fold cons (Stream.fromList xs) `shouldReturn` Prelude.last expected
    filterLaw cons xs

-- | Same as 'check' but does NOT apply the filter law. Use this only for the
-- few tests that pass bottom (e.g. 'error') input elements to verify early
-- termination -- the law would force those elements.
checkNoLaw :: (Eq b, Show b) => Op IO a b -> [a] -> [b] -> Expectation
checkNoLaw cons xs expected =
    Stream.fold cons (Stream.fromList xs) `shouldReturn` Prelude.last expected

-- | The filter law (an independent, black-box oracle): wrapping a fold in
-- 'Fold.filter' must produce the same result as folding the pre-filtered input:
--
--     fold (Fold.filter p s) xs === fold s (filter p xs)
--
-- This holds for EVERY fold because both sides feed the fold the identical
-- accepted subsequence. (A 'Fold' has no 'Continue'; 'Fold.filter' keeps the
-- accumulator unchanged for rejected inputs, which is the Fold counterpart of a
-- scan's 'Continue'.) Folded into 'check'/'checkPostscanl' so it applies to
-- every shared and Fold-specific test.
filterLawPred :: Show a => a -> Bool
filterLawPred x = even (Prelude.length (Prelude.show x))

-- Some scans driven as folds (the postscan-only ones, e.g. 'rollingMap',
-- 'uniq') are partial: their 'extract' on the initial state errors ("Empty
-- stream"). The filter law can reduce the input to empty, so we compare the two
-- sides exception-tolerantly: both sides throwing means the law still holds
-- (they fail identically); only one side throwing is a genuine violation.
-- | Run an action, returning 'Left' if it (or its result) throws. Fixes the
-- exception type so the law below needs no inline type annotations.
tryEval :: IO c -> IO (Either SomeException c)
tryEval act = try (act >>= evaluate)

filterLaw :: (Eq b, Show b, Show a) => Op IO a b -> [a] -> Expectation
filterLaw cons xs = do
    lhs <- tryEval
        (Stream.fold (F.filter filterLawPred cons) (Stream.fromList xs))
    rhs <- tryEval
        (Stream.fold cons (Stream.fromList (Prelude.filter filterLawPred xs)))
    case (lhs, rhs) of
        (Right l, Right r) -> l `shouldBe` r
        (Left _, Left _) -> return ()
        _ ->
            expectationFailure
                $ "filter law violated (one side failed): "
                    ++ show lhs ++ " vs " ++ show rhs

-- | Epsilon-equality counterpart of 'check' for Fractional results whose
-- floating-point output is only approximately equal to the reference (e.g.
-- 'mean'). The fold result must be within 1e-4 of the last expected prescan
-- value.
checkApprox ::
    (Ord b, Fractional b, Show b) => Op IO a b -> [a] -> [b] -> Expectation
checkApprox cons xs expected = do
    res <- Stream.fold cons (Stream.fromList xs)
    res `shouldSatisfy` \r -> abs (r - Prelude.last expected) < 1e-4

-- | Postscan-only counterpart of 'check' (for combinators whose scanl initial
-- is undefined). The fold result equals the last postscanl output.
checkPostscanl :: (Eq b, Show b, Show a) => Op IO a b -> [a] -> [b] -> Expectation
checkPostscanl cons xs expected = do
    Stream.fold cons (Stream.fromList xs) `shouldReturn` Prelude.last expected
    filterLaw cons xs

#include "Streamly/Test/Data/Scanl/CommonType.hs"

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

-- | A scanner that emits its input only when it is even, returning 'Continue'
-- (no output) for odd inputs. Per the 'Scanl' contract, the driver must not
-- call @extract@ on a 'Continue' step (extract is effectful and reserved for
-- output steps) and must not feed any value to the collector. We track every
-- @extract@ call to assert it happens only on emitting steps.
--
-- This catches a bug where @postscanl@ extracted and emitted on 'Continue',
-- which both leaked filtered elements and called @extract@ spuriously.
postscanlFilter :: Expectation
postscanlFilter = do
    ref <- newIORef []
    let scanner = Scanl.Scanl step initial extract final

        initial = return (Fold.Partial (0 :: Int))
        step s a =
            return $ if even a then Scanl.Partial a else Scanl.Continue s
        extract s = modifyIORef' ref (s :) >> return s
        final = return

    result <-
        Stream.fold
            (Fold.postscanl scanner Fold.toList)
            (Stream.fromList [1 .. 6 :: Int])
    extractCalls <- Prelude.reverse <$> readIORef ref

    -- Only the even inputs are emitted to the collector.
    result `shouldBe` [2, 4, 6]
    -- 'extract' is called only on the emitting (even) steps, never on 'Continue'.
    extractCalls `shouldBe` [2, 4, 6]

-- | The Continue filter law for Fold compositions: wrapping the scanner in
-- 'Scanl.filter' (which emits Continue for rejected inputs) must give the same
-- final fold result as running the fold on the pre-filtered input:
--
--     fold (f (Scanl.filter p scanner) collector) xs
--         === fold (f scanner collector) (filter p xs)
--
-- Parameterised by a context 'ctx' that combines a scanner with a collector.
foldFilterLaw ::
       Eq c
    => (Scanl.Scanl IO Int Int -> F.Fold IO Int c)
    -> [Int]
    -> Property
foldFilterLaw ctx xs = monadicIO $ do
    v1 <- run $ Stream.fold (ctx (Scanl.filter even Scanl.sum)) (Stream.fromList xs)
    v2 <- run $ Stream.fold (ctx Scanl.sum) (Stream.fromList (Prelude.filter even xs))
    assert (v1 == v2)

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

last :: [String] -> Expectation
last ls = Stream.fold Fold.last (Stream.fromList ls) `shouldReturn` safeLast ls

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

-- When the scanner is Done at the initial step it consumes no input, so with
-- postscanl the collector sees nothing, whereas with scanl it sees the default
-- (initial) value.
postscanlDoneAtInit :: Expectation
postscanlDoneAtInit =
    Stream.fold
        (Fold.postscanl (Scanl.take 0 Scanl.sum) Fold.toList)
        (Stream.fromList [1,2,3 :: Int])
    `shouldReturn` []

scanlDoneAtInit :: Expectation
scanlDoneAtInit =
    Stream.fold
        (Fold.scanl (Scanl.take 0 Scanl.sum) Fold.toList)
        (Stream.fromList [1,2,3 :: Int])
    `shouldReturn` [0]

postscanlMaybeDoneAtInit :: Expectation
postscanlMaybeDoneAtInit =
    Stream.fold
        (Fold.postscanlMaybe (fmap Just (Scanl.take 0 Scanl.sum)) Fold.toList)
        (Stream.fromList [1,2,3 :: Int])
    `shouldReturn` ([] :: [Int])

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

scanMaybe :: Expectation
scanMaybe =
    Stream.fold
        (Fold.scanMaybe
            (Fold.filtering even)
            Fold.toList)
        (Stream.fromList [1,2,3,4,5,6 :: Int])
    `shouldReturn` [2,4,6]

-------------------------------------------------------------------------------
-- Trimming
-------------------------------------------------------------------------------

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
        -- Tests shared with the Scanl suite (see Scanl/CommonType.hs)
        describe "common" commonTypeSpec

        -- Even though the following operations are common with scans, we have
        -- some additional tests which are not written in a way which can be
        -- shared with scans.
        prop "takeEndBy_" takeEndBy_
        it "takeEndBy" takeEndBy
        prop "takeEndByOrMax" takeEndByOrMax

        -- Before adding any tests here consider if it can be added to the
        -- common tests above.
        prop "foldl'" foldl'
        prop "foldlM'" foldlM'
        it "foldl1'" foldl1'
        it "foldl1M'" foldl1M'
        prop "foldr'" foldr'
        prop "foldrM'" foldrM'
        it "fromPure" fromPure
        it "fromEffect" fromEffect
        prop "fromScanl" fromScanl
        it "postscanl filter" postscanlFilter
        prop "filter law: Fold.postscanl" $ foldFilterLaw (\h -> Fold.postscanl h Fold.toList)
        prop "filter law: Fold.scanl" $ foldFilterLaw (\h -> Fold.scanl h Fold.toList)
        prop "filter law: Fold.scanlMany" $ foldFilterLaw (\h -> Fold.scanlMany h Fold.toList)
        prop "toStreamK" toStreamK
        prop "toStreamKRev" toStreamKRev
        prop "last" last
        prop "scan" scan
        prop "postscan" postscan
        prop "postscanl" postscanl
        prop "scanl" scanlFold
        prop "scanlMany" scanlMany
        it "postscanlMaybe" postscanlMaybe
        it "postscanl done-at-init" postscanlDoneAtInit
        it "postscanlMaybe done-at-init" postscanlMaybeDoneAtInit
        it "scanl done-at-init" scanlDoneAtInit
        it "scanMaybe" scanMaybe


        it "foldMaybes" foldMaybes
        it "foldEithers" foldEithers
        it "ifThen" ifThen
        it "split_" split_
        prop "many" many
        it "manyPost" manyPost
        it "groupsOf" groupsOf
        it "concatMap" concatMap
        it "duplicate" duplicate
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
        prop "foldBreak" foldBreak
