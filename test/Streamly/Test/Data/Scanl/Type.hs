{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
-- |
-- Module      : Streamly.Test.Data.Scanl.Type
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Scanl.Type
    ( main, check, checkApprox, checkPostscanl, checkNoLaw
    ) where

import Data.Functor.Identity (Identity, runIdentity)
import Data.IORef (newIORef, readIORef)
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Refold.Type as Refold
import qualified Streamly.Internal.Data.Scanl as F
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude hiding (const, last, length, take, filter, scanl, foldl', concatMap)
import qualified Prelude

import Streamly.Test.Common (chooseInt)
import Streamly.Test.Data.Scanl.Common (evenScanl, filterLawScanModifier, filterLawScan)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, forAll)

-------------------------------------------------------------------------------
-- Scanl and Fold test sharing mechanism
-------------------------------------------------------------------------------

-- Shared tests are written in CPP files that are included in both Fold tests
-- and Scanl tests. For example, "Streamly/Test/Data/Scanl/CommonType.hs"
-- contains tests that are common to Scanl/Type.hs module and the Fold module.
--
-- The including module must bring the following into scope BEFORE the #include:
--   * qualifier  F        -- Streamly.Internal.Data.{Fold,Scanl}
--   * qualifier  Stream   -- Streamly.Internal.Data.Stream
--   * type Op             -- F.Fold or F.Scanl
--   * check :: (Eq b, Show b) => Op IO a b -> [a] -> [b] -> Expectation
--   * checkApprox :: (Ord b, Fractional b, Show b)
--                       => Op IO a b -> [a] -> [b] -> Expectation
--     (epsilon-equality counterpart of 'check' for floating-point results)
--   plus the usual hspec / QuickCheck imports, Data.Functor.Identity, and
--   Streamly.Test.Common (chooseInt).
--
-- In every shared test the @expected@ value is the *inclusive prescan list*
-- (Prelude.scanl f z), i.e. exactly what Stream.scanl emits: the leading
-- initial value followed by one output per input (filtering scans emit no
-- output for filtered elements, so their @expected@ list is built from the
-- elements that pass; terminating scans truncate at, and including, the
-- terminating step). From this list:
--   * the Fold includer checks the final value  (Prelude.last expected)
--   * the Scanl includer checks the full Stream.scanl output (== expected) and
--     the Stream.postscanl output (== drop 1 expected, the initial omitted)
--
-- Combinators are grouped by their *Scanl* source submodule (these Common*.hs
-- fragments live under Scanl/, so Scanl is the authoritative source for
-- placement).

-- | A Scanl is exercised by scanning a stream and verifying the resulting
-- stream of intermediate outputs. The shared @expected@ value is the inclusive
-- prescan list (what 'Stream.scanl' emits); 'Stream.postscanl' is the same with
-- the leading initial value dropped.
type Op = F.Scanl

check :: (Eq b, Show b, Show a) => Op IO a b -> [a] -> [b] -> Expectation
check cons xs expected = do
    Stream.toList (Stream.scanl cons (Stream.fromList xs))
        `shouldReturn` expected
    Stream.toList (Stream.postscanl cons (Stream.fromList xs))
        `shouldReturn` drop 1 expected
    filterLaw cons xs

-- | Same as 'check' but does NOT apply the 'Continue' filter law. Use this only
-- for the few tests that pass bottom (e.g. 'error') input elements to verify
-- early termination -- the law would force those elements.
checkNoLaw :: (Eq b, Show b) => Op IO a b -> [a] -> [b] -> Expectation
checkNoLaw cons xs expected = do
    Stream.toList (Stream.scanl cons (Stream.fromList xs))
        `shouldReturn` expected
    Stream.toList (Stream.postscanl cons (Stream.fromList xs))
        `shouldReturn` drop 1 expected

-- | The 'Continue' filter law (an independent, black-box oracle that needs no
-- knowledge of how the scan is implemented): wrapping a scan in 'Scanl.filter'
-- -- which emits 'Continue' for rejected inputs -- must produce the same output
-- as running the scan on the pre-filtered input:
--
--     scanl/postscanl (Scanl.filter p s) xs === scanl/postscanl s (filter p xs)
--
-- This holds for EVERY scan because both sides feed the scan the identical
-- accepted subsequence; it verifies that the driver suppresses output (and does
-- not extract) on every 'Continue' step. 'filterLawPred' rejects a portion of
-- the inputs so the law actually exercises 'Continue'. Folded into 'check' and
-- 'checkPostscanl' so it applies to every shared and Scanl-specific test.
filterLawPred :: Show a => a -> Bool
filterLawPred x = even (Prelude.length (Prelude.show x))

filterLaw :: (Eq b, Show b, Show a) => Op IO a b -> [a] -> Expectation
filterLaw cons xs = do
    -- We run the filter twice, once with the original predicate and again with
    -- inverted predicate, this is to ensure that in one of them we filter the
    -- first element and in one keep it, and same for the last element. These
    -- two are important cases to cover.
    mapM_ runScanl [filterLawPred, not . filterLawPred]
    filterLawPost cons xs
  where
    runScanl p = do
        lhs <- Stream.toList
            (Stream.scanl (F.filter p cons) (Stream.fromList xs))
        rhs <- Stream.toList
            (Stream.scanl cons (Stream.fromList (Prelude.filter p xs)))
        lhs `shouldBe` rhs

filterLawPost :: (Eq b, Show b, Show a) => Op IO a b -> [a] -> Expectation
filterLawPost cons xs = mapM_ run [filterLawPred, not . filterLawPred]
  where
    run p = do
        lhs <- Stream.toList
            (Stream.postscanl (F.filter p cons) (Stream.fromList xs))
        rhs <- Stream.toList
            (Stream.postscanl cons (Stream.fromList (Prelude.filter p xs)))
        lhs `shouldBe` rhs

-- | Epsilon-equality counterpart of 'check' for Fractional results whose
-- floating-point output is only approximately equal to the reference (e.g.
-- 'mean'). Each emitted value must be within 1e-4 of the expected prescan value.
checkApprox ::
    (Ord b, Fractional b, Show b) => Op IO a b -> [a] -> [b] -> Expectation
checkApprox cons xs expected = do
    out <- Stream.toList (Stream.scanl cons (Stream.fromList xs))
    out `shouldSatisfy` approxEqList expected
    pout <- Stream.toList (Stream.postscanl cons (Stream.fromList xs))
    pout `shouldSatisfy` approxEqList (drop 1 expected)

    where

    approxEqList as bs =
        Prelude.length as == Prelude.length bs
            && and (zipWith (\a b -> abs (a - b) < 1e-4) as bs)

-- | For combinators that only support postscan (their scanl initial value is
-- undefined, e.g. rollingMap), so 'check' cannot be used. @expected@ is the
-- Stream.postscanl output (one value per input, no leading initial).
checkPostscanl :: (Eq b, Show b, Show a) => Op IO a b -> [a] -> [b] -> Expectation
checkPostscanl cons xs expected = do
    Stream.toList (Stream.postscanl cons (Stream.fromList xs))
        `shouldReturn` expected
    -- Only the postscanl half of the law: these scans have an undefined 'scanl'
    -- initial value.
    filterLawPost cons xs

#include "Streamly/Test/Data/Scanl/CommonType.hs"

-------------------------------------------------------------------------------
-- Scanl-only tests.
-------------------------------------------------------------------------------

scanlVsPostscanl :: Expectation
scanlVsPostscanl = do
    a <- Stream.toList (Stream.scanl F.sum (Stream.fromList [1, 2, 3 :: Int]))
    a `shouldBe` [0, 1, 3, 6]
    b <- Stream.toList (Stream.postscanl F.sum (Stream.fromList [1, 2, 3 :: Int]))
    b `shouldBe` [1, 3, 6]

-- 'Scanl.postscanl' composes two scans. Verify both the normal case and the
-- case where the inner scan is Done at the initial step (its value is dropped,
-- so the resulting scan is also Done at init and postscanl emits nothing).
postscanlCompose :: Expectation
postscanlCompose = do
    Stream.toList
        (Stream.postscanl (F.postscanl F.sum F.toList) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [[1], [1, 3], [1, 3, 6]]
    -- scanl additionally emits the initial value of the composed scan
    Stream.toList
        (Stream.scanl (F.postscanl F.sum F.toList) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [[], [1], [1, 3], [1, 3, 6]]
    -- done-at-init: postscanl emits nothing, scanl emits the default value
    Stream.toList
        (Stream.postscanl (F.postscanl (F.take 0 F.sum) F.toList) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` ([] :: [[Int]])
    Stream.toList
        (Stream.scanl (F.postscanl (F.take 0 F.sum) F.toList) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [[] :: [Int]]

postscanlMaybeCompose :: Expectation
postscanlMaybeCompose = do
    Stream.toList
        (Stream.postscanl (F.postscanlMaybe (F.filtering even) F.length) (Stream.fromList [1 .. 6 :: Int]))
        `shouldReturn` [1, 2, 3]
    Stream.toList
        (Stream.postscanl (F.postscanlMaybe (fmap Just (F.take 0 F.sum)) F.length) (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` ([] :: [Int])

-- postscanl must propagate Continue from the right scan (bug at line 1337 of
-- Scanl/Type.hs returns Partial instead, emitting spurious output).
--
-- postscanl sum (filter even sum) on [1..4]:
--   running sums fed to the right: 1, 3, 6, 10
--   1, 3 are odd  -> right returns Continue -> no output (BUG: emits 0)
--   6, 10 are even -> right returns Partial  -> output 6, then 16
postscanlRightFilter :: Expectation
postscanlRightFilter =
    Stream.toList
        (Stream.postscanl
            (F.postscanl (F.scanl' (+) 0) (F.filter even (F.scanl' (+) 0)))
            (Stream.fromList [1 .. 4 :: Int]))
        `shouldReturn` [6, 16]

-- postscanl must propagate Continue when left returns Continue and right
-- returns Partial (bug at line 1343 of Scanl/Type.hs returns Partial instead
-- of Continue; right is spuriously advanced with a stale left value).
--
-- postscanl (filter even sum) sum on [1..4]:
--   odd inputs  -> left returns Continue (stale extract = even-sum so far)
--                  right (sum) would return Partial if fed that stale value -- BUG
--   even inputs -> left returns Partial; feed new left value to right
--   Correct: only Partial when BOTH left and right return Partial
--     input 1 (odd,  left=Continue)  -> Continue; right NOT advanced
--     input 2 (even, left=Partial 2) -> right += 2 -> 2;  output 2
--     input 3 (odd,  left=Continue)  -> Continue; right NOT advanced
--     input 4 (even, left=Partial 6) -> right += 6 -> 8;  output 8
postscanlLeftFilter :: Expectation
postscanlLeftFilter =
    Stream.toList
        (Stream.postscanl
            (F.postscanl (F.filter even (F.scanl' (+) 0)) (F.scanl' (+) 0))
            (Stream.fromList [1 .. 4 :: Int]))
        `shouldReturn` [2, 8]

-- postscanl must propagate Continue when BOTH left and right return Continue
-- (bug at line 1344 of Scanl/Type.hs returns Partial instead).
--
-- postscanl (filter even sum) (filter odd sum) on [1..4]:
--   odd inputs   -> left returns Continue; extractL gives the even-running-sum (always even)
--   even inputs  -> left returns Partial;  extractL gives the new even-running-sum (always even)
--   right (filter odd) always sees an even value -> always returns Continue
--   so no input should ever produce output
postscanlBothFilter :: Expectation
postscanlBothFilter =
    Stream.toList
        (Stream.postscanl
            (F.postscanl (F.filter even (F.scanl' (+) 0)) (F.filter odd (F.scanl' (+) 0)))
            (Stream.fromList [1 .. 4 :: Int]))
        `shouldReturn` ([] :: [Int])

-------------------------------------------------------------------------------
-- Filter tests belonging to Scanl.Type
-------------------------------------------------------------------------------

teeWithFilter :: Expectation
teeWithFilter = do
    ref <- newIORef []
    out <-
        Stream.toList
            $ Stream.postscanl
                (F.teeWith (,) (evenScanl ref) (F.scanl' (\_ x -> x) 0))
                (Stream.fromList [1 .. 4 :: Int])
    calls <- Prelude.reverse <$> readIORef ref
    out `shouldBe` [(2, 2), (4, 4)]
    calls `shouldBe` [2, 4]

teeWithRightFilter :: Expectation
teeWithRightFilter = do
    ref <- newIORef []
    out <-
        Stream.toList
            $ Stream.postscanl
                (F.teeWith (,) (F.scanl' (\_ x -> x) 0) (evenScanl ref))
                (Stream.fromList [1 .. 4 :: Int])
    calls <- Prelude.reverse <$> readIORef ref
    out `shouldBe` [(2, 2), (4, 4)]
    calls `shouldBe` [2, 4]

takeFilter :: Expectation
takeFilter = do
    ref <- newIORef []
    out <-
        Stream.toList
            $ Stream.postscanl
                (F.take 100 (evenScanl ref))
                (Stream.fromList [1 .. 6 :: Int])
    calls <- Prelude.reverse <$> readIORef ref
    out `shouldBe` [2, 4, 6]
    calls `shouldBe` [2, 4, 6]

takeFilterCount :: Expectation
takeFilterCount = do
    ref <- newIORef []
    out <-
        Stream.toList
            $ Stream.postscanl
                (F.take 2 (evenScanl ref))
                (Stream.fromList [1 .. 6 :: Int])
    calls <- Prelude.reverse <$> readIORef ref
    out `shouldBe` [2, 4]
    calls `shouldBe` [2]

takeEndByFilter :: Expectation
takeEndByFilter = do
    ref <- newIORef []
    out <-
        Stream.toList
            $ Stream.postscanl
                (F.takeEndBy (== 3) (evenScanl ref))
                (Stream.fromList [1, 2, 3, 4 :: Int])
    calls <- Prelude.reverse <$> readIORef ref
    out `shouldBe` [2, 2]
    calls `shouldBe` [2]

-------------------------------------------------------------------------------
-- Constructors
-------------------------------------------------------------------------------

scanlS :: [Int] -> Expectation
scanlS ls = check (F.scanl' (+) 0) ls (Prelude.scanl (+) 0 ls)

scanlMS :: [Int] -> Expectation
scanlMS ls =
    check (F.scanlM' (\b a -> return (b + a)) (return 0)) ls
        (Prelude.scanl (+) 0 ls)

scanl1S :: Expectation
scanl1S = do
    check (F.scanl1' (+)) ([1, 2, 3] :: [Int]) [Nothing, Just 1, Just 3, Just 6]
    check (F.scanl1' (+)) ([] :: [Int]) [Nothing]

scanl1MS :: Expectation
scanl1MS =
    check (F.scanl1M' (\a b -> return (a + b))) ([1, 2, 3] :: [Int])
        [Nothing, Just 1, Just 3, Just 6]

-- A terminating scan: accumulate the running sum but stop (terminate) at, and
-- including, the input value 3.
scantStep :: Int -> Int -> F.Step Int Int
scantStep s a = if a == 3 then F.Done s else F.Partial (s + a)

scantS :: Expectation
scantS =
    check (F.scant' scantStep (Fold.Partial 0) id) ([1, 2, 3, 4] :: [Int])
        [0, 1, 3, 3]

scantMS :: Expectation
scantMS =
    check (F.scantM' (\s a -> return (scantStep s a)) (return (Fold.Partial 0)) return)
        ([1, 2, 3, 4] :: [Int]) [0, 1, 3, 3]

mkScanrS :: Expectation
mkScanrS =
    check (F.mkScanr (:) []) ([1, 2, 3] :: [Int]) [[], [1], [1, 2], [1, 2, 3]]

mkScanrMS :: Expectation
mkScanrMS =
    check (F.mkScanrM (\a xs -> return (a : xs)) (return [])) ([1, 2, 3] :: [Int])
        [[], [1], [1, 2], [1, 2, 3]]

constS :: Expectation
constS = check (F.const (7 :: Int)) ([1, 2, 3] :: [Int]) [7, 7, 7, 7]

constMS :: Expectation
constMS = check (F.constM (return (7 :: Int))) ([1, 2, 3] :: [Int]) [7, 7, 7, 7]

functionMS :: Expectation
functionMS =
    check
        (F.functionM (\x -> return (if even x then Just x else Nothing)))
        ([1, 2, 3, 4] :: [Int])
        [Nothing, Nothing, Just 2, Nothing, Just 4]

-- A 'Refold' that sums starting from the injected seed.
sumRefold :: Monad m => Refold.Refold m Int Int Int
sumRefold =
    Refold.Refold
        (\s a -> return (Fold.Partial (s + a)))
        (\c -> return (Fold.Partial c))
        return

fromRefoldS :: Expectation
fromRefoldS = check (F.fromRefold sumRefold 0) ([1, 2, 3] :: [Int]) [0, 1, 3, 6]

toStreamKS :: [Int] -> Expectation
toStreamKS ls =
    check (F.rmapM (Stream.toList . Stream.fromStreamK) F.toStreamK) ls
        (Prelude.scanl (\acc x -> acc ++ [x]) [] ls)

toStreamKRevS :: [Int] -> Expectation
toStreamKRevS ls =
    check (F.rmapM (Stream.toList . Stream.fromStreamK) F.toStreamKRev) ls
        (Prelude.scanl (flip (:)) [] ls)

-------------------------------------------------------------------------------
-- Deprecated constructors (aliases for the corresponding scanl*' functions)
-------------------------------------------------------------------------------

mkScanlS :: [Int] -> Expectation
mkScanlS ls = check (F.mkScanl (+) 0) ls (Prelude.scanl (+) 0 ls)

mkScanlMS :: [Int] -> Expectation
mkScanlMS ls =
    check (F.mkScanlM (\b a -> return (b + a)) (return 0)) ls
        (Prelude.scanl (+) 0 ls)

mkScanl1S :: Expectation
mkScanl1S =
    check (F.mkScanl1 (+)) ([1, 2, 3] :: [Int]) [Nothing, Just 1, Just 3, Just 6]

mkScanl1MS :: Expectation
mkScanl1MS =
    check (F.mkScanl1M (\a b -> return (a + b))) ([1, 2, 3] :: [Int])
        [Nothing, Just 1, Just 3, Just 6]

mkScantS :: Expectation
mkScantS =
    check (F.mkScant scantStep (Fold.Partial 0) id) ([1, 2, 3, 4] :: [Int])
        [0, 1, 3, 3]

mkScantMS :: Expectation
mkScantMS =
    check (F.mkScantM (\s a -> return (scantStep s a)) (return (Fold.Partial 0)) return)
        ([1, 2, 3, 4] :: [Int]) [0, 1, 3, 3]

moduleName :: String
moduleName = "Data.Scanl.Type"

main :: IO ()
main = hspec $
    describe moduleName $ do
        describe "common" commonTypeSpec

        -- Before adding any tests here consider if it can be added to the
        -- common tests above.
        it "scanl: emits initial value, postscanl omits it" scanlVsPostscanl
        prop "sum: filterLaw" $ filterLawScan F.sum

        -- postscanl
        it "postscanl: compose" postscanlCompose
        it "postscanl: filter right" postscanlRightFilter
        it "postscanl: filter left" postscanlLeftFilter
        it "postscanl: filter both" postscanlBothFilter
        prop "postscanl: filterLaw compose"
            $ filterLawScanModifier (\h -> F.postscanl h F.sum)

        -- postscanlMaybe
        it "postscanlMaybe: compose" postscanlMaybeCompose
        prop "postscanlMaybe: filterLaw"
            $ filterLawScan (F.postscanlMaybe (F.filtering even) F.length)

        -- teeWith
        it "teeWith: filter left" teeWithFilter
        it "teeWith: filter right" teeWithRightFilter
        prop "teeWith: filterLaw"
            $ filterLawScan (F.teeWith (,) F.sum F.sum)

        -- take
        it "take: filter" takeFilter
        it "take: filter count limit" takeFilterCount
        prop "take: filterLaw" $ filterLawScanModifier (F.take 3)

        -- takeEndBy
        it "takeEndBy: filter" takeEndByFilter

        -- constructors
        prop "scanl'" scanlS
        prop "scanlM'" scanlMS
        it "scanl1'" scanl1S
        it "scanl1M'" scanl1MS
        it "scant'" scantS
        it "scantM'" scantMS
        it "mkScanr" mkScanrS
        it "mkScanrM" mkScanrMS
        it "const" constS
        it "constM" constMS
        it "functionM" functionMS
        it "fromRefold" fromRefoldS
        prop "toStreamK" toStreamKS
        prop "toStreamKRev" toStreamKRevS

        -- deprecated constructors
        prop "mkScanl" mkScanlS
        prop "mkScanlM" mkScanlMS
        it "mkScanl1" mkScanl1S
        it "mkScanl1M" mkScanl1MS
        it "mkScant" mkScantS
        it "mkScantM" mkScantMS
