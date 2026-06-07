-- |
-- Module      : Streamly.Test.Data.Stream.Nesting
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Nesting (main) where

import Data.List (sort)
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Hspec as H

-------------------------------------------------------------------------------
-- Selective Appends
-------------------------------------------------------------------------------

testAppendUnfoldLastNonEmpty :: Expectation
testAppendUnfoldLastNonEmpty =
    Stream.toList
        (Stream.appendUnfoldLast trailer (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 3, 30, 300]

    where

    trailer = Unfold.lmap (maybe [-1] (\x -> [x * 10, x * 100])) Unfold.fromList

testAppendUnfoldLastEmpty :: Expectation
testAppendUnfoldLastEmpty =
    Stream.toList
        (Stream.appendUnfoldLast trailer (Stream.fromList ([] :: [Int])))
        `shouldReturn` [-1]

    where

    trailer = Unfold.lmap (maybe [-1] (\x -> [x * 10, x * 100])) Unfold.fromList

testAppendMapLastNonEmpty :: Expectation
testAppendMapLastNonEmpty =
    Stream.toList
        (Stream.appendMapLast trailer (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 3, 30, 300]

    where

    trailer =
        maybe (Stream.fromList [-1]) (\x -> Stream.fromList [x * 10, x * 100])

testAppendMapLastEmpty :: Expectation
testAppendMapLastEmpty =
    Stream.toList
        (Stream.appendMapLast trailer (Stream.fromList ([] :: [Int])))
        `shouldReturn` [-1]

    where

    trailer =
        maybe (Stream.fromList [-1]) (\x -> Stream.fromList [x * 10, x * 100])

-------------------------------------------------------------------------------
-- Selective Concat/Unfold
-------------------------------------------------------------------------------

testUnfoldLastNonEmpty :: Expectation
testUnfoldLastNonEmpty =
    Stream.toList
        (Stream.unfoldLast trailer (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 30, 300]

    where

    trailer = Unfold.lmap (foldMap (\x -> [x * 10, x * 100])) Unfold.fromList

testUnfoldLastEmpty :: Expectation
testUnfoldLastEmpty =
    Stream.toList
        (Stream.unfoldLast trailer (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

    where

    trailer = Unfold.lmap (foldMap (\x -> [x * 10, x * 100])) Unfold.fromList

testConcatMapLastNonEmpty :: Expectation
testConcatMapLastNonEmpty =
    Stream.toList
        (Stream.concatMapLast trailer (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [1, 2, 30, 300]

    where

    trailer =
        maybe (Stream.fromList []) (\x -> Stream.fromList [x * 10, x * 100])

testConcatMapLastEmpty :: Expectation
testConcatMapLastEmpty =
    Stream.toList
        (Stream.concatMapLast trailer (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

    where

    trailer =
        maybe (Stream.fromList []) (\x -> Stream.fromList [x * 10, x * 100])

testAppendIfEmptyNonEmpty :: Expectation
testAppendIfEmptyNonEmpty =
    Stream.toList
        (Stream.appendIfEmpty
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [3, 4]))
        `shouldReturn` [1, 2]

testAppendIfEmptyEmpty :: Expectation
testAppendIfEmptyEmpty =
    Stream.toList
        (Stream.appendIfEmpty
            (Stream.fromList ([] :: [Int]))
            (Stream.fromList [3, 4]))
        `shouldReturn` [3, 4]

testUnfoldFirstNonEmpty :: Expectation
testUnfoldFirstNonEmpty =
    Stream.toList
        (Stream.unfoldFirst header (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [10, 100, 2, 3]

    where

    header = Unfold.lmap (foldMap (\x -> [x * 10, x * 100])) Unfold.fromList

testUnfoldFirstEmpty :: Expectation
testUnfoldFirstEmpty =
    Stream.toList
        (Stream.unfoldFirst header (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

    where

    header = Unfold.lmap (foldMap (\x -> [x * 10, x * 100])) Unfold.fromList

testConcatMapFirstNonEmpty :: Expectation
testConcatMapFirstNonEmpty =
    Stream.toList
        (Stream.concatMapFirst header (Stream.fromList [1, 2, 3 :: Int]))
        `shouldReturn` [10, 100, 2, 3]

    where

    header =
        maybe (Stream.fromList []) (\x -> Stream.fromList [x * 10, x * 100])

testConcatMapFirstEmpty :: Expectation
testConcatMapFirstEmpty =
    Stream.toList
        (Stream.concatMapFirst header (Stream.fromList ([] :: [Int])))
        `shouldReturn` []

    where

    header =
        maybe (Stream.fromList []) (\x -> Stream.fromList [x * 10, x * 100])

-------------------------------------------------------------------------------
-- Interleaving
-------------------------------------------------------------------------------

testInterleave :: Expectation
testInterleave =
    Stream.toList
        (Stream.interleave
            (Stream.fromList [1, 3, 5 :: Int])
            (Stream.fromList [2, 4, 6]))
        `shouldReturn` [1, 2, 3, 4, 5, 6]

-- interleaveEndBy' s1 s2: elements of s2 ended by elements of s1; stops when
-- either stream ends.
testInterleaveEndBy' :: Expectation
testInterleaveEndBy' =
    Stream.toList
        (Stream.interleaveEndBy'
            (Stream.fromList [1, 2, 3 :: Int])
            (Stream.fromList [10, 20, 30]))
        `shouldReturn` [10, 1, 20, 2, 30, 3]

testInterleaveEndBy'StopsEarly :: Expectation
testInterleaveEndBy'StopsEarly =
    Stream.toList
        (Stream.interleaveEndBy'
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [10, 20, 30]))
        `shouldReturn` [10, 1, 20, 2]

-- interleaveSepBy' s1 s2: s1 infixed between s2 elements; stops when s1
-- is exhausted (needs n-1 separators for n s2 elements).
testInterleaveSepBy' :: Expectation
testInterleaveSepBy' =
    Stream.toList
        (Stream.interleaveSepBy'
            (Stream.fromList [0, 0 :: Int])
            (Stream.fromList [10, 20, 30]))
        `shouldReturn` [10, 0, 20, 0, 30]

testInterleaveSepBy'StopsEarly :: Expectation
testInterleaveSepBy'StopsEarly =
    Stream.toList
        (Stream.interleaveSepBy'
            (Stream.fromList [0 :: Int])
            (Stream.fromList [10, 20, 30]))
        `shouldReturn` [10, 0, 20]

-- interleaveEndBy s1 s2: like interleaveEndBy' but stops only when s2 ends;
-- shortfall of s1 is ignored.
testInterleaveEndBy :: Expectation
testInterleaveEndBy =
    Stream.toList
        (Stream.interleaveEndBy
            (Stream.fromList [1, 2, 3 :: Int])
            (Stream.fromList [10, 20, 30]))
        `shouldReturn` [10, 1, 20, 2, 30, 3]

testInterleaveEndByShortSep :: Expectation
testInterleaveEndByShortSep =
    Stream.toList
        (Stream.interleaveEndBy
            (Stream.fromList [1 :: Int])
            (Stream.fromList [10, 20, 30]))
        `shouldReturn` [10, 1, 20, 30]

-- interleaveSepBy s1 s2: like interleaveSepBy' but stops only when s2 ends;
-- shortfall of s1 is ignored.
testInterleaveSepBy :: Expectation
testInterleaveSepBy =
    Stream.toList
        (Stream.interleaveSepBy
            (Stream.fromList [0, 0 :: Int])
            (Stream.fromList [10, 20, 30]))
        `shouldReturn` [10, 0, 20, 0, 30]

testInterleaveSepByShortSep :: Expectation
testInterleaveSepByShortSep =
    Stream.toList
        (Stream.interleaveSepBy
            (Stream.fromList [0 :: Int])
            (Stream.fromList [10, 20, 30]))
        `shouldReturn` [10, 0, 20, 30]

-- roundRobin: fair round-robin scheduling; continues when one stream ends.
testRoundRobin :: Expectation
testRoundRobin =
    Stream.toList
        (Stream.roundRobin
            (Stream.fromList [1, 2, 3 :: Int])
            (Stream.fromList [10, 20, 30]))
        `shouldReturn` [1, 10, 2, 20, 3, 30]

testRoundRobinUnequal :: Expectation
testRoundRobinUnequal =
    Stream.toList
        (Stream.roundRobin
            (Stream.fromList [1, 2 :: Int])
            (Stream.fromList [10, 20, 30]))
        `shouldReturn` [1, 10, 2, 20, 30]

-------------------------------------------------------------------------------
-- Merging
-------------------------------------------------------------------------------

testMergeBy :: Expectation
testMergeBy =
    Stream.toList
        (Stream.mergeBy compare
            (Stream.fromList [1, 3, 5 :: Int])
            (Stream.fromList [2, 4, 6, 8]))
        `shouldReturn` [1, 2, 3, 4, 5, 6, 8]

testMergeByM :: Expectation
testMergeByM =
    Stream.toList
        (Stream.mergeByM (\a b -> return (compare a b))
            (Stream.fromList [1, 3, 5 :: Int])
            (Stream.fromList [2, 4, 6, 8]))
        `shouldReturn` [1, 2, 3, 4, 5, 6, 8]

-------------------------------------------------------------------------------
-- BFS/Fair Unfold
-------------------------------------------------------------------------------

nestedLists :: Stream.Stream IO [Int]
nestedLists = Stream.fromList [[1, 4, 7], [2, 5, 8], [3, 6, 9]]

testBfsUnfoldEach :: Expectation
testBfsUnfoldEach = do
    xs <- Stream.toList $ Stream.bfsUnfoldEach Unfold.fromList nestedLists
    xs `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]

testAltBfsUnfoldEach :: Expectation
testAltBfsUnfoldEach = do
    result <- fmap sort $ Stream.toList $
        Stream.altBfsUnfoldEach
            (Unfold.lmap (\n -> (1, n)) Unfold.enumerateFromToIntegral)
            (Stream.fromList [2, 3 :: Int])
    result `shouldBe` sort [1, 2, 1, 2, 3]

testFairUnfoldEach :: Expectation
testFairUnfoldEach = do
    xs <- fmap sort $ Stream.toList $
            Stream.fairUnfoldEach Unfold.fromList nestedLists
    xs `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]

-------------------------------------------------------------------------------
-- unfoldEachSepBy / unfoldEachEndBy
-------------------------------------------------------------------------------

-- unfoldEachSepBy x u s: unfold each element of s, insert x between groups.
testUnfoldEachSepBy :: Expectation
testUnfoldEachSepBy =
    Stream.toList
        (Stream.unfoldEachSepBy 0 Unfold.fromList
            (Stream.fromList [[1, 2 :: Int], [3, 4]]))
        `shouldReturn` [1, 2, 0, 3, 4]

testUnfoldEachSepByM :: Expectation
testUnfoldEachSepByM =
    Stream.toList
        (Stream.unfoldEachSepByM (return 0) Unfold.fromList
            (Stream.fromList [[1, 2 :: Int], [3, 4]]))
        `shouldReturn` [1, 2, 0, 3, 4]

-- unfoldEachEndBy x u s: unfold each element of s, append x after each group.
testUnfoldEachEndBy :: Expectation
testUnfoldEachEndBy =
    Stream.toList
        (Stream.unfoldEachEndBy 0 Unfold.fromList
            (Stream.fromList [[1, 2 :: Int], [3, 4]]))
        `shouldReturn` [1, 2, 0, 3, 4, 0]

testUnfoldEachEndByM :: Expectation
testUnfoldEachEndByM =
    Stream.toList
        (Stream.unfoldEachEndByM (return 0) Unfold.fromList
            (Stream.fromList [[1, 2 :: Int], [3, 4]]))
        `shouldReturn` [1, 2, 0, 3, 4, 0]

-- unfoldEachSepBySeq: intersperse seed element between list elements then unfoldEach.
testUnfoldEachSepBySeq :: Expectation
testUnfoldEachSepBySeq =
    Stream.toList
        (Stream.unfoldEachSepBySeq [0 :: Int] Unfold.fromList
            (Stream.fromList [[1, 2], [3, 4]]))
        `shouldReturn` [1, 2, 0, 3, 4]

-- unfoldEachEndBySeq: intersperseEndBy seed element then unfoldEach.
testUnfoldEachEndBySeq :: Expectation
testUnfoldEachEndBySeq =
    Stream.toList
        (Stream.unfoldEachEndBySeq [0 :: Int] Unfold.fromList
            (Stream.fromList [[1, 2], [3, 4]]))
        `shouldReturn` [1, 2, 0, 3, 4, 0]

-------------------------------------------------------------------------------
-- Intercalate
-------------------------------------------------------------------------------

-- intercalateSepBy u1 s1 u2 s2: interleave unfolded s1 (separators) between
-- unfolded s2 elements.
testIntercalateSepBy :: Expectation
testIntercalateSepBy =
    Stream.toList
        (Stream.intercalateSepBy
            Unfold.fromList (Stream.repeat [0 :: Int])
            Unfold.fromList (Stream.fromList [[1, 2], [3, 4]]))
        `shouldReturn` [1, 2, 0, 3, 4]

-- intercalateEndBy u1 s1 u2 s2: append unfolded s1 (endings) after each
-- unfolded s2 element.
testIntercalateEndBy :: Expectation
testIntercalateEndBy =
    Stream.toList
        (Stream.intercalateEndBy
            Unfold.fromList (Stream.repeat [0 :: Int])
            Unfold.fromList (Stream.fromList [[1, 2], [3, 4]]))
        `shouldReturn` [1, 2, 0, 3, 4, 0]

-------------------------------------------------------------------------------
-- Fair ConcatMap
-------------------------------------------------------------------------------

testFairConcatMapM :: Expectation
testFairConcatMapM = do
    xs <- fmap sort $ Stream.toList $
            Stream.fairConcatMapM
                (\x -> return (Stream.fromList [x * 10 :: Int]))
                (Stream.fromList [1, 2, 3])
    xs `shouldBe` [10, 20, 30]

testFairConcatMap :: Expectation
testFairConcatMap = do
    xs <- fmap sort $ Stream.toList $
            Stream.fairConcatMap
                (\x -> Stream.fromList [x * 10 :: Int])
                (Stream.fromList [1, 2, 3])
    xs `shouldBe` [10, 20, 30]

testFairConcatForM :: Expectation
testFairConcatForM = do
    xs <- fmap sort $ Stream.toList $
            Stream.fairConcatForM
                (Stream.fromList [1, 2, 3 :: Int])
                (\x -> return (Stream.fromList [x * 10]))
    xs `shouldBe` [10, 20, 30]

testFairConcatFor :: Expectation
testFairConcatFor = do
    xs <- fmap sort $ Stream.toList $
            Stream.fairConcatFor
                (Stream.fromList [1, 2, 3 :: Int])
                (\x -> Stream.fromList [x * 10])
    xs `shouldBe` [10, 20, 30]

-------------------------------------------------------------------------------
-- Unfold Sched
-------------------------------------------------------------------------------

testUnfoldSched :: Expectation
testUnfoldSched = do
    xs <- Stream.toList $
            Stream.unfoldSched Unfold.fromList nestedLists
    xs `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]

testFairUnfoldSched :: Expectation
testFairUnfoldSched = do
    xs <- fmap sort $ Stream.toList $
            Stream.fairUnfoldSched Unfold.fromList nestedLists
    xs `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]

-------------------------------------------------------------------------------
-- SchedMap / SchedFor
-------------------------------------------------------------------------------

testSchedMapM :: Expectation
testSchedMapM = do
    xs <- Stream.toList $
            Stream.schedMapM
                (\x -> return (Stream.fromList [x, x * 10 :: Int]))
                (Stream.fromList [1, 2, 3])
    xs `shouldBe` [1, 2, 3, 10, 20, 30]

testSchedMap :: Expectation
testSchedMap = do
    xs <- Stream.toList $
            Stream.schedMap
                (\x -> Stream.fromList [x, x * 10 :: Int])
                (Stream.fromList [1, 2, 3])
    xs `shouldBe` [1, 2, 3, 10, 20, 30]

testSchedForM :: Expectation
testSchedForM = do
    xs <- Stream.toList $
            Stream.schedForM
                (Stream.fromList [1, 2, 3 :: Int])
                (\x -> return (Stream.fromList [x, x * 10]))
    xs `shouldBe` [1, 2, 3, 10, 20, 30]

testSchedFor :: Expectation
testSchedFor = do
    xs <- Stream.toList $
            Stream.schedFor
                (Stream.fromList [1, 2, 3 :: Int])
                (\x -> Stream.fromList [x, x * 10])
    xs `shouldBe` [1, 2, 3, 10, 20, 30]

-------------------------------------------------------------------------------
-- FairSchedMap / FairSchedFor
-------------------------------------------------------------------------------

testFairSchedMapM :: Expectation
testFairSchedMapM = do
    xs <- fmap sort $ Stream.toList $
            Stream.fairSchedMapM
                (\x -> return (Stream.fromList [x * 10 :: Int]))
                (Stream.fromList [1, 2, 3])
    xs `shouldBe` [10, 20, 30]

testFairSchedMap :: Expectation
testFairSchedMap = do
    xs <- fmap sort $ Stream.toList $
            Stream.fairSchedMap
                (\x -> Stream.fromList [x * 10 :: Int])
                (Stream.fromList [1, 2, 3])
    xs `shouldBe` [10, 20, 30]

testFairSchedForM :: Expectation
testFairSchedForM = do
    xs <- fmap sort $ Stream.toList $
            Stream.fairSchedForM
                (Stream.fromList [1, 2, 3 :: Int])
                (\x -> return (Stream.fromList [x * 10]))
    xs `shouldBe` [10, 20, 30]

testFairSchedFor :: Expectation
testFairSchedFor = do
    xs <- fmap sort $ Stream.toList $
            Stream.fairSchedFor
                (Stream.fromList [1, 2, 3 :: Int])
                (\x -> Stream.fromList [x * 10])
    xs `shouldBe` [10, 20, 30]

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Stream.Nesting"

main :: IO ()
main = hspec
  $ H.parallel
  $ describe moduleName $ do

    -- Selective Appends
    describe "appendIfEmpty" $ do
        it "non-empty" testAppendIfEmptyNonEmpty
        it "empty" testAppendIfEmptyEmpty

    describe "appendUnfoldLast" $ do
        it "non-empty" testAppendUnfoldLastNonEmpty
        it "empty" testAppendUnfoldLastEmpty

    describe "appendMapLast" $ do
        it "non-empty" testAppendMapLastNonEmpty
        it "empty" testAppendMapLastEmpty

    -- Selective Concat/Unfold
    describe "unfoldLast" $ do
        it "non-empty" testUnfoldLastNonEmpty
        it "empty" testUnfoldLastEmpty

    describe "concatMapLast" $ do
        it "non-empty" testConcatMapLastNonEmpty
        it "empty" testConcatMapLastEmpty

    describe "unfoldFirst" $ do
        it "non-empty" testUnfoldFirstNonEmpty
        it "empty" testUnfoldFirstEmpty

    describe "concatMapFirst" $ do
        it "non-empty" testConcatMapFirstNonEmpty
        it "empty" testConcatMapFirstEmpty

    -- Interleaving
    it "interleave" testInterleave

    describe "interleaveEndBy'" $ do
        it "equal length" testInterleaveEndBy'
        it "stops when sep exhausted" testInterleaveEndBy'StopsEarly

    describe "interleaveSepBy'" $ do
        it "equal sep count" testInterleaveSepBy'
        it "stops when sep exhausted" testInterleaveSepBy'StopsEarly

    describe "interleaveEndBy" $ do
        it "equal length" testInterleaveEndBy
        it "short sep continues" testInterleaveEndByShortSep

    describe "interleaveSepBy" $ do
        it "equal sep count" testInterleaveSepBy
        it "short sep continues" testInterleaveSepByShortSep

    describe "roundRobin" $ do
        it "equal length" testRoundRobin
        it "unequal length continues" testRoundRobinUnequal

    -- Merging
    describe "mergeBy" $ do
        it "merges sorted streams" testMergeBy

    describe "mergeByM" $ do
        it "merges sorted streams" testMergeByM

    -- BFS/Fair Unfold
    describe "bfsUnfoldEach" $ do
        it "BFS traversal" testBfsUnfoldEach

    it "altBfsUnfoldEach" testAltBfsUnfoldEach

    describe "fairUnfoldEach" $ do
        it "fair traversal" testFairUnfoldEach

    -- unfoldEach sep/end variants
    describe "unfoldEachSepBy" $ do
        it "inserts sep between groups" testUnfoldEachSepBy

    describe "unfoldEachSepByM" $ do
        it "inserts monadic sep between groups" testUnfoldEachSepByM

    describe "unfoldEachEndBy" $ do
        it "appends end after each group" testUnfoldEachEndBy

    describe "unfoldEachEndByM" $ do
        it "appends monadic end after each group" testUnfoldEachEndByM

    describe "unfoldEachSepBySeq" $ do
        it "intersperse seq then unfold" testUnfoldEachSepBySeq

    describe "unfoldEachEndBySeq" $ do
        it "intersperseEnd seq then unfold" testUnfoldEachEndBySeq

    -- Intercalate
    describe "intercalateSepBy" $ do
        it "interleaves sep streams" testIntercalateSepBy

    describe "intercalateEndBy" $ do
        it "appends end streams" testIntercalateEndBy

    -- Fair ConcatMap
    describe "fairConcatMapM" $ do
        it "fair interleave" testFairConcatMapM

    describe "fairConcatMap" $ do
        it "fair interleave" testFairConcatMap

    describe "fairConcatForM" $ do
        it "fair interleave" testFairConcatForM

    describe "fairConcatFor" $ do
        it "fair interleave" testFairConcatFor

    -- Unfold Sched
    describe "unfoldSched" $ do
        it "scheduled traversal" testUnfoldSched

    describe "fairUnfoldSched" $ do
        it "fair scheduled traversal" testFairUnfoldSched

    -- SchedMap / SchedFor
    describe "schedMapM" $ do
        it "scheduled map" testSchedMapM

    describe "schedMap" $ do
        it "scheduled map" testSchedMap

    describe "schedForM" $ do
        it "scheduled for" testSchedForM

    describe "schedFor" $ do
        it "scheduled for" testSchedFor

    -- FairSchedMap / FairSchedFor
    describe "fairSchedMapM" $ do
        it "fair scheduled map" testFairSchedMapM

    describe "fairSchedMap" $ do
        it "fair scheduled map" testFairSchedMap

    describe "fairSchedForM" $ do
        it "fair scheduled for" testFairSchedForM

    describe "fairSchedFor" $ do
        it "fair scheduled for" testFairSchedFor
