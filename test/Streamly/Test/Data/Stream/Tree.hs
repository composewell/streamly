-- |
-- Module      : Streamly.Test.Data.Stream.Tree
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream.Tree (main) where

import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Unfold (Unfold)

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Unfold as Unfold

import Test.Hspec as H

-------------------------------------------------------------------------------
-- Unfold Iterate / Concat Iterate (DFS/BFS tree traversal)
--
-- Tree used: 1->[2,3], 2->[4,5], 3->[6,7], leaves have no children.
-- DFS order:               1, 2, 4, 5, 3, 6, 7
-- BFS order:               1, 2, 3, 4, 5, 6, 7
-- wideDfsRev order:        1, 2, 3, 6, 7, 4, 5
-- bfsRev order:            1, 2, 3, 6, 7, 4, 5
-------------------------------------------------------------------------------

treeChildren :: Int -> [Int]
treeChildren n
    | n == 1    = [2, 3]
    | n == 2    = [4, 5]
    | n == 3    = [6, 7]
    | otherwise = []

treeSeed :: Int -> Maybe Int
treeSeed n = if null (treeChildren n) then Nothing else Just n

treeUnfold :: Unfold IO Int Int
treeUnfold = Unfold.lmap treeChildren Unfold.fromList

treeConcatF :: Int -> Maybe (Stream IO Int)
treeConcatF n =
    let cs = treeChildren n
     in if null cs then Nothing else Just (Stream.fromList cs)

testUnfoldIterateMaybe :: Expectation
testUnfoldIterateMaybe = do
    result <- Stream.toList $
        Stream.unfoldIterateMaybe
            treeSeed treeUnfold (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 4, 5, 3, 6, 7]

testBfsUnfoldIterateMaybe :: Expectation
testBfsUnfoldIterateMaybe = do
    result <- Stream.toList $
        Stream.bfsUnfoldIterateMaybe
            treeSeed treeUnfold (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 3, 4, 5, 6, 7]

testWideDfsUnfoldIterateRev :: Expectation
testWideDfsUnfoldIterateRev = do
    result <- Stream.toList $
        Stream.wideDfsUnfoldIterateRev
            treeSeed treeUnfold (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 3, 6, 7, 4, 5]

testConcatIterate :: Expectation
testConcatIterate = do
    result <- Stream.toList $
        Stream.concatIterate treeConcatF (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 4, 5, 3, 6, 7]

testBfsConcatIterateRev :: Expectation
testBfsConcatIterateRev = do
    result <- Stream.toList $
        Stream.bfsConcatIterateRev treeConcatF (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 3, 6, 7, 4, 5]

testBfsUnfoldIterateRev :: Expectation
testBfsUnfoldIterateRev = do
    result <- Stream.toList $
        Stream.bfsUnfoldIterateRev
            treeSeed treeUnfold (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 3, 6, 7, 4, 5]

testBfsConcatIterate :: Expectation
testBfsConcatIterate = do
    result <- Stream.toList $
        Stream.bfsConcatIterate treeConcatF (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 3, 4, 5, 6, 7]

testWideDfsConcatIterateRev :: Expectation
testWideDfsConcatIterateRev = do
    result <- Stream.toList $
        Stream.wideDfsConcatIterateRev treeConcatF (Stream.fromPure (1 :: Int))
    result `shouldBe` [1, 2, 3, 6, 7, 4, 5]

-- Tree: 1->[2,3], 2->[4], 3->[5], 4->[7], 5->[6].
deepTreeChildren :: Int -> [Int]
deepTreeChildren n =
    case n of
        1 -> [2, 3]
        2 -> [4]
        3 -> [5]
        4 -> [7]
        5 -> [6]
        _ -> []

deepTreeConcatF :: Int -> Maybe (Stream IO Int)
deepTreeConcatF n =
    case deepTreeChildren n of
        [] -> Nothing
        cs -> Just (Stream.fromList cs)

{-# NOINLINE wideDfsConcatIterateDeep #-}
wideDfsConcatIterateDeep :: Stream IO Int -> Stream IO Int
wideDfsConcatIterateDeep = Stream.wideDfsConcatIterate deepTreeConcatF

{-# NOINLINE wideDfsConcatIterateRevDeep #-}
wideDfsConcatIterateRevDeep :: Stream IO Int -> Stream IO Int
wideDfsConcatIterateRevDeep = Stream.wideDfsConcatIterateRev deepTreeConcatF

{-# NOINLINE bfsConcatIterateRevDeep #-}
bfsConcatIterateRevDeep :: Stream IO Int -> Stream IO Int
bfsConcatIterateRevDeep = Stream.bfsConcatIterateRev deepTreeConcatF

testWideDfsConcatIterateDeep :: Expectation
testWideDfsConcatIterateDeep = do
    Stream.toList (wideDfsConcatIterateDeep (Stream.fromPure 1))
        `shouldReturn` [1, 2, 3, 4, 7, 5, 6]
    Stream.toList (wideDfsConcatIterateRevDeep (Stream.fromPure 1))
        `shouldReturn` [1, 2, 3, 5, 6, 4, 7]
    Stream.toList (bfsConcatIterateRevDeep (Stream.fromPure 1))
        `shouldReturn` [1, 2, 3, 5, 4, 7, 6]

deepTreeSeed :: Int -> Maybe Int
deepTreeSeed n = if null (deepTreeChildren n) then Nothing else Just n

deepTreeUnfold :: Unfold IO Int Int
deepTreeUnfold = Unfold.lmap deepTreeChildren Unfold.fromList

{-# NOINLINE wideDfsUnfoldIterateDeep #-}
wideDfsUnfoldIterateDeep :: Stream IO Int -> Stream IO Int
wideDfsUnfoldIterateDeep =
    Stream.wideDfsUnfoldIterate deepTreeSeed deepTreeUnfold

{-# NOINLINE wideDfsUnfoldIterateRevDeep #-}
wideDfsUnfoldIterateRevDeep :: Stream IO Int -> Stream IO Int
wideDfsUnfoldIterateRevDeep =
    Stream.wideDfsUnfoldIterateRev deepTreeSeed deepTreeUnfold

{-# NOINLINE bfsUnfoldIterateDeep #-}
bfsUnfoldIterateDeep :: Stream IO Int -> Stream IO Int
bfsUnfoldIterateDeep =
    Stream.bfsUnfoldIterateMaybe deepTreeSeed deepTreeUnfold

{-# NOINLINE bfsUnfoldIterateRevDeep #-}
bfsUnfoldIterateRevDeep :: Stream IO Int -> Stream IO Int
bfsUnfoldIterateRevDeep =
    Stream.bfsUnfoldIterateRev deepTreeSeed deepTreeUnfold

testUnfoldIterateDeep :: Expectation
testUnfoldIterateDeep = do
    Stream.toList (wideDfsUnfoldIterateDeep (Stream.fromPure 1))
        `shouldReturn` [1, 2, 3, 4, 7, 5, 6]
    Stream.toList (wideDfsUnfoldIterateRevDeep (Stream.fromPure 1))
        `shouldReturn` [1, 2, 3, 5, 6, 4, 7]
    Stream.toList (bfsUnfoldIterateDeep (Stream.fromPure 1))
        `shouldReturn` [1, 2, 3, 4, 5, 7, 6]
    Stream.toList (bfsUnfoldIterateRevDeep (Stream.fromPure 1))
        `shouldReturn` [1, 2, 3, 5, 4, 7, 6]

testConcatIterateScan :: Expectation
testConcatIterateScan = do
    -- generate: from acc, produce stream [acc+1, acc+2] while acc < 6
    -- scanner:  acc + element
    -- acc=0 -> stream [1,2], scan: 0+1=1, 1+2=3  (yields 1, 2)
    -- acc=3 -> stream [4,5], scan: 3+4=7, 7+5=12 (yields 4, 5)
    -- acc=12 >= 6 -> stop
    let generate acc =
            if acc >= 6
            then return Nothing
            else return $ Just (acc, Stream.fromList [acc + 1, acc + 2])
    result <- Stream.toList $
        Stream.concatIterateScan
            (\acc x -> return (acc + x))
            generate
            (0 :: Int)
    result `shouldBe` [1, 2, 4, 5]

-------------------------------------------------------------------------------
-- Fold Iterate
-------------------------------------------------------------------------------

testBfsReduceIterate :: Expectation
testBfsReduceIterate = do
    r1 <- Stream.bfsReduceIterate (\a b -> return (a + b :: Int))
            (Stream.fromList [1..4 :: Int])
    r1 `shouldBe` Just 10
    r2 <- Stream.bfsReduceIterate (\a b -> return (a + b :: Int))
            (Stream.fromList ([] :: [Int]))
    r2 `shouldBe` Nothing

moduleName :: String
moduleName = "Data.Stream.Tree"

main :: IO ()
main = hspec
  $ H.parallel
  $ describe moduleName $ do
    it "unfoldIterateMaybe" testUnfoldIterateMaybe
    it "concatIterate" testConcatIterate
    it "bfsConcatIterate" testBfsConcatIterate
    it "bfsConcatIterateRev" testBfsConcatIterateRev
    it "wideDfsConcatIterateRev" testWideDfsConcatIterateRev
    it "bfsUnfoldIterateMaybe" testBfsUnfoldIterateMaybe
    it "bfsUnfoldIterateRev" testBfsUnfoldIterateRev
    it "wideDfsUnfoldIterateRev" testWideDfsUnfoldIterateRev
    it "wideDfs/bfsConcatIterate(Rev) deep" testWideDfsConcatIterateDeep
    it "wideDfsUnfoldIterate(Rev)/bfsUnfoldIterateMaybe/Rev deep"
        testUnfoldIterateDeep
    it "concatIterateScan" testConcatIterateScan
    it "bfsReduceIterate" testBfsReduceIterate
