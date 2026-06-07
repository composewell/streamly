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

testInterleave :: Expectation
testInterleave =
    Stream.toList
        (Stream.interleave
            (Stream.fromList [1, 3, 5 :: Int])
            (Stream.fromList [2, 4, 6]))
        `shouldReturn` [1, 2, 3, 4, 5, 6]

testAltBfsUnfoldEach :: Expectation
testAltBfsUnfoldEach = do
    result <- fmap sort $ Stream.toList $
        Stream.altBfsUnfoldEach
            (Unfold.lmap (\n -> (1, n)) Unfold.enumerateFromToIntegral)
            (Stream.fromList [2, 3 :: Int])
    result `shouldBe` sort [1, 2, 1, 2, 3]

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

    it "interleave" testInterleave
    it "altBfsUnfoldEach" testAltBfsUnfoldEach
