-- |
-- Module      : Streamly.Test.Data.RingArray.Generic
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.RingArray.Generic (main) where

import Streamly.Internal.Data.RingArray.Generic (RingArray(..))

import qualified Streamly.Internal.Data.MutArray.Generic as MutArray
import qualified Streamly.Internal.Data.RingArray.Generic as RingArray
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude as P
import Test.Hspec as H

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Read the entire ring, starting at the ring head (oldest element), into a
-- list.
ringToList :: RingArray a -> IO [a]
ringToList rng = do
    marr <- RingArray.toMutArray 0 (ringMax rng) rng
    MutArray.toList marr

-- | Build a ring containing the last @n@ elements of the given list.
fromListLastN :: Int -> [a] -> IO (RingArray a)
fromListLastN n xs = Stream.fold (RingArray.createOf n) (Stream.fromList xs)

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

testEmptyOf :: IO ()
testEmptyOf = do
    rng <- RingArray.emptyOf 5 :: IO (RingArray Int)
    ringHead rng `shouldBe` 0
    ringMax rng `shouldBe` 5

testEmptyOfZero :: IO ()
testEmptyOfZero = do
    rng <- RingArray.emptyOf 0 :: IO (RingArray Int)
    ringHead rng `shouldBe` 0
    ringMax rng `shouldBe` 0
    ringToList rng `shouldReturn` ([] :: [Int])

testCreateOfFull :: IO ()
testCreateOfFull = do
    -- More elements than capacity: keep the newest n.
    rng <- fromListLastN 3 [1 .. 5 :: Int]
    ringMax rng `shouldBe` 3
    ringToList rng `shouldReturn` [3, 4, 5]

testCreateOfExact :: IO ()
testCreateOfExact = do
    rng <- fromListLastN 3 [1 .. 3 :: Int]
    ringMax rng `shouldBe` 3
    ringToList rng `shouldReturn` [1, 2, 3]

testCreateOfUnderfull :: IO ()
testCreateOfUnderfull = do
    -- Fewer elements than capacity: only the inserted elements, in order.
    rng <- fromListLastN 3 [1, 2 :: Int]
    ringToList rng `shouldReturn` [1, 2]

testCreateOfZero :: IO ()
testCreateOfZero = do
    rng <- fromListLastN 0 [1 .. 5 :: Int]
    ringMax rng `shouldBe` 0
    ringToList rng `shouldReturn` ([] :: [Int])

-------------------------------------------------------------------------------
-- Modification
-------------------------------------------------------------------------------

testUnsafeInsertRingWith :: IO ()
testUnsafeInsertRingWith = do
    rng <- RingArray.emptyOf 3 :: IO (RingArray Int)
    -- The head returned wraps around when it reaches ringMax.
    h1 <- RingArray.unsafeInsertRingWith rng 10
    h1 `shouldBe` 1
    h2 <- RingArray.unsafeInsertRingWith rng { ringHead = h1 } 20
    h2 `shouldBe` 2
    h3 <- RingArray.unsafeInsertRingWith rng { ringHead = h2 } 30
    h3 `shouldBe` 0
    -- Overwrites the oldest element (10).
    h4 <- RingArray.unsafeInsertRingWith rng { ringHead = h3 } 40
    h4 `shouldBe` 1
    ringToList rng { ringHead = h4 } `shouldReturn` [20, 30, 40]

testSeek :: IO ()
testSeek = do
    rng <- fromListLastN 3 [1 .. 3 :: Int]
    -- Clockwise.
    (ringToList =<< RingArray.seek 1 rng) `shouldReturn` [2, 3, 1]
    -- Counter clockwise.
    (ringToList =<< RingArray.seek (-1) rng) `shouldReturn` [3, 1, 2]
    -- A full rotation is a no-op.
    (ringToList =<< RingArray.seek 3 rng) `shouldReturn` [1, 2, 3]

testSeekEmpty :: IO ()
testSeekEmpty = do
    rng <- RingArray.emptyOf 0 :: IO (RingArray Int)
    rng1 <- RingArray.seek 2 rng
    ringHead rng1 `shouldBe` 0
    ringMax rng1 `shouldBe` 0

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

testToMutArray :: IO ()
testToMutArray = do
    rng <- fromListLastN 3 [1 .. 3 :: Int]
    -- Full read.
    marr <- RingArray.toMutArray 0 3 rng
    MutArray.toList marr `shouldReturn` [1, 2, 3]
    -- Partial read.
    marr1 <- RingArray.toMutArray 0 2 rng
    MutArray.toList marr1 `shouldReturn` [1, 2]
    -- Read with a head adjustment.
    marr2 <- RingArray.toMutArray 1 3 rng
    MutArray.toList marr2 `shouldReturn` [2, 3, 1]

testToMutArrayWrap :: IO ()
testToMutArrayWrap = do
    -- A full ring whose head is in the middle requires wrap-around handling.
    rng <- fromListLastN 3 [1 .. 5 :: Int]
    marr <- RingArray.toMutArray 0 3 rng
    MutArray.toList marr `shouldReturn` [3, 4, 5]

testToMutArrayEmpty :: IO ()
testToMutArrayEmpty = do
    rng <- RingArray.emptyOf 0 :: IO (RingArray Int)
    marr <- RingArray.toMutArray 0 5 rng
    MutArray.toList marr `shouldReturn` ([] :: [Int])

testCopyToMutArray :: IO ()
testCopyToMutArray = do
    rng <- fromListLastN 3 [1 .. 5 :: Int]
    marr <- RingArray.copyToMutArray 0 3 rng
    MutArray.toList marr `shouldReturn` [3, 4, 5]
    -- The copy is independent of the ring's underlying memory.
    _ <- RingArray.unsafeInsertRingWith rng 99
    MutArray.toList marr `shouldReturn` [3, 4, 5]

testCopyToMutArrayEmpty :: IO ()
testCopyToMutArrayEmpty = do
    rng <- RingArray.emptyOf 0 :: IO (RingArray Int)
    marr <- RingArray.copyToMutArray 0 5 rng
    MutArray.toList marr `shouldReturn` ([] :: [Int])

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.RingArray.Generic"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        describe "Generation" $ do
            it "emptyOf" testEmptyOf
            it "emptyOf 0" testEmptyOfZero
            it "createOf (more than capacity)" testCreateOfFull
            it "createOf (exactly capacity)" testCreateOfExact
            it "createOf (fewer than capacity)" testCreateOfUnderfull
            it "createOf 0" testCreateOfZero

        describe "Modification" $ do
            it "unsafeInsertRingWith" testUnsafeInsertRingWith
            it "seek" testSeek
            it "seek on empty ring" testSeekEmpty

        describe "Conversion" $ do
            it "toMutArray" testToMutArray
            it "toMutArray (wrap around)" testToMutArrayWrap
            it "toMutArray (empty ring)" testToMutArrayEmpty
            it "copyToMutArray" testCopyToMutArray
            it "copyToMutArray (empty ring)" testCopyToMutArrayEmpty
