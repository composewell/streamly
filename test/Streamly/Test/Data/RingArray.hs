-- |
-- Module      : Streamly.Test.Data.RingArray
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.RingArray (main) where

import Data.Maybe (fromJust, isNothing)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import Streamly.Internal.Data.RingArray (RingArray)
import Streamly.Internal.Data.MutByteArray (sizeOf)
import Streamly.Test.Common (performGCSweep)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.RingArray as RingArray
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude as P
import Test.Hspec as H

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

-- | Size of an 'Int' element in bytes.
iSize :: Int
iSize = sizeOf (Proxy :: Proxy Int)

-- | A simple, easy to eyeball, base list. The oldest (ring head) element is
-- 10, the newest is 50.
base :: [Int]
base = [10, 20, 30, 40, 50]

-- | Build a ring (with the head at index 0) from a list of Ints.
fromList :: [Int] -> IO (RingArray Int)
fromList xs = do
    marr <- MutArray.fromList xs
    pure $ fromJust $ RingArray.castMutArray marr

-- | Build a Word8 ring (with the head at index 0) from a list of Word8.
fromListW8 :: [Word8] -> IO (RingArray Word8)
fromListW8 xs = do
    marr <- MutArray.fromList xs
    pure $ fromJust $ RingArray.castMutArray marr

baseRing :: IO (RingArray Int)
baseRing = fromList base

-------------------------------------------------------------------------------
-- Casting from MutArray
-------------------------------------------------------------------------------

testCastMutArray :: IO ()
testCastMutArray = do
    marr <- MutArray.fromList base
    let ring = fromJust $ RingArray.castMutArray marr
    RingArray.toList ring `shouldReturn` base

-- A sliced MutArray (one that does not start at offset 0) cannot be cast.
testCastMutArraySlice :: IO ()
testCastMutArraySlice = do
    marr <- MutArray.fromList base
    let slc = MutArray.unsafeSliceOffLen 1 3 marr
    isNothing (RingArray.castMutArray slc) `shouldBe` True

testCastMutArrayWith :: IO ()
testCastMutArrayWith = do
    marr <- MutArray.fromList base
    let ring = fromJust $ RingArray.castMutArrayWith 2 marr
    -- Head at index 2 (value 30), reading wraps around.
    RingArray.toList ring `shouldReturn` [30, 40, 50, 10, 20]

testUnsafeCastMutArray :: IO ()
testUnsafeCastMutArray = do
    marr <- MutArray.fromList base
    let ring = RingArray.unsafeCastMutArray marr
    RingArray.toList ring `shouldReturn` base

testUnsafeCastMutArrayWith :: IO ()
testUnsafeCastMutArrayWith = do
    marr <- MutArray.fromList base
    let ring = RingArray.unsafeCastMutArrayWith 2 marr
    RingArray.toList ring `shouldReturn` [30, 40, 50, 10, 20]

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

testLength :: IO ()
testLength = do
    ring <- baseRing
    RingArray.length ring `shouldBe` 5

testByteLength :: IO ()
testByteLength = do
    ring <- baseRing
    RingArray.byteLength ring `shouldBe` 5 * iSize

-------------------------------------------------------------------------------
-- Random access
-------------------------------------------------------------------------------

testGetIndex :: IO ()
testGetIndex = do
    ring <- baseRing
    RingArray.getIndex 0 ring `shouldReturn` Just 10
    RingArray.getIndex 4 ring `shouldReturn` Just 50
    -- Negative indices count from the newest element.
    RingArray.getIndex (-1) ring `shouldReturn` Just 50
    RingArray.getIndex (-5) ring `shouldReturn` Just 10
    -- Out of bounds.
    RingArray.getIndex (5 * iSize) ring `shouldReturn` Nothing
    RingArray.getIndex (-5 * iSize) ring `shouldReturn` Nothing

testUnsafeGetIndex :: IO ()
testUnsafeGetIndex = do
    ring <- baseRing
    RingArray.unsafeGetIndex 0 ring `shouldReturn` 10
    RingArray.unsafeGetIndex 2 ring `shouldReturn` 30
    RingArray.unsafeGetIndex (-1) ring `shouldReturn` 50

testUnsafeGetHead :: IO ()
testUnsafeGetHead = do
    ring <- baseRing
    RingArray.unsafeGetHead ring `shouldReturn` 10

-------------------------------------------------------------------------------
-- Moving the head
-------------------------------------------------------------------------------

testMoveForward :: IO ()
testMoveForward = do
    ring <- baseRing
    let ring1 = RingArray.moveForward ring
    RingArray.unsafeGetHead ring1 `shouldReturn` 20
    RingArray.toList ring1 `shouldReturn` [20, 30, 40, 50, 10]

testMoveReverse :: IO ()
testMoveReverse = do
    ring <- baseRing
    -- Moving back from the oldest element wraps to the newest.
    let ring1 = RingArray.moveReverse ring
    RingArray.unsafeGetHead ring1 `shouldReturn` 50
    RingArray.toList ring1 `shouldReturn` [50, 10, 20, 30, 40]

testMoveBy :: IO ()
testMoveBy = do
    ring <- baseRing
    RingArray.toList (RingArray.moveBy 0 ring) `shouldReturn` base
    RingArray.toList (RingArray.moveBy 2 ring) `shouldReturn` [30, 40, 50, 10, 20]
    RingArray.toList (RingArray.moveBy (-2) ring)
        `shouldReturn` [40, 50, 10, 20, 30]

testMoveRoundTrip :: IO ()
testMoveRoundTrip = do
    ring <- baseRing
    let ring1 = RingArray.moveReverse (RingArray.moveForward ring)
    RingArray.toList ring1 `shouldReturn` base

-------------------------------------------------------------------------------
-- Streams, unfolds and conversion
-------------------------------------------------------------------------------

testRead :: IO ()
testRead = do
    ring <- baseRing
    Stream.toList (RingArray.read ring) `shouldReturn` base

testReadRev :: IO ()
testReadRev = do
    ring <- baseRing
    Stream.toList (RingArray.readRev ring) `shouldReturn` reverse base

testReader :: IO ()
testReader = do
    ring <- baseRing
    Stream.toList (Stream.unfold RingArray.reader ring) `shouldReturn` base

testReaderRev :: IO ()
testReaderRev = do
    ring <- baseRing
    Stream.toList (Stream.unfold RingArray.readerRev ring)
        `shouldReturn` reverse base

testToList :: IO ()
testToList = do
    ring <- baseRing
    RingArray.toList ring `shouldReturn` base

testToMutArray :: IO ()
testToMutArray = do
    ring <- baseRing
    marr <- RingArray.toMutArray ring
    MutArray.toList marr `shouldReturn` base
    -- The order is head-first even when the head has moved.
    marr1 <- RingArray.toMutArray (RingArray.moveBy 2 ring)
    MutArray.toList marr1 `shouldReturn` [30, 40, 50, 10, 20]

-------------------------------------------------------------------------------
-- In-place mutation
-------------------------------------------------------------------------------

testPutIndex :: IO ()
testPutIndex = do
    ring <- baseRing
    RingArray.putIndex 0 ring 99
    RingArray.toList ring `shouldReturn` [99, 20, 30, 40, 50]

    ring1 <- baseRing
    RingArray.putIndex 2 ring1 99
    RingArray.toList ring1 `shouldReturn` [10, 20, 99, 40, 50]

    ring2 <- baseRing
    RingArray.putIndex (-1) ring2 99
    RingArray.toList ring2 `shouldReturn` [10, 20, 30, 40, 99]

testReplace_ :: IO ()
testReplace_ = do
    ring <- baseRing
    ring1 <- RingArray.replace_ ring 99
    -- The oldest (10) is overwritten with 99 and becomes the newest.
    RingArray.toList ring1 `shouldReturn` [20, 30, 40, 50, 99]

testReplace :: IO ()
testReplace = do
    ring <- baseRing
    (ring1, old) <- RingArray.replace ring 99
    old `shouldBe` 10
    RingArray.toList ring1 `shouldReturn` [20, 30, 40, 50, 99]

-------------------------------------------------------------------------------
-- Casting the element type
-------------------------------------------------------------------------------

testUnsafeCast :: IO ()
testUnsafeCast = do
    ring <- baseRing
    let ring1 = RingArray.unsafeCast ring :: RingArray Int
    RingArray.toList ring1 `shouldReturn` base

testAsBytes :: IO ()
testAsBytes = do
    ring <- baseRing
    let bytes = RingArray.asBytes ring
    RingArray.length bytes `shouldBe` 5 * iSize
    RingArray.byteLength bytes `shouldBe` 5 * iSize

testCastJust :: IO ()
testCastJust = do
    ring <- baseRing
    let r = RingArray.cast ring :: Maybe (RingArray Word8)
    RingArray.length (fromJust r) `shouldBe` 5 * iSize

testCastNothing :: IO ()
testCastNothing = do
    -- A 5-byte ring cannot be cast to Int (8 bytes): 5 is not a multiple of 8.
    ring <- fromListW8 [1, 2, 3, 4, 5]
    let r = RingArray.cast ring :: Maybe (RingArray Int)
    isNothing r `shouldBe` True

-------------------------------------------------------------------------------
-- Casting to MutArray
-------------------------------------------------------------------------------

testAsMutArray :: IO ()
testAsMutArray = do
    ring <- baseRing
    let (marr, h) = RingArray.asMutArray ring
    h `shouldBe` 0
    MutArray.toList marr `shouldReturn` base
    -- The underlying array is unchanged; only the head offset is returned.
    let (marr1, h1) = RingArray.asMutArray (RingArray.moveBy 2 ring)
    h1 `shouldBe` 2 * iSize
    MutArray.toList marr1 `shouldReturn` base

testAsMutArray_ :: IO ()
testAsMutArray_ = do
    ring <- baseRing
    MutArray.toList (RingArray.asMutArray_ ring) `shouldReturn` base

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

testFoldlM' :: IO ()
testFoldlM' = do
    ring <- baseRing
    r <- RingArray.foldlM' (\b a -> pure (b + a)) 0 ring
    r `shouldBe` sum base

testFold :: IO ()
testFold = do
    ring <- baseRing
    RingArray.fold Fold.sum ring `shouldReturn` sum base
    RingArray.fold Fold.toList ring `shouldReturn` base
    -- Folds the whole ring starting at the head.
    RingArray.fold Fold.toList (RingArray.moveBy 2 ring)
        `shouldReturn` [30, 40, 50, 10, 20]

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

testShowRing :: IO ()
testShowRing = do
    ring <- baseRing
    s <- RingArray.showRing ring
    s `shouldBe` show base

-------------------------------------------------------------------------------
-- Stream of rings
-------------------------------------------------------------------------------

-- Note: the rings produced are mutable references, so each must be converted
-- to a list within the stream before the next iteration mutates it.

testRingsOf :: IO ()
testRingsOf = do
    res <- Stream.toList
        $ Stream.mapM RingArray.toList
        $ RingArray.ringsOf 3 (Stream.fromList [1 .. 5 :: Int])
    res `shouldBe` [[1], [1, 2], [1, 2, 3], [2, 3, 4], [3, 4, 5]]

testScanRingsOf :: IO ()
testScanRingsOf = do
    res <- Stream.toList
        $ Stream.mapM RingArray.toList
        $ Stream.postscanl (RingArray.scanRingsOf 3) (Stream.fromList [1 .. 5 :: Int])
    res `shouldBe` [[1], [1, 2], [1, 2, 3], [2, 3, 4], [3, 4, 5]]

testScanFoldRingsBy :: IO ()
testScanFoldRingsBy = do
    res <- Stream.toList
        $ Stream.postscanl
            (RingArray.scanFoldRingsBy Fold.sum 3) (Stream.fromList [1 .. 5 :: Int])
    -- Sliding window sums of size up to 3.
    res `shouldBe` [1, 3, 6, 9, 12]

testScanCustomFoldRingsBy :: IO ()
testScanCustomFoldRingsBy = do
    res <- Stream.toList
        $ Stream.postscanl
            (RingArray.scanCustomFoldRingsBy
                (pure . RingArray.length) 3)
            (Stream.fromList [1 .. 5 :: Int])
    res `shouldBe` [1, 2, 3, 3, 3]

testCreateOfLast :: IO ()
testCreateOfLast = do
    ring <- Stream.fold (RingArray.createOfLast 3) (Stream.fromList [1 .. 5 :: Int])
    RingArray.toList ring `shouldReturn` [3, 4, 5]

-------------------------------------------------------------------------------
-- Fast byte comparisons
-------------------------------------------------------------------------------

eqArrayN :: [Int] -> [Int] -> Int -> Int -> Bool -> IO ()
eqArrayN lstArr lstRing startR nBytes expected = do
    let arr = Array.fromList lstArr
    marr <- MutArray.fromList lstRing
    let ring =
            maybe (error "cast failed") id $ RingArray.castMutArrayWith startR marr
    performGCSweep 4 100000
    res <- RingArray.eqArrayN ring arr nBytes
    res `shouldBe` expected

eqArray :: [Int] -> [Int] -> Int -> Bool -> IO ()
eqArray lstArr lstRing startR expected = do
    let arr = Array.fromList lstArr
    marr <- MutArray.fromList lstRing
    let ring =
            maybe (error "cast failed") id $ RingArray.castMutArrayWith startR marr
    performGCSweep 4 100000
    res <- RingArray.eqArray ring arr
    res `shouldBe` expected

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.RingArray"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
        describe "Casting from MutArray" $ do
            it "castMutArray" testCastMutArray
            it "castMutArray on a slice fails" testCastMutArraySlice
            it "castMutArrayWith" testCastMutArrayWith
            it "unsafeCastMutArray" testUnsafeCastMutArray
            it "unsafeCastMutArrayWith" testUnsafeCastMutArrayWith

        describe "Size" $ do
            it "length" testLength
            it "byteLength" testByteLength

        describe "Random access" $ do
            it "getIndex" testGetIndex
            it "unsafeGetIndex" testUnsafeGetIndex
            it "unsafeGetHead" testUnsafeGetHead

        describe "Moving the head" $ do
            it "moveForward" testMoveForward
            it "moveReverse" testMoveReverse
            it "moveBy" testMoveBy
            it "moveForward then moveReverse is identity" testMoveRoundTrip

        describe "Streams and conversion" $ do
            it "read" testRead
            it "readRev" testReadRev
            it "reader" testReader
            it "readerRev" testReaderRev
            it "toList" testToList
            it "toMutArray" testToMutArray

        describe "In-place mutation" $ do
            it "putIndex" testPutIndex
            it "replace_" testReplace_
            it "replace" testReplace

        describe "Casting the element type" $ do
            it "unsafeCast" testUnsafeCast
            it "asBytes" testAsBytes
            it "cast (Just)" testCastJust
            it "cast (Nothing)" testCastNothing

        describe "Casting to MutArray" $ do
            it "asMutArray" testAsMutArray
            it "asMutArray_" testAsMutArray_

        describe "Folds" $ do
            it "foldlM'" testFoldlM'
            it "fold" testFold

        describe "Debugging" $ do
            it "showRing" testShowRing

        describe "Stream of rings" $ do
            it "ringsOf" testRingsOf
            it "scanRingsOf" testScanRingsOf
            it "scanFoldRingsBy" testScanFoldRingsBy
            it "scanCustomFoldRingsBy" testScanCustomFoldRingsBy
            it "createOfLast" testCreateOfLast

        describe "Eq" $ do
            let lstArr = [0..99]
                lstRing = [50..99] ++ [0..49]
            it "eqArrayN True (n < len)"
                   $ eqArrayN lstArr lstRing 50 75 True
            it "eqArrayN True (n > len)"
                   $ eqArrayN lstArr lstRing 50 200 True
            it "eqArrayN False"
                   $ eqArrayN lstArr lstRing 10 75 False
            it "eqArray True" $ eqArray lstArr lstRing 50 True
            it "eqArray False" $ eqArray lstArr lstRing 20 False
