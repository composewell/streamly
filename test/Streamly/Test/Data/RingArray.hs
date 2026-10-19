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
import Streamly.Internal.Data.MutByteArray (sizeOf)
import Streamly.Internal.Data.RingArray (RingArray)
import Streamly.Test.Common (performGCSweep)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.MutArray.Generic as GMutArray
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.RingArray as RingArray
import qualified Streamly.Internal.Data.RingArray.Generic as GRingArray
import qualified Streamly.Internal.Data.Stream as Stream

import Prelude as P
import Test.Hspec as H

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

iSize :: Int
iSize = sizeOf (Proxy :: Proxy Int)

-- | Build a ring with the head at index 0.
fromList :: [Int] -> IO (RingArray Int)
fromList xs = do
    marr <- MutArray.fromList xs
    pure $ fromJust $ RingArray.castMutArray marr

fromListW8 :: [Word8] -> IO (RingArray Word8)
fromListW8 xs = do
    marr <- MutArray.fromList xs
    pure $ fromJust $ RingArray.castMutArray marr

testGetIndexBounds :: IO ()
testGetIndexBounds = do
    ring <- fromList [10, 20, 30, 40, 50]
    RingArray.getIndex 4 ring `shouldReturn` Just 50
    RingArray.getIndex (-5) ring `shouldReturn` Just 10
    RingArray.getIndex 5 ring `shouldReturn` Nothing
    RingArray.getIndex (-6) ring `shouldReturn` Nothing

-- The ring length (16 bytes) is a multiple of the Int size but the head (byte
-- 3) is not, so the Int elements would straddle the end of the ring.
testCastMisalignedHead :: IO ()
testCastMisalignedHead = do
    ring <- fromListW8 [1 .. 16]
    let r = RingArray.cast (RingArray.moveBy 3 ring) :: Maybe (RingArray Int)
    isNothing r `shouldBe` True

testFoldEmpty :: IO ()
testFoldEmpty = do
    ring <- Stream.fold (RingArray.createOfLast 3) (Stream.fromList [])
    RingArray.length ring `shouldBe` 0
    RingArray.fold Fold.toList ring `shouldReturn` ([] :: [Int])
    RingArray.foldlM' (\b a -> pure (b + a)) 0 ring `shouldReturn` 0

-- The array is a slice that does not start at offset 0 of its memory.
eqArraySlice :: IO ()
eqArraySlice = do
    let arr = Array.unsafeSliceOffLen 2 5 (Array.fromList ([0, 0] ++ [1 .. 5]))
    ring <- fromList [1 .. 5]
    RingArray.eqArray ring arr `shouldReturn` True
    RingArray.eqArrayN ring arr (3 * iSize) `shouldReturn` True
    ring1 <- fromList [0 .. 4]
    RingArray.eqArray ring1 arr `shouldReturn` False
    RingArray.eqArrayN ring1 arr (3 * iSize) `shouldReturn` False

-- Copy fewer elements than the ring has, without wrapping around.
testCopyToMutArrayPartial :: IO ()
testCopyToMutArrayPartial = do
    rng <-
        Stream.fold (GRingArray.createOf 5) (Stream.fromList [1 .. 5 :: Int])
    marr <- GRingArray.copyToMutArray 0 2 rng
    GMutArray.toList marr `shouldReturn` [1, 2]
    marr1 <- GRingArray.copyToMutArray 1 3 rng
    GMutArray.toList marr1 `shouldReturn` [2, 3, 4]

moduleName :: String
moduleName = "Data.RingArray"

main :: IO ()
main = hspec $ do
    describe moduleName $ do
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
            it "eqArray and eqArrayN on an array slice" eqArraySlice
        it "getIndex bounds" testGetIndexBounds
        it "cast (misaligned head)" testCastMisalignedHead
        it "fold (empty ring)" testFoldEmpty
        it "Generic copyToMutArray (partial)" testCopyToMutArrayPartial
