-- |
-- Module      : Streamly.Test.Data.Array.Type
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Array.Type (typeMain, main) where

import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16)
import Foreign.ForeignPtr (newForeignPtr_, withForeignPtr)
import Foreign.Storable (peek)
import GHC.Ptr (plusPtr, Ptr(..))
import Streamly.Internal.Data.MutByteArray (sizeOf)
import System.Mem (performMajorGC)
import Test.Hspec (hspec, describe, it, shouldBe, shouldReturn, SpecWith)
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.System.IO (defaultChunkSize)
import Streamly.Test.Common (listEquals, performGCSweep)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.MutArray as MA
import qualified Streamly.Internal.Data.Stream as S
import qualified Test.Hspec as Hspec

type Array = A.Array

moduleName :: String
moduleName = "Data.Array"

#include "Streamly/Test/Data/Array/TypeCommon.hs"

testBreakOn :: [Word8] -> Word8 -> [Word8] -> Maybe [Word8] -> IO ()
testBreakOn inp sep bef aft = do
    (bef_, aft_) <- A.breakEndByWord8_ sep (A.fromList inp)
    bef_ `shouldBe` A.fromList bef
    aft_ `shouldBe` fmap A.fromList aft

getIntList :: Ptr Int -> Int -> IO [Int]
getIntList ptr byteLen = do
    performMajorGC
    getList ptr (ptr `plusPtr` byteLen)

    where

    sizeOfInt = sizeOf (Proxy :: Proxy Int)

    getList p limitP
        | p >= limitP = return []
    getList p limitP = do
        val <- peek p
        rest <- getList (p `plusPtr` sizeOfInt) limitP
        return $ val : rest

testUnsafePinnedAsPtr :: IO ()
testUnsafePinnedAsPtr = do
    arr <- MA.unsafeSliceOffLen 10 50 <$> MA.fromList ([0 .. 99] :: [Int])
    let arr1 = A.unsafeFreeze arr
    A.unsafePinnedAsPtr arr1 getIntList `shouldReturn` [10 .. 59]

testUnsafeAsForeignPtr :: IO ()
testUnsafeAsForeignPtr = do
    arr <- MA.unsafeSliceOffLen 10 50 <$> MA.fromList ([0 .. 99] :: [Int])
    let arr1 = A.unsafeFreeze arr
    A.unsafeAsForeignPtr arr1 getIntList1 `shouldReturn` [10 .. 59]
    where
    getIntList1 fp blen = withForeignPtr fp $ \p -> getIntList p blen

testForeignPtrConversionId :: IO ()
testForeignPtrConversionId = do
    arr0 <- MA.unsafeSliceOffLen 10 50 <$> MA.fromList ([0 .. 99] :: [Word8])
    let arr = A.unsafeFreeze arr0
    A.unsafeAsForeignPtr arr $ \a b -> do
        res <- A.unsafeFromForeignPtr a b
        performGCSweep 4 100000
        res `shouldBe` arr

testUnsafeFromForeignPtr :: IO ()
testUnsafeFromForeignPtr = do
    arr0 <- MA.unsafeSliceOffLen 10 50 <$> MA.fromList ([0 .. 99] :: [Word8])
    let arr = A.unsafeFreeze arr0
    A.unsafePinnedAsPtr arr $ \ptr len -> do
        fptr <- newForeignPtr_ ptr
        performMajorGC
        A.unsafeFromForeignPtr fptr len `shouldReturn` arr

testFromCString# :: IO ()
testFromCString# = do
    arr0 <- MA.unsafeSliceOffLen 10 50 <$> MA.fromList ([0 .. 99] :: [Word8])
    let arr = A.unsafeFreeze arr0
    A.unsafePinnedAsPtr (arr <> A.fromList [0]) $ \(Ptr addr#) _ -> do
        arr1 <- A.fromCString# addr#
        performGCSweep 4 100000
        arr1 `shouldBe` arr

testFromW16CString# :: IO ()
testFromW16CString# = do
    arr0 <- MA.unsafeSliceOffLen 10 50 <$> MA.fromList ([0 .. 99] :: [Word16])
    let arr = A.unsafeFreeze arr0
    A.unsafePinnedAsPtr (arr <> A.fromList [0]) $ \(Ptr addr#) _ -> do
        arr1 <- A.fromW16CString# addr#
        performGCSweep 4 100000
        arr1 `shouldBe` arr

testConcatArrayW8 :: Property
testConcatArrayW8 =
    forAll (vectorOf 10000 (arbitrary :: Gen Word8))
        $ \w8List -> do
              let w8ArrList = A.fromList . (: []) <$> w8List
              f2 <- S.fold Fold.toList $ A.concat $ S.fromList w8ArrList
              w8List `shouldBe` f2

-- Tests for exports of the Streamly.Internal.Data.Array.Type source module.
typeMain :: SpecWith ()
typeMain = do
    -- IMPORTANT NOTE: Before adding any test here first consider if it can
    -- be added to the Array/TypeCommon test module. Only those tests which are
    -- specific to the Unboxed Array module and do not apply to the Generic
    -- Array module should be added here.

    it "unsafePinnedAsPtr" testUnsafePinnedAsPtr
    describe "unsafeAsForeignPtr" $ do
        it "read via Ptr" testUnsafeAsForeignPtr
        it "roundtrip with unsafeFromForeignPtr" testForeignPtrConversionId
    -- Random Access / Slicing (Array.Type)
    describe "breakEndByWord8_" $ do
        it "[1,0,2] sep=0" (testBreakOn [1, 0, 2] 0 [1] (Just [2]))
        it "[1,0] sep=0" (testBreakOn [1, 0] 0 [1] (Just []))
        it "[1] sep=0" (testBreakOn [1] 0 [1] Nothing)
    prop "unsafeCreateOf" (foldManyWith (\n -> Fold.take n (A.unsafeCreateOf n)))
    it "fromCString#" testFromCString#
    it "fromW16CString#" testFromW16CString#
    it "unsafeFromForeignPtr" testUnsafeFromForeignPtr

    -- Stream of Arrays
    prop "concat" testConcatArrayW8

main :: IO ()
main =
    hspec $
    Hspec.parallel $
    modifyMaxSuccess (const maxTestCount) $
    describe moduleName $ do
        typeCommon
        typeMain
