-- |
-- Module      : Streamly.Test.Data.Array
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Array (main) where

import Data.Proxy (Proxy(..))
import Data.Word(Word8, Word16)
import Foreign.Storable (peek)
import Foreign.ForeignPtr (newForeignPtr_, withForeignPtr)
import GHC.Ptr (plusPtr, Ptr(..))
import Streamly.Internal.Data.MutByteArray (sizeOf)
import System.Mem (performMajorGC)
import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose, chooseInt)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.System.IO (defaultChunkSize)
import Streamly.Test.Common (listEquals, performGCSweep)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.MutArray as MA
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Internal.Data.Stream as S

#if MIN_VERSION_QuickCheck(2,14,0)
import Test.QuickCheck (chooseAny)
#else
import System.Random (Random(random))
import Test.QuickCheck.Gen (Gen(MkGen))

chooseAny :: Random a => Gen a
chooseAny = MkGen (\r _ -> let (x, _) = random r in x)
#endif

type Array = A.Array

moduleName :: String
moduleName = "Data.Array"

#include "Streamly/Test/Data/Array/Common.hs"

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

    -- We need to be careful here. We assume Unboxed and Storable are compatible
    -- with each other. For Int, they are compatible.
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

-------------------------------------------------------------------------------
-- Array.Stream tests
-------------------------------------------------------------------------------

chunksOf :: Monad m => Int -> Fold.Fold m a b -> S.Stream m a -> S.Stream m b
chunksOf n f = S.foldMany (Fold.take n f)

testParseBreak :: Property
testParseBreak = do
    let len = 200
    forAll
        ((,,)
            <$> vectorOf len (chooseAny :: Gen Int)
            <*> chooseInt (1, len)
            <*> chooseInt (0, len))
        $ \(ls, clen, tlen) ->
            monadicIO $ do
                (ls1, str) <-
                    let input =
                            S.toStreamK
                                $ chunksOf clen (A.createOf clen) (S.fromList ls)
                        parser = Parser.fromFold (Fold.take tlen Fold.toList)
                     in run $ A.parseBreak (A.toParserK parser) input
                ls2 <- run $ S.fold Fold.toList (A.concat $ S.fromStreamK str)
                case ls1 of
                    Right x -> listEquals (==) (x ++ ls2) ls
                    Left _ -> assert False

testSplitOnSuffix :: Word8 -> [Word8] -> [[Word8]] -> IO ()
testSplitOnSuffix sep inp out = do
    res <-
        S.fold Fold.toList
            $ A.compactEndByByte_ sep
            $ chunksOf 2 (A.createOf 2) $ S.fromList inp
    fmap A.toList res `shouldBe` out

testConcatArrayW8 :: Property
testConcatArrayW8 =
    forAll (vectorOf 10000 (arbitrary :: Gen Word8))
        $ \w8List -> do
              let w8ArrList = A.fromList . (: []) <$> w8List
              f2 <- S.fold Fold.toList $ A.concat $ S.fromList w8ArrList
              w8List `shouldBe` f2

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
      describe moduleName $ do
        -----------------------------------------------------------------------
        -- Array.Type module
        -----------------------------------------------------------------------

        -- Conversion/Casting (Array.Type)
        it "unsafePinnedAsPtr" testUnsafePinnedAsPtr
        describe "unsafeAsForeignPtr" $ do
            it "read via Ptr" testUnsafeAsForeignPtr
            it "roundtrip with unsafeFromForeignPtr" testForeignPtrConversionId
        -- Random Access / Slicing (Array.Type)
        describe "breakEndByWord8_" $ do
            it "[1,0,2] sep=0" (testBreakOn [1, 0, 2] 0 [1] (Just [2]))
            it "[1,0] sep=0" (testBreakOn [1, 0] 0 [1] (Just []))
            it "[1] sep=0" (testBreakOn [1] 0 [1] Nothing)
        commonMain
        prop "unsafeCreateOf" (foldManyWith (\n -> Fold.take n (A.unsafeCreateOf n)))
        it "fromCString#" testFromCString#
        it "fromW16CString#" testFromW16CString#
        it "unsafeFromForeignPtr" testUnsafeFromForeignPtr
        -- Streams of arrays / Concat (Array.Type)
        prop "concat" testConcatArrayW8

        -----------------------------------------------------------------------
        -- Array module
        -----------------------------------------------------------------------

        -- Stream of Arrays (Streamly.Internal.Data.Array)
        describe "compactEndByByte_" $ do
            it "0 [1,2,0,4,0,5,6]"
                   $ testSplitOnSuffix 0 [1, 2, 0, 4, 0, 5, 6]
                                        [[1, 2], [4], [5, 6]]
            it "0 [1,2,0,4,0,5,6,0]"
                   $ testSplitOnSuffix 0 [1, 2, 0, 4, 0, 5, 6, 0]
                                        [[1, 2], [4], [5, 6]]
            it "0 [0,1,2,0,4,0,5,6]"
                   $ testSplitOnSuffix 0 [0, 1, 2, 0, 4, 0, 5, 6]
                                        [[], [1, 2], [4], [5, 6]]
        -- Parsing Stream of Arrays (Streamly.Internal.Data.Array)
        prop "parseBreak" testParseBreak
