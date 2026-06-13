-- |
-- Module      : Streamly.Test.Data.Array
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Array (main) where

import Data.Char (isLower)
import Data.List (sort)
import Data.Proxy (Proxy(..))
import Data.Word(Word8, Word16)
import Foreign.Storable (peek)
import Foreign.ForeignPtr (newForeignPtr_, withForeignPtr)
import GHC.Ptr (plusPtr, Ptr(..))
import Streamly.Internal.Data.MutByteArray (Unbox, sizeOf)
import Streamly.Internal.Data.MutArray (MutArray)
import System.Mem (performMajorGC)
import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose, chooseInt, listOf)
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

testFromStreamToStream :: Property
testFromStreamToStream = genericTestFromTo (const A.fromStream) A.read (==)

testFoldUnfold :: Property
testFoldUnfold =
    genericTestFromTo (const (S.fold A.create)) (S.unfold A.reader) (==)

testFromList :: Property
testFromList =
    forAll (choose (0, maxArrLen)) $ \len ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    let arr = A.fromList list
                    xs <- run $ S.fold Fold.toList $ S.unfold A.reader arr
                    assert (xs == list)

testLengthFromStream :: Property
testLengthFromStream = genericTestFrom (const A.fromStream)

unsafeWriteIndex :: [Int] -> Int -> Int -> IO Bool
unsafeWriteIndex xs i x = do
    arr <- MA.fromList xs
    MA.unsafePutIndex i arr x
    x1 <- MA.unsafeGetIndex i arr
    return $ x1 == x

lastN :: Int -> [a] -> [a]
lastN n l = drop (length l - n) l

testLastN :: Property
testLastN =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (choose (0, len)) $ \n ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    xs <- run
                        $ fmap A.toList
                        $ S.fold (A.createOfLast n)
                        $ S.fromList list
                    assert (xs == lastN n list)

testLastN_LN :: Int -> Int -> IO Bool
testLastN_LN len n = do
    let list = [1..len]
    l1 <- fmap A.toList $ S.fold (A.createOfLast n) $ S.fromList list
    let l2 = lastN n list
    return $ l1 == l2

testStrip :: IO Bool
testStrip = do
    dt <- MA.fromList "abcDEFgeh"
    dt' <- MA.dropAround isLower dt
    x <- MA.toList dt'
    return $ x == "DEF"

testStripLeft :: IO Bool
testStripLeft = do
    dt <- MA.fromList "abcDEF"
    dt' <- MA.dropAround isLower dt
    x <- MA.toList dt'
    return $ x == "DEF"

testStripRight :: IO Bool
testStripRight = do
    dt <- MA.fromList "DEFgeh"
    dt' <- MA.dropAround isLower dt
    x <- MA.toList dt'
    return $ x == "DEF"

testStripZero :: IO Bool
testStripZero = do
    dt <- MA.fromList "DEF"
    dt' <- MA.dropAround isLower dt
    x <- MA.toList dt'
    return $ x == "DEF"

testStripEmpty :: IO Bool
testStripEmpty = do
    dt <- MA.fromList "abc"
    dt' <- MA.dropAround isLower dt
    x <- MA.toList dt'
    return $ x == ""

testStripNull :: IO Bool
testStripNull = do
    dt <- MA.fromList ""
    dt' <- MA.dropAround isLower dt
    x <- MA.toList dt'
    return $ x == ""

unsafeSlice :: Int -> Int -> [Int] -> Bool
unsafeSlice i n list =
    let lst = take n $ drop i list
        arr = A.toList $ A.unsafeSliceOffLen i n $ A.fromList list
     in arr == lst

testBubbleWith :: Bool -> Property
testBubbleWith asc =
   forAll (listOf (chooseInt (-50, 100))) $ \ls0 ->
        monadicIO $ action ls0

        where

        action ls = do
            x <- S.fold (fldm ls) $ S.fromList ls
            lst <- MA.toList x
            if asc
            then assert (sort ls == lst)
            else assert (sort ls == reverse lst)

        fldm ls =
            Fold.foldlM'
                (\b a -> do
                    arr <- MA.snoc b a
                    if asc
                    then MA.bubble compare arr
                    else MA.bubble (flip compare) arr
                    return arr
                )
                (MA.emptyOf' $ length ls)

testBubbleAsc ::  Property
testBubbleAsc = testBubbleWith True

testBubbleDesc ::  Property
testBubbleDesc = testBubbleWith False

testByteLengthWithMA :: forall a. Unbox a => a -> IO ()
testByteLengthWithMA _ = do
     arrA <- MA.emptyOf' 100 :: IO (MutArray a)
     let arrW8 = MA.unsafeCast arrA :: MutArray Word8
     MA.byteLength arrA `shouldBe` MA.length arrW8

testBreakOn :: [Word8] -> Word8 -> [Word8] -> Maybe [Word8] -> IO ()
testBreakOn inp sep bef aft = do
    (bef_, aft_) <- A.breakEndByWord8_ sep (A.fromList inp)
    bef_ `shouldBe` A.fromList bef
    aft_ `shouldBe` fmap A.fromList aft

testWrite :: [Char] -> IO ()
testWrite inp = do
    arr <- S.fold A.create (S.fromList inp)
    A.toList arr `shouldBe` inp

testFromToList :: [Char] -> IO ()
testFromToList inp = A.toList (A.fromList inp) `shouldBe` inp

testUnsafeIndxedFromList :: [Char] -> IO ()
testUnsafeIndxedFromList inp =
    let arr = A.fromList inp
     in fmap (`A.unsafeGetIndex` arr) [0 .. (length inp - 1)] `shouldBe` inp

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

testAsPtrUnsafeMA :: IO ()
testAsPtrUnsafeMA = do
    arr <- MA.fromList ([0 .. 99] :: [Int])
    arr1 <- MA.pin arr
    MA.unsafeAsPtr arr1 getIntList `shouldReturn` [0 .. 99]

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

reallocMA :: Property
reallocMA =
    let len = 10000
        bSize = len * sizeOf (Proxy :: Proxy Char)
    in forAll (vectorOf len (arbitrary :: Gen Char)) $ \vec ->
           forAll (chooseInt (bSize - 2000, bSize + 2000)) $ \newBLen -> do
               arr <- MA.fromList vec
               arr1 <- MA.reallocBytes newBLen arr
               lst <- MA.toList arr
               lst1 <- MA.toList arr1
               lst `shouldBe` lst1

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
        -- Conversion/Casting (Array.Type)
        it "unsafePinnedAsPtr" testUnsafePinnedAsPtr
        describe "unsafeAsForeignPtr" $ do
            it "read via Ptr" testUnsafeAsForeignPtr
            it "roundtrip with unsafeFromForeignPtr" testForeignPtrConversionId
        -- Subarrays (Array.Type)
        describe "unsafeSliceOffLen" $ do
            it "partial" $ unsafeSlice 2 4 [1..10]
            it "none" $ unsafeSlice 10 0 [1..10]
            it "full" $ unsafeSlice 0 10 [1..10]
        -- Random Access / Slicing (Array.Type)
        describe "breakEndByWord8_" $ do
            it "[1,0,2] sep=0" (testBreakOn [1, 0, 2] 0 [1] (Just [2]))
            it "[1,0] sep=0" (testBreakOn [1, 0] 0 [1] (Just []))
            it "[1] sep=0" (testBreakOn [1] 0 [1] Nothing)
        -- Random Access / Stream Folds (Array.Type)
        describe "createOf" $ do
            prop "length . createOf n === n" testLength
            prop "reader . createOf === id" testFoldNUnfold
            prop "read . createOf === id" testFoldNToStream
            prop "readRev . createOf === reverse" testFoldNToStreamRev
            prop "foldMany concats to original" (foldManyWith A.createOf)
        prop "unsafeCreateOf" (foldManyWith (\n -> Fold.take n (A.unsafeCreateOf n)))
        describe "create" $ do
            prop "reader . create === id" testFoldUnfold
            it "abc" (testWrite "abc")
            it "\\22407" (testWrite "\22407")
        -- Random Access / From containers (Array.Type)
        prop "fromListN" testFromListN
        describe "fromList" $ do
            prop "reader . fromList === id" testFromList
            it "abc" (testFromToList "abc")
            it "\\22407" (testFromToList "\22407")
        describe "fromStreamN" $ do
            prop "length . fromStreamN n === n" testLengthFromStreamN
            prop "reader . fromStreamN === id" testFromStreamNUnfold
            prop "read . fromStreamN === id" testFromStreamNToStream
        describe "fromStream" $ do
            prop "length . fromStream === n" testLengthFromStream
            prop "read . fromStream === id" testFromStreamToStream
        it "fromCString#" testFromCString#
        it "fromW16CString#" testFromW16CString#
        it "unsafeFromForeignPtr" testUnsafeFromForeignPtr
        -- Reading / Indexing (Array.Type)
        describe "unsafeGetIndex" $ do
            it "abc" (testUnsafeIndxedFromList "abc")
            it "\\22407" (testUnsafeIndxedFromList "\22407")
        -- Streams of arrays / Concat (Array.Type)
        prop "concat" testConcatArrayW8
        -- Construction (Streamly.Internal.Data.Array)
        describe "createOfLast" $ do
            prop "0 <= n <= len" testLastN
            it "-1" (testLastN_LN 10 (-1) `shouldReturn` True)
            it "0" (testLastN_LN 10 0 `shouldReturn` True)
            it "length" (testLastN_LN 10 10 `shouldReturn` True)
            it "length + 1" (testLastN_LN 10 11 `shouldReturn` True)
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
        -- MutArray
        it "MA.unsafeAsPtr" testAsPtrUnsafeMA
        describe "MA.unsafePutIndex" $ do
            it "first" (unsafeWriteIndex [1..10] 0 0 `shouldReturn` True)
            it "middle" (unsafeWriteIndex [1..10] 5 0 `shouldReturn` True)
            it "last" (unsafeWriteIndex [1..10] 9 0 `shouldReturn` True)
        describe "MA.byteLength" $ do
            it "Int" (testByteLengthWithMA (undefined :: Int))
            it "Char" (testByteLengthWithMA (undefined :: Char))
        describe "MA.dropAround" $ do
            it "both sides" (testStrip `shouldReturn` True)
            it "left only" (testStripLeft `shouldReturn` True)
            it "right only" (testStripRight `shouldReturn` True)
            it "no match" (testStripZero `shouldReturn` True)
            it "all match" (testStripEmpty `shouldReturn` True)
            it "empty" (testStripNull `shouldReturn` True)
        -- XXX There is an issue https://github.com/composewell/streamly/issues/1577
        --prop "MA.bubble" testAppend
        describe "MA.bubble" $ do
            prop "ascending" testBubbleAsc
            prop "descending" testBubbleDesc
        prop "MA.reallocBytes" reallocMA
