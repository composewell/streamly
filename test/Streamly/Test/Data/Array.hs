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
import Data.Word(Word8)
import Foreign.Storable (peek)
import GHC.Ptr (plusPtr)
import Streamly.Internal.Data.MutByteArray (Unbox, sizeOf)
import Streamly.Internal.Data.MutArray (MutArray)
import Test.QuickCheck (chooseInt, listOf)

import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.MutArray as MA

#include "Streamly/Test/Data/Array/CommonImports.hs"

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
    dt' <- MA.strip isLower dt
    x <- MA.toList dt'
    return $ x == "DEF"

testStripLeft :: IO Bool
testStripLeft = do
    dt <- MA.fromList "abcDEF"
    dt' <- MA.strip isLower dt
    x <- MA.toList dt'
    return $ x == "DEF"

testStripRight :: IO Bool
testStripRight = do
    dt <- MA.fromList "DEFgeh"
    dt' <- MA.strip isLower dt
    x <- MA.toList dt'
    return $ x == "DEF"

testStripZero :: IO Bool
testStripZero = do
    dt <- MA.fromList "DEF"
    dt' <- MA.strip isLower dt
    x <- MA.toList dt'
    return $ x == "DEF"

testStripEmpty :: IO Bool
testStripEmpty = do
    dt <- MA.fromList "abc"
    dt' <- MA.strip isLower dt
    x <- MA.toList dt'
    return $ x == ""

testStripNull :: IO Bool
testStripNull = do
    dt <- MA.fromList ""
    dt' <- MA.strip isLower dt
    x <- MA.toList dt'
    return $ x == ""

unsafeSlice :: Int -> Int -> [Int] -> Bool
unsafeSlice i n list =
    let lst = take n $ drop i list
        arr = A.toList $ A.getSliceUnsafe i n $ A.fromList list
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
                (MA.pinnedEmptyOf $ length ls)

testBubbleAsc ::  Property
testBubbleAsc = testBubbleWith True

testBubbleDesc ::  Property
testBubbleDesc = testBubbleWith False

testByteLengthWithMA :: forall a. Unbox a => a -> IO ()
testByteLengthWithMA _ = do
     arrA <- MA.pinnedEmptyOf 100 :: IO (MutArray a)
     let arrW8 = MA.unsafeCast arrA :: MutArray Word8
     MA.byteLength arrA `shouldBe` MA.length arrW8

testBreakOn :: [Word8] -> Word8 -> [Word8] -> Maybe [Word8] -> IO ()
testBreakOn inp sep bef aft = do
    (bef_, aft_) <- A.breakOn sep (A.fromList inp)
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
     in fmap (`A.getIndexUnsafe` arr) [0 .. (length inp - 1)] `shouldBe` inp

testAsPtrUnsafeMA :: IO ()
testAsPtrUnsafeMA = do
    arr <- MA.fromList ([0 .. 99] :: [Int])
    MA.unsafePinnedAsPtr arr getList0 `shouldReturn` [0 .. 99]

    where

    sizeOfInt = sizeOf (Proxy :: Proxy Int)

    getList0 p byteLen = getList p (p `plusPtr` byteLen)

    -- We need to be careful here. We assume Unboxed and Storable are compatible
    -- with each other. For Int, they are compatible.
    getList p limitP
        | p >= limitP = return []
    getList p limitP = do
        val <- peek p
        rest <- getList (p `plusPtr` sizeOfInt) limitP
        return $ val : rest

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

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
      describe moduleName $ do
        commonMain
        describe "Construction" $ do
            -- XXX There is an issue https://github.com/composewell/streamly/issues/1577
            --prop "testAppend" testAppend
            prop "testBubbleAsc" testBubbleAsc
            prop "testBubbleDesc" testBubbleDesc
            prop "length . fromStream === n" testLengthFromStream
            prop "toStream . fromStream === id" testFromStreamToStream
            prop "read . write === id" testFoldUnfold
            prop "fromList" testFromList
            prop "foldMany with writeNUnsafe concats to original"
                (foldManyWith (\n -> Fold.take n (A.unsafeCreateOf n)))
        describe "unsafeSlice" $ do
            it "partial" $ unsafeSlice 2 4 [1..10]
            it "none" $ unsafeSlice 10 0 [1..10]
            it "full" $ unsafeSlice 0 10 [1..10]
        describe "Mut.unsafeWriteIndex" $ do
            it "first" (unsafeWriteIndex [1..10] 0 0 `shouldReturn` True)
            it "middle" (unsafeWriteIndex [1..10] 5 0 `shouldReturn` True)
            it "last" (unsafeWriteIndex [1..10] 9 0 `shouldReturn` True)
        describe "Fold" $ do
            prop "createOfLast : 0 <= n <= len" testLastN
            describe "createOfLast boundary conditions" $ do
                it "createOfLast -1" (testLastN_LN 10 (-1) `shouldReturn` True)
                it "createOfLast 0" (testLastN_LN 10 0 `shouldReturn` True)
                it "createOfLast length" (testLastN_LN 10 10 `shouldReturn` True)
                it "createOfLast (length + 1)" (testLastN_LN 10 11 `shouldReturn` True)
        describe "Strip" $ do
            it "strip" (testStrip `shouldReturn` True)
            it "stripLeft" (testStripLeft `shouldReturn` True)
            it "stripRight" (testStripRight `shouldReturn` True)
            it "stripZero" (testStripZero `shouldReturn` True)
            it "stripEmpty" (testStripEmpty `shouldReturn` True)
            it "stripNull" (testStripNull `shouldReturn` True)
        describe "Mut" $ do
            it "testByteLengthWithMA Int"
                   (testByteLengthWithMA (undefined :: Int))
            it "testByteLengthWithMA Char"
                   (testByteLengthWithMA (undefined :: Char))
            it "testAsPtrUnsafeMA" testAsPtrUnsafeMA
            it "reallocMA" reallocMA
        describe "breakOn" $ do
            it "testBreakOn [1, 0, 2] 0"
                   (testBreakOn [1, 0, 2] 0 [1] (Just [2]))
            it "testBreakOn [1, 0] 0" (testBreakOn [1, 0] 0 [1] (Just []))
            it "testBreakOn [1] 0" (testBreakOn [1] 0 [1] Nothing)
        describe "toList . fromList" $ do
            it "testFromToList abc" (testFromToList "abc")
            it "testFromToList \\22407" (testFromToList "\22407")
        describe "getIndexUnsafe . fromList" $ do
            it "testUnsafeIndxedFromList abc" (testUnsafeIndxedFromList "abc")
            it "testUnsafeIndxedFromList \\22407"
                   (testUnsafeIndxedFromList "\22407")
        describe "write" $ do
            it "testWrite abc" (testWrite "abc")
            it "testWrite \\22407" (testWrite "\22407")
