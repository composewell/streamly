-- |
-- Module      : Streamly.Test.Data.Array.Foreign
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Array.Foreign (main) where

#include "Streamly/Test/Data/Array/CommonImports.hs"

import Data.Char (isLower)
import Data.List (sort)
import Data.Word(Word8)
import Test.QuickCheck (chooseInt, listOf)

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array.Unboxed as A
import qualified Streamly.Internal.Data.Array.Foreign.Type as A
import qualified Streamly.Internal.Data.Array.Foreign.Mut.Type as MA
import qualified Streamly.Internal.Data.Array.Stream.Foreign as AS

type Array = A.Array

moduleName :: String
moduleName = "Data.Array.Foreign"

#include "Streamly/Test/Data/Array/Common.hs"

testFromStreamToStream :: Property
testFromStreamToStream = genericTestFromTo (const A.fromStream) A.toStream (==)

testFoldUnfold :: Property
testFoldUnfold = genericTestFromTo (const (S.fold A.write)) (S.unfold A.read) (==)

testFromList :: Property
testFromList =
    forAll (choose (0, maxArrLen)) $ \len ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    let arr = A.fromList list
                    xs <- run $ S.toList $ (S.unfold A.read) arr
                    assert (xs == list)

testLengthFromStream :: Property
testLengthFromStream = genericTestFrom (const A.fromStream)


unsafeWriteIndex :: [Int] -> Int -> Int -> IO Bool
unsafeWriteIndex xs i x = do
    arr <- MA.fromList xs
    MA.putIndexUnsafe i x arr
    x1 <- MA.getIndexUnsafe i arr
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
                        $ S.fold (A.writeLastN n)
                        $ S.fromList list
                    assert (xs == lastN n list)

testLastN_LN :: Int -> Int -> IO Bool
testLastN_LN len n = do
    let list = [1..len]
    l1 <- fmap A.toList $ S.fold (A.writeLastN n) $ S.fromList list
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

-- Instead of hard coding 10000 here we can have maxStreamLength for operations
-- that use stream of arrays.
concatArrayW8 :: Property
concatArrayW8 =
    forAll (vectorOf 10000 (arbitrary :: Gen Word8))
        $ \w8List -> do
              let w8ArrList = A.fromList . (: []) <$> w8List
              f2 <- S.toList $ AS.concat $ S.fromList w8ArrList
              w8List `shouldBe` f2

unsafeSlice :: Int -> Int -> [Int] -> Bool
unsafeSlice i n list =
    let lst = take n $ drop i $ list
        arr = A.toList $ A.getSliceUnsafe i n $ A.fromList list
     in arr == lst

{-
testAppend ::  Property
testAppend =
   forAll (listOf (chooseInt (-50, 100))) $ \ls0 ->
        monadicIO $ action ls0

        where

        action ls = do
            x <- S.fold
                    (MA.append (MA.newArray 0))
                    (S.fromList (ls::[Int]))
            lst <- MA.toList x
            assert (ls == lst)
-}

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
                (MA.newArray $ length ls)

testBubbleAsc ::  Property
testBubbleAsc = testBubbleWith True

testBubbleDesc ::  Property
testBubbleDesc = testBubbleWith False

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
                (foldManyWith (\n -> Fold.take n (A.writeNUnsafe n)))
            prop "AS.concat . (A.fromList . (:[]) <$>) === id" $ concatArrayW8
        describe "unsafeSlice" $ do
            it "partial" $ unsafeSlice 2 4 [1..10]
            it "none" $ unsafeSlice 10 0 [1..10]
            it "full" $ unsafeSlice 0 10 [1..10]
        describe "Mut.unsafeWriteIndex" $ do
            it "first" (unsafeWriteIndex [1..10] 0 0 `shouldReturn` True)
            it "middle" (unsafeWriteIndex [1..10] 5 0 `shouldReturn` True)
            it "last" (unsafeWriteIndex [1..10] 9 0 `shouldReturn` True)
        describe "Fold" $ do
            prop "writeLastN : 0 <= n <= len" $ testLastN
            describe "writeLastN boundary conditions" $ do
                it "writeLastN -1" (testLastN_LN 10 (-1) `shouldReturn` True)
                it "writeLastN 0" (testLastN_LN 10 0 `shouldReturn` True)
                it "writeLastN length" (testLastN_LN 10 10 `shouldReturn` True)
                it "writeLastN (length + 1)" (testLastN_LN 10 11 `shouldReturn` True)
        describe "Strip" $ do
            it "strip" (testStrip `shouldReturn` True)
            it "stripLeft" (testStripLeft `shouldReturn` True)
            it "stripRight" (testStripRight `shouldReturn` True)
            it "stripZero" (testStripZero `shouldReturn` True)
            it "stripEmpty" (testStripEmpty `shouldReturn` True)
            it "stripNull" (testStripNull `shouldReturn` True)
