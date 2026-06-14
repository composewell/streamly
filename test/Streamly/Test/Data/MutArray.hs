-- |
-- Module      : Streamly.Test.Data.MutArray
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.MutArray (main) where

import Data.Proxy (Proxy(..))
import GHC.Word (Word8(..))
import Streamly.Internal.Data.MutArray (MutArray)
import Test.Hspec (hspec, describe, it, shouldBe, SpecWith)
import Test.Hspec.QuickCheck
import Test.QuickCheck (forAll, listOf, Property, Gen, arbitrary)
import Test.QuickCheck.Monadic (monadicIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Test.Hspec as Hspec

import Streamly.Test.Common (chooseInt)
import Streamly.Test.Data.MutArray.Type (typeMain)

moduleName :: String
moduleName = "Data.MutArray"

maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

-------------------------------------------------------------------------------
-- MutArray module
-------------------------------------------------------------------------------

testIndexerFromLen :: IO ()
testIndexerFromLen = do
    arr <- MArray.fromList ([1..10] :: [Int])
    pairs <- Stream.fold Fold.toList
        $ Stream.unfold (MArray.indexerFromLen 0 3) arr
    pairs `shouldBe` [(0,3),(3,3),(6,3),(9,1)]

testSplitterFromLen :: IO ()
testSplitterFromLen = do
    arr <- MArray.fromList ([1..10] :: [Int])
    slices <- Stream.fold Fold.toList
        $ Stream.unfold (MArray.splitterFromLen 0 3) arr
    lsts <- mapM MArray.toList slices
    lsts `shouldBe` [[1,2,3],[4,5,6],[7,8,9],[10]]

testCompactMax :: IO ()
testCompactMax = do
    arrs <- mapM MArray.fromList ([[1,2],[3,4],[5,6],[7,8]] :: [[Int]])
    result <- Stream.fold Fold.toList
        $ MArray.compactMax 4
        $ Stream.fromList arrs
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2,3,4],[5,6,7,8]]

testCompactMax' :: IO ()
testCompactMax' = do
    arrs <- mapM MArray.fromList ([[1,2],[3,4]] :: [[Int]])
    result <- Stream.fold Fold.toList
        $ MArray.compactMax' 4
        $ Stream.fromList arrs
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2,3,4]]

testCompactSepByByte_ :: IO ()
testCompactSepByByte_ = do
    arr1 <- MArray.fromList ([1,2,0,3] :: [Word8])
    arr2 <- MArray.fromList ([4,0,5] :: [Word8])
    result <- Stream.fold Fold.toList
        $ MArray.compactSepByByte_ 0
        $ Stream.fromList [arr1, arr2]
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2],[3,4],[5]]

testCompactEndByByte_ :: IO ()
testCompactEndByByte_ = do
    arr1 <- MArray.fromList ([1,2,0,3] :: [Word8])
    arr2 <- MArray.fromList ([4,0,5] :: [Word8])
    result <- Stream.fold Fold.toList
        $ MArray.compactEndByByte_ 0
        $ Stream.fromList [arr1, arr2]
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2],[3,4],[5]]

testCompactEndByLn_ :: IO ()
testCompactEndByLn_ = do
    arr1 <- MArray.fromList ([1,2,10,3] :: [Word8])
    arr2 <- MArray.fromList ([4,10,5,6] :: [Word8])
    result <- Stream.fold Fold.toList
        $ MArray.compactEndByLn_
        $ Stream.fromList [arr1, arr2]
    lsts <- mapM MArray.toList result
    lsts `shouldBe` [[1,2],[3,4],[5,6]]

lastN :: Int -> [a] -> [a]
lastN n xs = drop (max 0 (length xs - n)) xs

testCreateOfLast :: Property
testCreateOfLast =
    forAll (chooseInt (0, 20)) $ \n ->
        forAll (listOf (arbitrary :: Gen Int)) $ \ls -> do
            arr <- Stream.fold (MArray.createOfLast n) (Stream.fromList ls)
            lst <- MArray.toList arr
            lst `shouldBe` lastN n ls

testSerializeDeserialize :: IO ()
testSerializeDeserialize = do
    let val = 42 :: Int
    arr <- MArray.serialize MArray.empty val
    (result, _) <- MArray.deserialize arr :: IO (Int, MutArray Word8)
    result `shouldBe` val

testSerializeDeserializeMultiple :: IO ()
testSerializeDeserializeMultiple = do
    let x = 1 :: Int
        y = 2 :: Int
    arr <- MArray.serialize MArray.empty x
    arr1 <- MArray.serialize arr y
    (v1, arr2) <- MArray.deserialize arr1 :: IO (Int, MutArray Word8)
    (v2, _) <- MArray.deserialize arr2 :: IO (Int, MutArray Word8)
    v1 `shouldBe` x
    v2 `shouldBe` y

mutArrayMain :: SpecWith ()
mutArrayMain = do
    -- MutArray module
    describe "indexerFromLen" $ do
        it "basic" testIndexerFromLen
    describe "splitterFromLen" $ do
        it "basic" testSplitterFromLen
    describe "compactMax" $ do
        it "coalesces small arrays" testCompactMax
    describe "compactMax'" $ do
        it "coalesces to pinned" testCompactMax'
    describe "compactSepByByte_" $ do
        it "splits on separator" testCompactSepByByte_
    describe "compactEndByByte_" $ do
        it "splits on suffix" testCompactEndByByte_
    describe "compactEndByLn_" $ do
        it "splits on newline" testCompactEndByLn_
    describe "createOfLast" $ do
        prop "last n elements" testCreateOfLast
    describe "serialize/deserialize" $ do
        it "roundtrip"       testSerializeDeserialize
        it "multiple values" testSerializeDeserializeMultiple

main :: IO ()
main =
    hspec $
    Hspec.parallel $
    modifyMaxSuccess (const maxTestCount) $
    describe moduleName $ do
        typeMain
        mutArrayMain
