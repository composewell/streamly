
-- |
-- Module      : Streamly.Test.Data.Array.Generic
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Array.Generic (main) where

import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.System.IO (defaultChunkSize)
import Streamly.Test.Common (listEquals)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array.Generic as A
import qualified Streamly.Internal.Data.Stream as S

type Array = A.Array

moduleName :: String
moduleName = "Data.Array"

#include "Streamly/Test/Data/Array/Common.hs"

testFromStreamToStream :: Property
testFromStreamToStream =
    genericTestFromTo (const A.fromStream) A.read (==)

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

testReadShowInstance :: Property
testReadShowInstance =
    forAll (choose (0, maxArrLen)) $ \len ->
            forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
                monadicIO $ do
                    let arr = A.fromList list
                    assert (A.toList (read (show arr)) == list)


main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
      describe moduleName $ do
        -- Construction
        describe "createOf" $ do
            prop "length . createOf n === n" testLength
            prop "reader . createOf === id" testFoldNUnfold
            prop "read . createOf === id" testFoldNToStream
            prop "readRev . createOf === reverse" testFoldNToStreamRev
            prop "foldMany concats to original" (foldManyWith A.createOf)
        prop "reader . create === id" testFoldUnfold
        describe "fromStreamN" $ do
            prop "length . fromStreamN n === n" testLengthFromStreamN
            prop "reader . fromStreamN === id" testFromStreamNUnfold
            prop "read . fromStreamN === id" testFromStreamNToStream
        describe "fromStream" $ do
            prop "length . fromStream === n" testLengthFromStream
            prop "read . fromStream === id" testFromStreamToStream
        prop "fromListN" testFromListN
        prop "fromList" testFromList
        -- Show/Read instances
        prop "show/read roundtrip" testReadShowInstance
