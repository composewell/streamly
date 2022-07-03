-- |
-- Module      : Streamly.Test.Data.Array
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Array (main) where

#include "Streamly/Test/Data/Array/CommonImports.hs"

import Streamly.Test.Common (listEquals, checkListEqual)
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Array.Tree.Type as T
import qualified Streamly.Internal.Data.Stream.IsStream.Eliminate as S
type Array = A.Array

moduleName :: String
moduleName = "Data.Array"

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

testTreeAppendCount :: Property
testTreeAppendCount =
    forAll (choose (0, maxArrLen)) $ \len ->
      forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
        monadicIO $ do
          arr <- S.fold (T.appendN T.newArray $ length list) $ S.fromList list
          assert (T.count arr == length list)

testTreeAppendRead :: Property
testTreeAppendRead =
  forAll (choose (0, 7)) $ \len ->
    forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
      monadicIO $ do
        arr <- S.fold (T.appendN T.newArray $ length list) $ S.fromList list
        readArr <- S.toList $ S.unfold T.read arr
        assert (readArr == list)

testLengthFromStream :: Property
testLengthFromStream = genericTestFrom (const A.fromStream)

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
      describe moduleName $ do
        commonMain
        describe "Construction" $ do
            prop "length . fromStream === n" testLengthFromStream
            prop "toStream . fromStream === id" testFromStreamToStream
            prop "read . write === id" testFoldUnfold
            prop "fromList" testFromList
            prop "testTreeAppendCount" testTreeAppendCount
            prop "testTreeAppendRead" testTreeAppendRead
