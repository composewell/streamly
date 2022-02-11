-- |
-- Module      : Streamly.Test.Data.Array.Prim.Pinned
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Array.Prim.Pinned (main) where

#include "Streamly/Test/Data/Array/CommonImports.hs"

import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array.Prim as A
import qualified Streamly.Internal.Data.Array.Prim.Type as A
type Array = A.Array

moduleName :: String
moduleName = "Data.Array.Prim.Pinned"

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
            prop "foldMany with writeNUnsafe concats to original"
                (foldManyWith (\n -> Fold.take n (A.writeNUnsafe n)))
