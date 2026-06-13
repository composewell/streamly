
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
        commonMain
        -- Show/Read instances
        prop "show/read roundtrip" testReadShowInstance
