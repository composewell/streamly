
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
import qualified Streamly.Internal.Data.Array.Generic as Arr
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream

type Array = A.Array

moduleName :: String
moduleName = "Data.Array"

#include "Streamly/Test/Data/MutArray/Common.hs"
#include "Streamly/Test/Data/Array/TypeCommon.hs"

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
      describe moduleName $ do
        arrayCommon
        typeCommon
        -- IMPORTANT NOTE: Before adding any test here first consider if it can
        -- be added to the Array/Common test module. Only those tests which are
        -- specific to the Generic Array module and do not apply to the Unboxed
        -- Array module should be added here.
