-- |
-- Module      : Streamly.Test.Data.SmallArray
-- Copyright   : (c) 2020 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.SmallArray (main) where

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import Test.Hspec as H

import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.System.IO (defaultChunkSize)
import Streamly.Test.Common (listEquals)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as S

import qualified Streamly.Internal.Data.SmallArray as A
type Array = A.SmallArray

moduleName :: String
moduleName = "Data.SmallArray"

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

#include "Streamly/Test/Data/Array/TypeCommon.hs"

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
      describe moduleName $ do
        typeCommon
