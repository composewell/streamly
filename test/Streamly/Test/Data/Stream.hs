-- |
-- Module      : Streamly.Test.Data.Stream
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Stream (main) where

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream

import Test.Hspec.QuickCheck
import Test.Hspec as H

moduleName :: String
moduleName = "Data.Stream"

testgroupsOf ::  Expectation
testgroupsOf =
    Stream.toList
        (Stream.groupsOf 2 Fold.sum (Stream.enumerateFromTo 1 10))
        `shouldReturn` [3::Int, 7, 11, 15, 19]

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const 1) $ do
      describe moduleName $ do
        describe "Repeated Fold" $ do
            prop "testgroupsOf" testgroupsOf
