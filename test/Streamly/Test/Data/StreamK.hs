-- |
-- Module      : Streamly.Test.Data.StreamK
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.StreamK (main) where

import Data.List (sort)
import Streamly.Internal.Data.Stream (Stream)
import Test.QuickCheck (Property, forAll, listOf)
import Test.QuickCheck.Monadic (assert, monadicIO)

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.StreamK as StreamK

import Test.Hspec as H
import Test.Hspec.QuickCheck

import Streamly.Test.Common (chooseInt)

toList :: Monad m => Stream m a -> m [a]
toList = Stream.toList

max_length :: Int
max_length = 1000

sortBy :: Property
sortBy =
    forAll (listOf (chooseInt (0, max_length)))
        $ \lst -> monadicIO $ do
            let s1 = sort lst
            s2 <- toList
                $ StreamK.toStream
                    ( StreamK.sortBy compare
                    $ StreamK.fromStream
                    $ Stream.fromList lst
                    )
            assert $ s1 == s2

moduleName :: String
moduleName = "Data.StreamK"

main :: IO ()
main = hspec
  $ H.parallel
#ifdef COVERAGE_BUILD
  $ modifyMaxSuccess (const 10)
#endif
  $ describe moduleName $ do
    -- Just some basic sanity tests for now
    let input = [[1,1] :: [Int],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7],[8,8]]
        mustBe g inp out =
            Stream.toList (StreamK.toStream (StreamK.mergeMapWith g StreamK.fromList (StreamK.fromList inp)))
                `shouldReturn` out
     in do
        it "concatPairsWith serial"
            $ mustBe StreamK.append input [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8]
        it "concatPairsWith wSerial"
            $ mustBe StreamK.interleave input [1,5,3,7,2,6,4,8,1,5,3,7,2,6,4,8]
        it "concatPairsWith mergeBy sorted"
            $ mustBe
                (StreamK.mergeBy compare) input [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8]
        it "concatPairsWith mergeBy reversed"
            $ mustBe
                (StreamK.mergeBy compare)
                (reverse input)
                [1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8]
        prop "sortBy" sortBy
