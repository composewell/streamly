-- |
-- Module      : Streamly.Test.Data.Serialize
-- Copyright   : (c) 2022 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Serialize (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Internal.Data.Unbox (newBytes)

import Test.Hspec as H

import qualified Streamly.Internal.Data.Serialize as Serialize

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

testSerializeList
    :: forall a. (Eq a, Show a, Serialize.Serialize a)
    => Int
    -> a
    -> IO ()
testSerializeList sizeOfA val = do

    let sz =
          case Serialize.size :: Serialize.Size a of
              Serialize.VarSize f -> f val
              Serialize.ConstSize csz -> csz

    sz `shouldBe` sizeOfA

    arr <- newBytes sz

    off1 <- Serialize.serialize 0 arr val
    (off2, val2) <- Serialize.deserialize 0 arr
    val2 `shouldBe` val
    off2 `shouldBe` off1

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testCases :: Spec
testCases = do

    it "Serialize [Int]"
        $ testSerializeList (8 + 4 * 8) ([1, 2, 3, 4] :: [Int])
    it "Serialize [[Int]]"
        $ testSerializeList
              (8 + 3 * 8 + 6 * 8)
              ([[1], [1, 2], [1, 2, 3]] :: [[Int]])

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unboxed"

main :: IO ()
main = hspec $ H.parallel $ describe moduleName testCases
