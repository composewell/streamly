{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

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
import GHC.Generics (Generic)
import Streamly.Test.Data.Serialize.TH (genDatatype)
import Streamly.Internal.Data.Serialize.TH (deriveSerialize)
import Streamly.Internal.Data.Serialize (VLWord64)

import qualified Streamly.Internal.Data.Serialize as Serialize

import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Hspec as H

--------------------------------------------------------------------------------
-- Generated types
--------------------------------------------------------------------------------

$(genDatatype "CustomDatatype" 15)
$(deriveSerialize ''CustomDatatype)

--------------------------------------------------------------------------------
-- Recursive type
--------------------------------------------------------------------------------

-- Recursive ADT
data BinTree a
  = Tree (BinTree a) (BinTree a)
  | Leaf a
  deriving (Show, Read, Eq, Generic)

$(deriveSerialize ''BinTree)

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = oneof [Leaf <$> arbitrary, Tree <$> arbitrary <*> arbitrary]

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

roundtrip
    :: forall a. (Eq a, Show a, Serialize.Serialize a)
    => a
    -> IO ()
roundtrip val = do

    let sz = Serialize.size 0 val

    -- putStrLn "----------------------------------------------------------------"
    -- putStrLn $ show val
    -- putStrLn $ "Size is: " ++ show sz
    -- putStrLn "----------------------------------------------------------------"
    arr <- newBytes sz

    off1 <- Serialize.serialize 0 arr val
    (off2, val2) <- Serialize.deserialize 0 arr sz
    val2 `shouldBe` val
    off2 `shouldBe` off1
    off2 `shouldBe` sz

testSerializeList
    :: forall a. (Eq a, Show a, Serialize.Serialize a)
    => Int
    -> a
    -> IO ()
testSerializeList sizeOfA val = do

    let sz = Serialize.size 0 val

    sz `shouldBe` sizeOfA

    roundtrip val

--------------------------------------------------------------------------------
-- VLWord64 helper
--------------------------------------------------------------------------------

#define VLWord64Test(LOW, UP, EXP_SZ) prop "[LOW, UP]" \
     $ forAll (chooseBoundedIntegral (LOW, UP)) \
     $ \(x :: VLWord64) -> roundtrip x >> (Serialize.size 0 x `shouldBe` EXP_SZ)

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

    describe "VLWord64" $ do
        VLWord64Test(0,240,1)
        VLWord64Test(241,2287,2)
        VLWord64Test(2288,67823,3)
        VLWord64Test(67824,16777215,4)
        VLWord64Test(16777216,4294967295,5)
        VLWord64Test(4294967296,1099511627775,6)
        VLWord64Test(1099511627776,281474976710655,7)
        VLWord64Test(281474976710656,72057594037927935,8)
        VLWord64Test(72057594037927936,maxBound,9)

    limitQC
        $ prop "CustomDatatype"
        $ \(x :: CustomDatatype) -> roundtrip x

    limitQC
        $ prop "[CustomDatatype]"
        $ \(x :: [CustomDatatype]) -> roundtrip x

    limitQC
        $ prop "BinTree" $ \(x :: BinTree Int) -> roundtrip x

    where

    limitQC = modifyMaxSize (const 50)

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Serialize"

main :: IO ()
main = hspec $ H.parallel $ describe moduleName testCases
