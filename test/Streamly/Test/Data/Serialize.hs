{-# LANGUAGE TemplateHaskell #-}

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

    let sz =
          case Serialize.size :: Serialize.Size a of
              Serialize.Size f -> f 0 val

    -- putStrLn "----------------------------------------------------------------"
    -- putStrLn $ show val
    -- putStrLn $ "Size is: " ++ show sz
    -- putStrLn "----------------------------------------------------------------"
    arr <- newBytes sz

    off1 <- Serialize.serialize 0 arr val
    (off2, val2) <- Serialize.deserialize 0 arr sz
    val2 `shouldBe` val
    off2 `shouldBe` off1

testSerializeList
    :: forall a. (Eq a, Show a, Serialize.Serialize a)
    => Int
    -> a
    -> IO ()
testSerializeList sizeOfA val = do

    let sz =
          case Serialize.size :: Serialize.Size a of
              Serialize.Size f -> f 0 val

    sz `shouldBe` sizeOfA

    roundtrip val

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
