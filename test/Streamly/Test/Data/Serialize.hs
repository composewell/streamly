{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- Required for Eq instance declaration of HigherOrderType
{-# LANGUAGE UndecidableInstances #-}

-- We are generating an orphan instance of Serialize for Identity.
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

import System.Random (randomRIO)
import Streamly.Internal.Data.Unbox (MutableByteArray, newBytes)
import GHC.Generics (Generic)
import Streamly.Data.Serialize (Serialize)
import Streamly.Test.Data.Serialize.TH (genDatatype)

import qualified Streamly.Internal.Data.Serialize.TH as Serialize

import Data.Functor.Identity (Identity (..))

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Serialize as Serialize

import qualified Streamly.Test.Data.Serialize.CompatV0 as CompatV0
import qualified Streamly.Test.Data.Serialize.CompatV1 as CompatV1

import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Hspec as H

--------------------------------------------------------------------------------
-- Serialize configuration
--------------------------------------------------------------------------------

#ifdef ENABLE_constructorTagAsString
#define CONF_NAME  "ENABLE_constructorTagAsString"
#define CONF (Serialize.defaultConfig {Serialize.constructorTagAsString = True})
#else
#define CONF_NAME  "DEFAULT"
#define CONF Serialize.defaultConfig
#endif

--------------------------------------------------------------------------------
-- Edge case types
--------------------------------------------------------------------------------

data Unit =
    Unit
    deriving (Eq, Show)
$(Serialize.deriveSerializeWith CONF [d|instance Serialize Unit|])

data The =
    The Unit Int Char
    deriving (Eq, Show)
$(Serialize.deriveSerializeWith CONF [d|instance Serialize The|])

--------------------------------------------------------------------------------
-- Generated types
--------------------------------------------------------------------------------

$(genDatatype "CustomDatatype" 15)
$(Serialize.deriveSerializeWith CONF [d|instance Serialize CustomDatatype|])

--------------------------------------------------------------------------------
-- Types with functional parameters
--------------------------------------------------------------------------------

data HigherOrderType f =
    HigherOrderType
        { field0 :: f Int
        , field1 :: f Char
        }

instance (Eq (f Int), Eq (f Char)) => Eq (HigherOrderType f) where
    (==) a b = (field0 a == field0 b) && (field1 a == field1 b)

instance (Show (f Int), Show (f Char)) => Show (HigherOrderType f) where
    show a = "HigherOrderType " ++ show (field0 a) ++ " " ++ show (field1 a)

$(Serialize.deriveSerializeWith CONF
      [d|instance Serialize a => Serialize (Identity a)|])
$(Serialize.deriveSerializeWith CONF
      [d|instance Serialize (HigherOrderType Identity)|])

--------------------------------------------------------------------------------
-- Recursive type
--------------------------------------------------------------------------------

-- Recursive ADT
data BinTree a
  = Tree (BinTree a) (BinTree a)
  | Leaf a
  deriving (Show, Read, Eq, Generic)

$(Serialize.deriveSerializeWith
      CONF
      [d|instance Serialize a => Serialize (BinTree a)|])

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = oneof [Leaf <$> arbitrary, Tree <$> arbitrary <*> arbitrary]

--------------------------------------------------------------------------------
-- Record syntax type
--------------------------------------------------------------------------------

upgradeRec :: (a -> b) -> CompatV0.Rec a -> CompatV1.Rec b
upgradeRec f val =
    CompatV1.Rec
        { CompatV1.initialField = CompatV0.initialField val
        , CompatV1.otherField = f (CompatV0.otherField val)
        , CompatV1.theLastField = CompatV0.theLastField val
        , CompatV1.aNewField = Nothing
        }

upgradeRiver :: CompatV0.River -> CompatV1.River
upgradeRiver = read . show

downgradeRec  :: (a -> b) -> CompatV1.Rec a -> CompatV0.Rec b
downgradeRec f val =
    CompatV0.Rec
        { CompatV0.initialField = CompatV1.initialField val
        , CompatV0.otherField = f (CompatV1.otherField val)
        , CompatV0.theLastField = CompatV1.theLastField val
        }

downgradeRiver :: CompatV1.River -> CompatV0.River
downgradeRiver = read . show

testCompatibility ::
       CompatV0.Rec (CompatV0.Rec CompatV0.River)
    -> CompatV1.Rec (CompatV1.Rec CompatV1.River)
    -> IO ()
testCompatibility v0 v1 = do
    let upgradedV0 = upgradeRec (upgradeRec upgradeRiver) v0
        downgradedV1 = downgradeRec (downgradeRec downgradeRiver) v1

    res <- poke v0
    peekAndVerify res upgradedV0

    res1 <- poke v1
    peekAndVerify res1 downgradedV1

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

poke ::
       forall a. Serialize.Serialize a
    => a
    -> IO (MutableByteArray, Int, Int)
poke val = do
    let sz = Serialize.size 0 val

    let excessSize = 100
    randomOff <- randomRIO (10, excessSize)
    -- Use a proper slice to test instead of the array directly. This will catch
    -- any hardcoded 0 offsets
    let arrSize = sz + excessSize
        serStartOff = randomOff
        serEndOff = randomOff + sz
    arr <- newBytes arrSize

    off1 <- Serialize.serialize serStartOff arr val
    off1 `shouldBe` serEndOff
    pure (arr, serStartOff, serEndOff)

peekAndVerify ::
       forall a. (Eq a, Show a, Serialize.Serialize a)
    => (MutableByteArray, Int, Int)
    -> a
    -> IO ()
peekAndVerify (arr, serStartOff, serEndOff) val = do
    (off2, val2) <- Serialize.deserialize serStartOff arr serEndOff
    val2 `shouldBe` val
    off2 `shouldBe` serEndOff
    let slice = Array.Array arr serStartOff serEndOff
    val `shouldBe` Serialize.decode slice
    clonedSlice <- Array.clone slice
    val `shouldBe` Serialize.decode clonedSlice

roundtrip
    :: forall a. (Eq a, Show a, Serialize.Serialize a)
    => a
    -> IO ()
roundtrip val = do

    val `shouldBe` Serialize.decode (Serialize.encode val)

    res <- poke val
    peekAndVerify res val

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

    describe "Edge Cases" $ do
        it "Unit" $ roundtrip Unit
        it "The" $ roundtrip $ The Unit 1 'a'

    it "HigherOrderType"
        $ roundtrip $ HigherOrderType (Identity 5) (Identity 'e')

    prop "Integer"
        $ \(x :: Integer) -> roundtrip x

    prop "Array Int"
        $ \(x :: [Int]) -> roundtrip (Array.fromList x)

    prop "Compatible Record"
        $ \(a :: CompatV1.Rec (CompatV0.Rec CompatV1.River)) -> roundtrip a

    prop "Compatibility"
        $ \a b -> testCompatibility a b

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
moduleName = "Data.Serialize." ++ CONF_NAME

main :: IO ()
main = hspec $ H.parallel $ describe moduleName testCases
