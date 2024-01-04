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

import Data.Foldable (forM_)
import Data.Word (Word8)
import System.Random (randomRIO)
import Streamly.Internal.Data.MutByteArray (MutByteArray)
import GHC.Generics (Generic)
import Streamly.Data.MutByteArray (Serialize)
import Streamly.Test.Data.Serialize.TH (genDatatype)

import Data.Functor.Identity (Identity (..))

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutByteArray as Serialize

import qualified Streamly.Test.Data.Serialize.CompatV0 as CompatV0
import qualified Streamly.Test.Data.Serialize.CompatV1 as CompatV1

import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Hspec as H

--------------------------------------------------------------------------------
-- Serialize configuration
--------------------------------------------------------------------------------

#ifdef ENABLE_constructorTagAsString
#define CONF_NAME "ENABLE_constructorTagAsString"
#define CONF (Serialize.encodeConstrNames True)
#else
#define CONF_NAME "DEFAULT"
#define CONF id
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

-- XXX This may not terminate, or could become really large.
instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = oneof [Leaf <$> arbitrary, Tree <$> arbitrary <*> arbitrary]

-- Make a balanced tree of given level
mkBinTree :: (Arbitrary a) => Int -> IO (BinTree a)
mkBinTree = go (generate arbitrary)

    where

    go r 0 = Leaf <$> r
    go r n = Tree <$> go r (n - 1) <*> go r (n - 1)

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
    -> IO (MutByteArray, Int, Int)
poke val = do
    let sz = Serialize.addSizeTo 0 val

    let excessSize = 100
    randomOff <- randomRIO (10, excessSize)
    -- Use a proper slice to test instead of the array directly. This will catch
    -- any hardcoded 0 offsets
    let arrSize = sz + excessSize
        serStartOff = randomOff
        serEndOff = randomOff + sz
    arr <- Serialize.new arrSize
    arr2 <- Serialize.new arrSize
    -- Re-initialize the array with random value
    forM_ [0..(arrSize - 1)] $ \i -> Serialize.pokeAt i arr2 (8 :: Word8)

    off1 <- Serialize.serializeAt serStartOff arr val
    off2 <- Serialize.serializeAt 0 arr2 val

    let slice1 = Array.Array arr serStartOff off1 :: Array.Array Word8
        slice2 = Array.Array arr2 0 off2 :: Array.Array Word8
    -- The serialized representation should be the same
    slice1 `shouldBe` slice2

    off1 `shouldBe` serEndOff
    pure (arr, serStartOff, serEndOff)

peekAndVerify ::
       forall a. (Eq a, Show a, Serialize.Serialize a)
    => (MutByteArray, Int, Int)
    -> a
    -> IO ()
peekAndVerify (arr, serStartOff, serEndOff) val = do
    (off2, val2) <- Serialize.deserializeAt serStartOff arr serEndOff
    val2 `shouldBe` val
    off2 `shouldBe` serEndOff
    let slice = Array.Array arr serStartOff serEndOff
    val `shouldBe` Array.deserialize slice
    clonedSlice <- Array.clone slice
    val `shouldBe` Array.deserialize clonedSlice

roundtrip
    :: forall a. (Eq a, Show a, Serialize.Serialize a)
    => a
    -> IO ()
roundtrip val = do

    -- For debugging large size generated by arbitrary
    -- let sz = Serialize.addSizeTo 0 val
    -- putStrLn $ "Size is: " ++ show sz

    val `shouldBe` Array.deserialize (Array.pinnedSerialize val)

    res <- poke val
    peekAndVerify res val

testSerializeList
    :: forall a. (Eq a, Show a, Serialize.Serialize a)
    => Int
    -> a
    -> IO ()
testSerializeList sizeOfA val = do

    let sz = Serialize.addSizeTo 0 val

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

    prop "([Integer], [Int])"
        $ \(x :: ([Integer], [Int])) -> roundtrip x

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
        $ prop "BinTree"
        $ forAll (elements [1..15])
            (\(x :: Int) -> do
                (r :: BinTree Int) <- mkBinTree x
                roundtrip r
            )

    where

    limitQC = modifyMaxSize (const 50)

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Serialize." ++ CONF_NAME

main :: IO ()
main = hspec $ H.parallel $ describe moduleName testCases
