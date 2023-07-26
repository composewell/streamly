{-# LANGUAGE StandaloneDeriving, DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- |
-- Module      : Streamly.Test.Data.Unbox
-- Copyright   : (c) 2022 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Unbox (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Complex (Complex ((:+)))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Rep(..))
import GHC.Real (Ratio(..))
import Streamly.Internal.Data.Unbox
    ( PeekRep(..)
    , PokeRep(..)
    , SizeOfRep(..)
    , Unbox(..)
    , genericPeekByteIndex
    , genericPokeByteIndex
    , genericSizeOf
    , newBytes
    , pokeByteIndex
    )

import Test.Hspec as H

import qualified Streamly.Internal.Data.Serialize as Serialize

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Unit instance uses a hack, so test all cases
data Unit =
    Unit
    deriving (Unbox, Show, Generic, Eq)

data UnarySum
    = Sum1
    | Sum2
    deriving (Unbox, Show, Generic, Eq)

data UnarySum2
    = UnitSum1 Unit
    | UnitSum2 Unit
    deriving (Generic, Unbox, Eq, Show)

data Unit1 =
    Unit1 Unit
    deriving (Generic, Unbox, Eq, Show)

data Unit2 =
    Unit2 Unit Unit
    deriving (Generic, Unbox, Eq, Show)

data Unit3 =
    Unit3 Int Unit Int
    deriving (Generic, Unbox, Eq, Show)

data Unit4 =
    Unit4 Int Unit1 Int
    deriving (Generic, Unbox, Eq, Show)

{-# ANN Single "HLint: ignore" #-}
data Single =
    Single Int
    deriving (Unbox, Show, Generic, Eq)

data Product2 =
    Product2 Int Char
    deriving (Unbox, Show, Generic, Eq)

data SumOfProducts
    = SOP0
    | SOP1 Int
    | SOP2 Int Char
    | SOP3 Int Int Int
    deriving (Unbox, Show, Generic, Eq)

data NestedSOP
    = NSOP0 SumOfProducts
    | NSOP1 SumOfProducts
    deriving (Unbox, Show, Generic, Eq)

--------------------------------------------------------------------------------
-- Standalone derivations
--------------------------------------------------------------------------------

-- Ratio does not have a Generic instance by default
deriving instance Generic (Ratio Int)

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

testSerialization ::
       forall a. (Eq a, Show a, Unbox a)
    => a
    -> IO ()
testSerialization val = do
    arr <- newBytes (sizeOf (Proxy :: Proxy a))
    pokeByteIndex 0 arr val
    peekByteIndex 0 arr `shouldReturn` val

testGenericConsistency ::
       forall a.
       ( Eq a
       , Show a
       , Unbox a
       , Generic a
       , SizeOfRep (Rep a)
       , PeekRep (Rep a)
       , PokeRep (Rep a)
       )
    => a
    -> IO ()
testGenericConsistency val = do

    -- Test the generic sizeOf
    sizeOf (Proxy :: Proxy a) `shouldBe` genericSizeOf (Proxy :: Proxy a)

    -- Test the serialization and deserialization
    arr <- newBytes (sizeOf (Proxy :: Proxy a))

    pokeByteIndex 0 arr val
    genericPeekByteIndex arr 0 `shouldReturn` val

    genericPokeByteIndex arr 0 val
    peekByteIndex 0 arr `shouldReturn` val

-- Size is also implicitly tested while serializing and deserializing.
checkSizeOf :: forall a. Unbox a => Proxy a -> Int -> IO ()
checkSizeOf _ size = sizeOf (Proxy :: Proxy a) `shouldBe` size

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
-- CPP helpers
--------------------------------------------------------------------------------

#define CHECK_SIZE(type, expectation) \
 it "checkSizeOf type" $ checkSizeOf (Proxy :: Proxy type) expectation

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testCases :: Spec
testCases = do
    it "Unit" $ testSerialization Unit
    it "Unit1" $ testSerialization (Unit1 Unit)
    it "Unit2" $ testSerialization (Unit2 Unit Unit)
    it "Unit3" $ testSerialization (Unit3 1234 Unit 4567)
    it "Unit4" $ testSerialization (Unit4 1234 (Unit1 Unit) 4567)
    it "UnarySum Sum1" $ testSerialization Sum1
    it "UnarySum Sum2" $ testSerialization Sum2
    it "UnarySum2 UnitSum1" $ testSerialization (UnitSum1 Unit)
    it "UnarySum2 UnitSum2" $ testSerialization (UnitSum2 Unit)
    it "Single" $ testSerialization (Single 2)
    it "Product2" $ testSerialization (Product2 2 'b')
    it "SumOfProducts SOP0" $ testSerialization SOP0
    it "SumOfProducts SOP1" $ testSerialization (SOP1 1)
    it "SumOfProducts SOP2" $ testSerialization (SOP2 1 'a')
    it "SumOfProducts SOP3" $ testSerialization (SOP3 1 2 3)

    CHECK_SIZE(Unit, 1)
    CHECK_SIZE(Unit1, 1)
    CHECK_SIZE(Unit2, 2)
    CHECK_SIZE(Unit3, 17)
    CHECK_SIZE(Unit4, 17)
    CHECK_SIZE(UnarySum, 1)
    CHECK_SIZE(UnarySum2, 2)
    CHECK_SIZE(Single, 8)
    CHECK_SIZE(Product2, 12)
    CHECK_SIZE(SumOfProducts, 25)
    CHECK_SIZE(NestedSOP, 26)

    it "Bool" $ testSerialization True
    it "Complex Int" $ testSerialization (5 :+ 3 :: Complex Int)
    it "Ratio Int" $ testSerialization (5 :% 3 :: Ratio Int)
    it "Const Float Int" $ testSerialization (Const 333.5678 :: Const Float Int)
    it "Identity Int" $ testSerialization (Identity 56760 :: Identity Int)

    it "GenericConsistency Bool" $ testGenericConsistency True
    it "GenericConsistency (Complex Int)"
        $ testGenericConsistency (5 :+ 3 :: Complex Int)
    it "GenericConsistency (Ratio Int)"
        $ testGenericConsistency (5 :% 3 :: Ratio Int)
    it "GenericConsistency (Const Float Int)"
        $ testGenericConsistency (Const 333.5678 :: Const Float Int)
    it "GenericConsistency (Identity Int)"
        $ testGenericConsistency (Identity 56760 :: Identity Int)

    it "Serialize [Int]"
        $ testSerializeList (8 + 4 * 8) ([1, 2, 3, 4] :: [Int])
    it "Serialize [[Int]]"
        $ testSerializeList
              (8 + 3 * 8 + 6 * 8)
              ([[1], [1, 2], [1, 2, 3]] :: [[Int]])

    -- Fingerprint does not work for GHC 8.6.5
    -- it "Fingerprint" $ testSerialization (Fingerprint 123456 876588)
    -- it "GenericConsistency Fingerprint"
    --     $ testGenericConsistency (Fingerprint 123456 876588)

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unboxed"

main :: IO ()
main = hspec $ H.parallel $ describe moduleName testCases
