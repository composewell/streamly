{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- This module has a lot of orphan instances as we are deriving it here. We can
-- ignore this warning.
{-# OPTIONS_GHC -Wno-orphans #-}

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

#ifdef USE_SERIALIZE
import Control.Monad (void)
#endif
import Data.Complex (Complex ((:+)))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Rep(..))
import GHC.Real (Ratio(..))

#ifdef USE_SERIALIZE

import Streamly.Internal.Data.Serialize (Serialize(..), Size(..))
import Streamly.Internal.Data.Serialize.TH

#else

#ifdef USE_TH
import Streamly.Internal.Data.Unbox.TH
#endif

#endif

import Streamly.Internal.Data.Unbox
    ( PeekRep(..)
    , PokeRep(..)
    , SizeOfRep(..)
    , Unbox(..)
    , genericPeekByteIndex
    , genericPokeByteIndex
    , genericSizeOf
    , newBytes
    )

import Test.Hspec as H

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

#ifdef USE_SERIALIZE

#define MODULE_NAME "Data.Serialize.Deriving.TH"
#define DERIVE_UNBOX(typ) $(deriveSerialize ''typ)
#define PEEK(i, arr, sz) fmap snd (deserialize i arr sz)
#define POKE(i, arr, val) void (serialize i arr val)
#define TYPE_CLASS Serialize

#else

#define PEEK(i, arr, sz) peekByteIndex i arr
#define POKE(i, arr, val) pokeByteIndex i arr val
#define TYPE_CLASS Unbox

#ifdef USE_TH

#define MODULE_NAME "Data.Unbox.Deriving.TH"
#define DERIVE_UNBOX(typ) $(deriveUnbox ''typ)

#else

#define MODULE_NAME "Data.Unbox.Deriving.Generic"
#define DERIVE_UNBOX(typ) deriving instance Unbox (typ)

#endif

#endif

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Unit instance uses a hack, so test all cases
data Unit =
    Unit
    deriving (Show, Generic, Eq)
DERIVE_UNBOX(Unit)

data UnarySum
    = Sum1
    | Sum2
    deriving (Show, Generic, Eq)
DERIVE_UNBOX(UnarySum)


data UnarySum2
    = UnitSum1 Unit
    | UnitSum2 Unit
    deriving (Generic, Eq, Show)
DERIVE_UNBOX(UnarySum2)

data Unit1 =
    Unit1 Unit
    deriving (Generic, Eq, Show)
DERIVE_UNBOX(Unit1)

data Unit2 =
    Unit2 Unit Unit
    deriving (Generic, Eq, Show)
DERIVE_UNBOX(Unit2)

data Unit3 =
    Unit3 Int Unit Int
    deriving (Generic, Eq, Show)
DERIVE_UNBOX(Unit3)

data Unit4 =
    Unit4 Int Unit1 Int
    deriving (Generic,  Eq, Show)
DERIVE_UNBOX(Unit4)

{-# ANN Single "HLint: ignore" #-}
data Single =
    Single Int
    deriving (Show, Generic, Eq)
DERIVE_UNBOX(Single)

data Product2 =
    Product2 Int Char
    deriving (Show, Generic, Eq)
DERIVE_UNBOX(Product2)

data SumOfProducts
    = SOP0
    | SOP1 Int
    | SOP2 Int Char
    | SOP3 Int Int Int
    deriving (Show, Generic, Eq)
DERIVE_UNBOX(SumOfProducts)

data NestedSOP
    = NSOP0 SumOfProducts
    | NSOP1 SumOfProducts
    deriving (Show, Generic, Eq)
DERIVE_UNBOX(NestedSOP)

--------------------------------------------------------------------------------
-- Standalone derivations
--------------------------------------------------------------------------------

-- Ratio does not have a Generic instance by default
deriving instance Generic (Ratio Int)

#if defined(USE_SERIALIZE)
$(deriveSerialize ''Complex)
$(deriveSerialize ''Ratio)
$(deriveSerializeWith ["a"] ''Const)
$(deriveSerialize ''Identity)
#endif

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

#ifdef USE_SERIALIZE
variableSizeOf ::
       forall a. Serialize a
    => a
    -> Int
variableSizeOf val =
    case size :: Size a of
        Size f -> f 0 val
#endif

testSerialization ::
       forall a. (Eq a, Show a, TYPE_CLASS a)
    => a
    -> IO ()
testSerialization val = do
    let len =
#ifdef USE_SERIALIZE
               (variableSizeOf val)
#else
               (sizeOf (Proxy :: Proxy a))
#endif
    arr <- newBytes len
    POKE(0, arr, val)
    PEEK(0, arr, len) `shouldReturn` val

testGenericConsistency ::
       forall a.
       ( Eq a
       , Show a
#ifdef USE_SERIALIZE
       , Serialize a
#endif
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
    let len =
#ifdef USE_SERIALIZE
            variableSizeOf val
#else
            sizeOf (Proxy :: Proxy a)
#endif
    len  `shouldBe` genericSizeOf (Proxy :: Proxy a)

    -- Test the serialization and deserialization
    arr <- newBytes (sizeOf (Proxy :: Proxy a))

    POKE(0, arr, val)
    genericPeekByteIndex arr 0 `shouldReturn` val

    genericPokeByteIndex arr 0 val
    PEEK(0, arr, len) `shouldReturn` val


#ifndef USE_SERIALIZE
-- Size is also implicitly tested while serializing and deserializing.
checkSizeOf :: forall a. Unbox a => Proxy a -> Int -> IO ()
checkSizeOf _ size = sizeOf (Proxy :: Proxy a) `shouldBe` size

#endif

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

#ifndef USE_SERIALIZE
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
#endif

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

    -- Fingerprint does not work for GHC 8.6.5
    -- it "Fingerprint" $ testSerialization (Fingerprint 123456 876588)
    -- it "GenericConsistency Fingerprint"
    --     $ testGenericConsistency (Fingerprint 123456 876588)

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

moduleName :: String
moduleName = MODULE_NAME

main :: IO ()
main = hspec $ H.parallel $ describe moduleName testCases
