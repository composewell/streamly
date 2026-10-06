{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
-- The JavaScript backend recompiles modules with TemplateHaskell enabled every
-- time (GHC #23013), enable it only when the instances are derived using TH.
#if defined(TEST_DERIVE_SERIALIZE) || defined(TEST_DERIVE_UNBOX)
{-# LANGUAGE TemplateHaskell #-}
#endif

-- This module has a lot of orphan instances as we are deriving it here. We can
-- ignore this warning.
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Test.Data.MutByteArray.DeriveInstances
-- Copyright   : (c) 2022 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.MutByteArray.DeriveInstances (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

#ifdef TEST_DERIVE_SERIALIZE
import Data.Foldable (forM_)
import Data.Word (Word8)
import qualified Streamly.Internal.Data.Array as Array
#endif

import GHC.Fingerprint (Fingerprint(..))

import Data.Complex (Complex ((:+)))
import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Rep)
import Data.Int (Int64)
import GHC.Real (Ratio(..))

import Streamly.Internal.Data.MutByteArray
import qualified Streamly.Internal.Data.MutByteArray as MBA

import Test.Hspec as H

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- There are three cases, one for Serialize and two for Unbox
-- 1. TEST_DERIVE_SERIALIZE defined: tests work using Serialize type class
-- 2. TEST_DERIVE_SERIALIZE NOT defined: tests work using Unbox type class
--   there are two further cases in Unbox case
--   2a. TEST_DERIVE_UNBOX defined: Unbox type class uses deriveUnbox
--   2b. TEST_DERIVE_UNBOX NOT defined: Unbox uses Generic deriving

#ifdef TEST_DERIVE_SERIALIZE

-- Tests for Serialize type class
#define TYPE_CLASS Serialize
#define MODULE_NAME "Data.Serialize.Deriving.TH"
-- All Serialize instances are derived by a single splice, see SERIALIZE_ALL.
#define DERIVE_UNBOX(typ)
#define PEEK(i, arr, sz) (deserializeAt i arr sz)
#define POKE(i, arr, val) (serializeAt i arr val)

#else

-- Tests for Unbox type class
#define TYPE_CLASS Unbox
#define PEEK(i, arr, sz) peekAtWithNextOff i arr
#define POKE(i, arr, val) pokeAtWithNextOff i arr val

#ifdef TEST_DERIVE_UNBOX

-- Derive Unbox instances using deriveUnbox (TH)
#define MODULE_NAME "Data.Unbox.Deriving.TH"
#define DERIVE_UNBOX(typ) $(deriveUnbox [d|instance Unbox typ|])

#else

-- Derive Unbox instances using Generic deriving.
#define MODULE_NAME "Data.Unbox.Deriving.Generic"
#define DERIVE_UNBOX(typ) deriving instance Unbox (typ)

#endif

#endif

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

#ifndef TEST_DERIVE_SERIALIZE

-- For the Unbox case
peekAtWithNextOff ::
       forall a. Unbox a
    => Int
    -> MutByteArray
    -> IO (Int, a)
peekAtWithNextOff i arr = do
    val <- peekAt i arr
    pure (i + sizeOf (Proxy :: Proxy a), val)

pokeAtWithNextOff ::
       forall a. Unbox a
    => Int
    -> MutByteArray
    -> a
    -> IO Int
pokeAtWithNextOff i arr val = do
    pokeAt i arr val
    pure $ i + sizeOf (Proxy :: Proxy a)

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

-- The following types don't have a Generic instance by default
deriving instance Generic (Ratio Int)
deriving instance Generic (Ratio Int64)
#if !MIN_VERSION_base(4,15,0)
deriving instance Generic (Fingerprint)
#endif

#if defined(TEST_DERIVE_SERIALIZE)
-- SERIALIZE_ALL: The JavaScript backend retains several tens of MB of linker
-- state for each splice it evaluates, one splice keeps the compilation
-- within the memory limit.
$(concat <$> mapM deriveSerialize
    [ [d|instance Serialize Unit|]
    , [d|instance Serialize UnarySum|]
    , [d|instance Serialize UnarySum2|]
    , [d|instance Serialize Unit1|]
    , [d|instance Serialize Unit2|]
    , [d|instance Serialize Unit3|]
    , [d|instance Serialize Unit4|]
    , [d|instance Serialize Single|]
    , [d|instance Serialize Product2|]
    , [d|instance Serialize SumOfProducts|]
    , [d|instance Serialize NestedSOP|]
    , [d|instance Serialize a => Serialize (Complex a)|]
    , [d|instance Serialize a => Serialize (Ratio a)|]
    , [d|instance Serialize a => Serialize (Const a b)|]
    , [d|instance Serialize a => Serialize (Identity a)|]
    ])
#endif

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

#ifdef TEST_DERIVE_SERIALIZE
variableSizeOf ::
       forall a. Serialize a
    => a
    -> Int
variableSizeOf = addSizeTo 0
#endif

testSerialization ::
       forall a. (Eq a, Show a, TYPE_CLASS a)
    => a
    -> IO ()
testSerialization val = do
    let len =
#ifdef TEST_DERIVE_SERIALIZE
               (variableSizeOf val)
#else
               (sizeOf (Proxy :: Proxy a))
#endif
    arr <- MBA.new len
    nextOff <- POKE(0, arr, val)
#ifdef TEST_DERIVE_SERIALIZE
    arr2 <- MBA.new len
    -- Re-initialize the array with random value
    forM_ [0..(len - 1)] $ \i -> POKE(i, arr2, (8 :: Word8))
    _ <- POKE(0, arr2, val)
    let slice1 = Array.Array arr 0 len :: Array.Array Word8
        slice2 = Array.Array arr2 0 len :: Array.Array Word8
    -- The serialized representation should be the same
    slice1 `shouldBe` slice2
    -- The serialized representation is not the same for "Unbox" as the Array
    -- might not be fully utilized in case of "Unbox". This is because different
    -- constructors might have different lengths.
#endif
    (nextOff1, val1) <- PEEK(0, arr, len)
    val1 `shouldBe` val
    nextOff1 `shouldBe` len
    nextOff `shouldBe` len

testGenericConsistency ::
       forall a.
       ( Eq a
       , Show a
#ifdef TEST_DERIVE_SERIALIZE
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
#ifdef TEST_DERIVE_SERIALIZE
            variableSizeOf val
#else
            sizeOf (Proxy :: Proxy a)
#endif
    len  `shouldBe` genericSizeOf (Proxy :: Proxy a)

    -- Test the serialization and deserialization
    arr <- MBA.new (sizeOf (Proxy :: Proxy a))

    nextOff <- POKE(0, arr, val)
    genericPeekByteIndex arr 0 `shouldReturn` val

    genericPokeByteIndex arr 0 val
    (nextOff1, val1) <- PEEK(0, arr, len)
    val1 `shouldBe` val

    nextOff1 `shouldBe` len
    nextOff `shouldBe` len


#ifndef TEST_DERIVE_SERIALIZE
-- Size is also implicitly tested while serializing and deserializing.
checkSizeOf :: forall a. Unbox a => Proxy a -> Int -> IO ()
checkSizeOf _ sz = sizeOf (Proxy :: Proxy a) `shouldBe` sz

-- Int is 4 bytes on 32-bit platforms, e.g. the JavaScript backend
intSize :: Int
intSize = sizeOf (Proxy :: Proxy Int)

#endif

--------------------------------------------------------------------------------
-- CPP helpers
--------------------------------------------------------------------------------

#define CHECK_SIZE(type, expectation) \
 it "checkSizeOf type" $ checkSizeOf (Proxy :: Proxy type) (expectation)

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

#ifndef TEST_DERIVE_SERIALIZE
    CHECK_SIZE(Unit, 1)
    CHECK_SIZE(Unit1, 1)
    CHECK_SIZE(Unit2, 2)
    CHECK_SIZE(Unit3, 2 * intSize + 1)
    CHECK_SIZE(Unit4, 2 * intSize + 1)
    CHECK_SIZE(UnarySum, 1)
    CHECK_SIZE(UnarySum2, 2)
    CHECK_SIZE(Single, intSize)
    CHECK_SIZE(Product2, intSize + 4)
    CHECK_SIZE(SumOfProducts, 3 * intSize + 1)
    CHECK_SIZE(NestedSOP, 3 * intSize + 2)
#endif

    it "Bool" $ testSerialization True
    it "Complex Int" $ testSerialization (5 :+ 3 :: Complex Int)
    it "Ratio Int" $ testSerialization (5 :% 3 :: Ratio Int)
    it "Const Float Int" $ testSerialization (Const 333.5678 :: Const Float Int)
    it "Identity Int" $ testSerialization (Identity 56760 :: Identity Int)

    it "GenericConsistency Bool" $ testGenericConsistency True
    -- Int64, not Int, the serialized size of Int is 8 bytes on all platforms,
    -- its size in memory is 4 bytes on 32-bit platforms.
    it "GenericConsistency (Complex Int64)"
        $ testGenericConsistency (5 :+ 3 :: Complex Int64)
    it "GenericConsistency (Ratio Int64)"
        $ testGenericConsistency (5 :% 3 :: Ratio Int64)
    it "GenericConsistency (Const Float Int)"
        $ testGenericConsistency (Const 333.5678 :: Const Float Int)
    it "GenericConsistency (Identity Int64)"
        $ testGenericConsistency (Identity 56760 :: Identity Int64)

    it "Fingerprint" $ testSerialization (Fingerprint 123456 876588)
    it "GenericConsistency Fingerprint"
        $ testGenericConsistency (Fingerprint 123456 876588)

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

moduleName :: String
moduleName = MODULE_NAME

main :: IO ()
main = hspec $ H.parallel $ describe moduleName $ do
    testCases
