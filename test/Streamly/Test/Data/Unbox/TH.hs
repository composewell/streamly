{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Test.Data.Unbox.TH
-- Copyright   : (c) 2022 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Unbox.TH (main) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (void)
import Data.Complex (Complex)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
#if MIN_VERSION_base(4,14,0)
import Data.Ord (Down)
#endif
import Data.Proxy (Proxy(..))
import Foreign.Ptr (FunPtr, IntPtr, Ptr, WordPtr)
import GHC.Fingerprint (Fingerprint)
import GHC.Int (Int8, Int16, Int32, Int64)
#if MIN_VERSION_base(4,15,0)
import GHC.IO.SubSystem (IoSubSystem)
#endif
import GHC.Real (Ratio)
import GHC.Stable (StablePtr)
import GHC.Word (Word8, Word16, Word32, Word64)
import Streamly.Internal.Data.MutByteArray
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.MutByteArray as MBA

import Test.Hspec as H

--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

testSerialization ::
       forall a. (Eq a, Show a, Unbox a)
    => a
    -> IO ()
testSerialization val = do
    arr <- MBA.new (sizeOf (Proxy :: Proxy a))
    pokeAt 0 arr val
    peekAt 0 arr `shouldReturn` val

-- Size is also implicitly tested while serializing and deserializing.
checkSizeOf :: forall a. Unbox a => Proxy a -> Int -> IO ()
checkSizeOf _ sz = sizeOf (Proxy :: Proxy a) `shouldBe` sz

checkSizeOfNew :: forall a. Unbox a => String -> Proxy a -> Int -> Spec
checkSizeOfNew tag proxy expectation =
    it ("checkSizeOf " ++ tag) $ checkSizeOf proxy expectation

data CustomDataType1 =
    CustomDataType1
    deriving (Show, Eq)

data CustomDataType2
    = CDT2Constructor1
    | CDT2Constructor2
    | CDT2Constructor3
    deriving (Show, Eq)

data CustomDataType3 a b c
    = CDT3Constructor1 a
    | CDT3Constructor2 a b
    | CDT3Constructor3 a b c
    deriving (Show, Eq)

data CustomDataType4 a b
    = CDT4Constructor1
    | CDT4Constructor2 Bool
    | CDT4Constructor3 Bool b
    deriving (Show, Eq)

$(deriveUnbox [d|instance Unbox CustomDataType1|])
$(deriveUnbox [d|instance Unbox CustomDataType2|])
$(deriveUnbox
    [d|instance (Unbox a, Unbox b, Unbox c) => Unbox (CustomDataType3 a b c)|])
$(deriveUnbox [d|instance Unbox b => Unbox (CustomDataType4 a b)|])

--------------------------------------------------------------------------------
-- CPP helpers
--------------------------------------------------------------------------------

#define CHECK_SIZE(type, expectation) \
 it "checkSizeOf type" $ checkSizeOf (Proxy :: Proxy type) expectation

-- Unbox instance existence tests via MutArray creation
#define TEST_IE(_type) it "_type" $ testIE ([] :: [_type])

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testCustomDatatype1TH :: Spec
testCustomDatatype1TH = do
    it "CustomDataType1" $ testSerialization CustomDataType1

    CHECK_SIZE(CustomDataType1, 1)

testCustomDatatype2TH :: Spec
testCustomDatatype2TH = do
    it "CustomDataType2 1" $ testSerialization CDT2Constructor1
    it "CustomDataType2 2" $ testSerialization CDT2Constructor2
    it "CustomDataType2 3" $ testSerialization CDT2Constructor3

    CHECK_SIZE(CustomDataType2, 1)

testCustomDatatype3TH :: Spec
testCustomDatatype3TH = do
    it "CustomDataType3 1"
           $ testSerialization
                 (CDT3Constructor1 3 :: CustomDataType3 Int Bool Double)
    it "CustomDataType3 2"
           $ testSerialization
                 (CDT3Constructor2 3 False :: CustomDataType3 Int Bool Double)
    it "CustomDataType3 3"
           $ testSerialization (CDT3Constructor3 (3 :: Int) False (5 :: Double))

    checkSizeOfNew
        "CustomDataType3"
        (Proxy :: Proxy (CustomDataType3 Int Bool Double))
        (1 + sizeOf (Proxy :: Proxy Int)
           + sizeOf (Proxy :: Proxy Bool)
           + sizeOf (Proxy :: Proxy Double))

testCustomDatatype4TH :: Spec
testCustomDatatype4TH = do
    it "CustomDataType4 1"
           $ testSerialization (CDT4Constructor1 :: CustomDataType4 a Int)
    it "CustomDataType4 2"
           $ testSerialization
                 (CDT4Constructor2 True :: CustomDataType4 a Int)
    it "CustomDataType4 3"
           $ testSerialization (CDT4Constructor3 False (5 :: Int))

    checkSizeOfNew
        "CustomDataType4"
        (Proxy :: Proxy (CustomDataType4 a Int))
        (1 + sizeOf (Proxy :: Proxy Bool) + sizeOf (Proxy :: Proxy Int))

-- C-ish foreign types are platform specific. The platform needs to be taken
-- into consideration when making instances.
testUnboxInstanceExistance :: Spec
testUnboxInstanceExistance = do
    describe "Unbox instances" $ do
        -- TEST_IE(CBool)
        -- TEST_IE(CChar)
        -- TEST_IE(CClock)
        -- TEST_IE(CDouble)
        -- TEST_IE(CFloat)
        -- TEST_IE(CInt)
        -- TEST_IE(CIntMax)
        -- TEST_IE(CIntPtr)
        -- TEST_IE(CLLong)
        -- TEST_IE(CLong)
        -- TEST_IE(CPtrdiff)
        -- TEST_IE(CSChar)
        -- TEST_IE(CSUSeconds)
        -- TEST_IE(CShort)
        -- TEST_IE(CSigAtomic)
        -- TEST_IE(CSize)
        -- TEST_IE(CTime)
        -- TEST_IE(CUChar)
        -- TEST_IE(CUInt)
        -- TEST_IE(CUIntMax)
        -- TEST_IE(CUIntPtr)
        -- TEST_IE(CULLong)
        -- TEST_IE(CULong)
        -- TEST_IE(CUSeconds)
        -- TEST_IE(CUShort)
        -- TEST_IE(CWchar)
        TEST_IE(IntPtr)
        TEST_IE(WordPtr)
        TEST_IE(Fingerprint)
        TEST_IE(Int16)
        TEST_IE(Int32)
        TEST_IE(Int64)
        TEST_IE(Int8)
#if MIN_VERSION_base(4,15,0)
        TEST_IE(IoSubSystem)
#endif
        TEST_IE(Word16)
        TEST_IE(Word32)
        TEST_IE(Word64)
        TEST_IE(Word8)
        -- TEST_IE(CBlkCnt)
        -- TEST_IE(CBlkSize)
        -- TEST_IE(CCc)
        -- TEST_IE(CClockId)
        -- TEST_IE(CDev)
        -- TEST_IE(CFsBlkCnt)
        -- TEST_IE(CFsFilCnt)
        -- TEST_IE(CGid)
        -- TEST_IE(CId)
        -- TEST_IE(CIno)
        -- TEST_IE(CKey)
        -- TEST_IE(CMode)
        -- TEST_IE(CNfds)
        -- TEST_IE(CNlink)
        -- TEST_IE(COff)
        -- TEST_IE(CPid)
        -- TEST_IE(CRLim)
        -- TEST_IE(CSocklen)
        -- TEST_IE(CSpeed)
        -- TEST_IE(CSsize)
        -- TEST_IE(CTcflag)
        -- TEST_IE(CTimer)
        -- TEST_IE(CUid)
        -- TEST_IE(Fd)
        TEST_IE(())
        TEST_IE(Bool)
        TEST_IE(Char)
        TEST_IE(Double)
        TEST_IE(Float)
        TEST_IE(Int)
        TEST_IE(Word)
        TEST_IE(Complex Int)
        TEST_IE(Identity Int)
#if MIN_VERSION_base(4,14,0)
        TEST_IE(Down Int)
#endif
        TEST_IE(FunPtr Int)
        TEST_IE(Ptr Int)
        TEST_IE(Ratio Int)
        TEST_IE(StablePtr Int)
        TEST_IE(Const Int Int)
    where

    testIE :: Unbox a => [a] -> IO ()
    testIE lst = void $ MArray.fromList lst

testCases :: Spec
testCases = do

    testCustomDatatype1TH
    testCustomDatatype2TH
    testCustomDatatype3TH
    testCustomDatatype4TH
    testUnboxInstanceExistance

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unboxed.TH"

main :: IO ()
main = hspec $ H.parallel $ describe moduleName testCases
