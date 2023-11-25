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

import Data.Proxy (Proxy(..))
import Streamly.Internal.Data.MutByteArray
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

testCases :: Spec
testCases = do

    testCustomDatatype1TH
    testCustomDatatype2TH
    testCustomDatatype3TH
    testCustomDatatype4TH

--------------------------------------------------------------------------------
-- Main function
--------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Unboxed.TH"

main :: IO ()
main = hspec $ H.parallel $ describe moduleName testCases
