{-# OPTIONS_GHC -Wno-missing-methods #-}
-- |
-- Module      : Streamly.Test.Data.Unbox
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Unbox (main) where

import Data.Proxy
import Foreign.Storable (Storable(..))
import GHC.Generics hiding (moduleName)
import Streamly.Internal.Data.Unboxed
    ( SizeOfRep
    , Unbox(..)
    , genericPeekByteIndex
    , genericPokeByteIndex
    , genericSizeOf
    )
import qualified Streamly.Internal.Data.IORef.Unboxed as IORef

import Test.Hspec as H
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

#define DERIVE_UNBOX(type) \
instance Storable type where \
    { sizeOf _ = genericSizeOf (Proxy :: Proxy type) }; \
\
instance Unbox type where { \
    peekByteIndex = genericPeekByteIndex; \
    pokeByteIndex = genericPokeByteIndex }

test ::(Unbox a, Eq a) => a -> a -> Property
test v1 v2 = monadicIO $ do
    ref <- run $ IORef.newIORef v1
    x1 <- run $ IORef.readIORef ref
    run $  IORef.writeIORef ref v2
    x2 <- run $  IORef.readIORef ref
    assert (x2 == v2)
    assert (x1 == v1)

checkSizeOf :: (SizeOfRep (Rep a)) => Proxy a -> Int -> Property
checkSizeOf v size = monadicIO $
    assert (genericSizeOf v == size)

data Unit = Unit deriving (Show, Generic, Eq)
DERIVE_UNBOX(Unit)

data Single = Single Int deriving (Show, Generic, Eq)
DERIVE_UNBOX(Single)

data Product2 = Product2 Int Char deriving (Show, Generic, Eq)
DERIVE_UNBOX(Product2)

data SumOfProducts = SOP0 | SOP1 Int | SOP2 Int Char | SOP3 Int Int Int deriving (Show, Generic, Eq)
DERIVE_UNBOX(SumOfProducts)

data NestedSOP = NSOP0 SumOfProducts | NSOP1 SumOfProducts deriving (Show, Generic, Eq)
DERIVE_UNBOX(NestedSOP)

moduleName :: String
moduleName = "Data.Unboxed"

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const 1) $ do
      describe moduleName $ do
        describe "Generic Unboxed Type" $ do
            prop "test Unit Unit" $ test Unit Unit
            prop "(Single 0) (Single 1)" $ test (Single 0) (Single 1)
            prop "(Product2 0 'a') (Product2 1 'b')" $ test (Product2 0 'a') (Product2 1 'b')
            prop "(SOP0) (SOP1 1)" $ test (SOP0) (SOP1 1)
            prop "(SOP0) (SOP2 1 'a')" $ test (SOP0) (SOP2 1 'a')
            prop "(SOP0) (SOP3 1 2 3)" $ test (SOP0) (SOP3 1 2 3)

            prop "checkSizeOf Unit" $ checkSizeOf (Proxy :: Proxy Unit) 1
            prop "checkSizeOf Single" $ checkSizeOf (Proxy :: Proxy Single) 8
            prop "checkSizeOf Product2" $ checkSizeOf (Proxy :: Proxy Product2) 12
            prop "checkSizeOf SumOfProducts" $ checkSizeOf (Proxy :: Proxy SumOfProducts) 25
            prop "checkSizeOf NestedSOP" $ checkSizeOf (Proxy :: Proxy NestedSOP) 26
