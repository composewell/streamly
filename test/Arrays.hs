{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Data.Functor.Identity (runIdentity)
import GHC.Word (Word8)

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, (===))

import Test.Hspec as H

import qualified Streamly.Prelude as S
import qualified Streamly.Array as A

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

randomLengthArray :: Word8 -> Property
randomLengthArray len =
    forAll (vectorOf (fromIntegral len) (arbitrary :: Gen Int)) $ \list ->
        let arr = runIdentity $ A.fromStreamN (fromIntegral len)
                              $ S.fromList list
        in A.length arr === fromIntegral len

randomArrayRoundTrip :: Word8 -> Property
randomArrayRoundTrip len =
    forAll (vectorOf (fromIntegral len) (arbitrary :: Gen Int)) $ \list ->
        let arr = runIdentity $ A.fromStreamN (fromIntegral len)
                              $ S.fromList list
        in runIdentity (S.toList (A.toStream arr)) === list

main :: IO ()
main = hspec
    $ H.parallel
    $ modifyMaxSuccess (const maxTestCount)
    $ do
    describe "Construction" $ do
        prop "length . fromStreamN n === n" $ randomLengthArray
        prop "fromStreamN n . toStream === id" $ randomArrayRoundTrip
