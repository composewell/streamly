{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Foreign.Storable (Storable(..))

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary,
                        choose)
import Test.QuickCheck.Monadic (monadicIO, assert)

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

allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

-- XXX this should be in sync with the defaultChunkSize in Array code, or we
-- should expose that and use that. For fast testing we could reduce the
-- defaultChunkSize under CPP conditionals.
--
defaultChunkSize :: Int
defaultChunkSize = 32 * k - allocOverhead
   where k = 1024

maxArrLen :: Int
maxArrLen = defaultChunkSize * 8

testLength :: Property
testLength =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <-  A.fromStreamN len
                      $ S.fromList list
                assert (A.length arr == len)

testFromToStreamN :: Property
testFromToStreamN =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- A.fromStreamN len
                     $ S.fromList list
                xs <- S.toList
                    $ A.toStream arr
                assert (xs == list)

testToStreamRev :: Property
testToStreamRev =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- A.fromStreamN len
                     $ S.fromList list
                xs <- S.toList
                    $ A.toStreamRev arr
                assert (xs == reverse list)

testArraysOf :: Property
testArraysOf =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                xs <- S.toList
                    $ S.concatMap A.toStream
                    $ A.arraysOf 240
                    $ S.fromList list
                assert (xs == list)

testFlattenArrays :: Property
testFlattenArrays =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                xs <- S.toList
                    $ A.flattenArrays
                    $ A.arraysOf 240
                    $ S.fromList list
                assert (xs == list)

testFromToStream :: Property
testFromToStream =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- A.fromStream $ S.fromList list
                xs <- S.toList
                    $ A.toStream arr
                assert (xs == list)

main :: IO ()
main = hspec
    $ H.parallel
    $ modifyMaxSuccess (const maxTestCount)
    $ do
    describe "Construction" $ do
        prop "length . fromStreamN n === n" $ testLength
        prop "toStream . fromStreamN n === id" $ testFromToStreamN
        prop "toStreamRev . fromStream === reverse" $ testToStreamRev
        prop "arraysOf concats to original" $ testArraysOf
        prop "flattenArrays concats to original" $ testFlattenArrays
        prop "toStream . fromStream === id" $ testFromToStream
