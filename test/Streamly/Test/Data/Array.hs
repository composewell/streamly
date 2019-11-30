{-# LANGUAGE CPP #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Main (main) where

import Foreign.Storable (Storable(..))

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert)

import Test.Hspec as H

import qualified Streamly.Data.Array as A
import qualified Streamly.Prelude as S

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
                arr <-  S.fold (A.writeN len)
                      $ S.fromList list
                assert (A.length arr == len)

testLengthFromStreamN :: Property
testLengthFromStreamN =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <-  A.fromStreamN len
                      $ S.fromList list
                assert (A.length arr == len)

testFoldUnfold :: Property
testFoldUnfold =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- S.fold (A.writeN len)
                     $ S.fromList list
                xs <- S.toList
                    $ S.unfold A.read arr
                assert (xs == list)

testFoldToStream :: Property
testFoldToStream =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- S.fold (A.writeN len)
                     $ S.fromList list
                xs <- S.toList
                    $ A.toStream arr
                assert (xs == list)

testFoldToStreamRev :: Property
testFoldToStreamRev =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- S.fold (A.writeN len)
                     $ S.fromList list
                xs <- S.toList
                    $ A.toStreamRev arr
                assert (xs == reverse list)

testFromStreamUnfold :: Property
testFromStreamUnfold =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- A.fromStreamN len
                     $ S.fromList list
                xs <- S.toList
                    $ S.unfold A.read arr
                assert (xs == list)

testFromStreamToStream :: Property
testFromStreamToStream =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len (arbitrary :: Gen Int)) $ \list ->
            monadicIO $ do
                arr <- A.fromStreamN len
                     $ S.fromList list
                xs <- S.toList
                    $ A.toStream arr
                assert (xs == list)

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $
    describe "Construction" $ do
        prop "length . writeN n === n" testLength
        prop "length . fromStreamN n === n" testLengthFromStreamN
        prop "read . writeN === id " testFoldUnfold
        prop "toStream . writeN === id" testFoldToStream
        prop "toStreamRev . writeN === reverse" testFoldToStreamRev
        prop "read . fromStreamN === id" testFromStreamUnfold
        prop "toStream . fromStreamN === reverse" testFromStreamToStream
