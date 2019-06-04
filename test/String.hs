{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, listOf, arbitraryUnicodeChar)
import Test.QuickCheck.Monadic (monadicIO, assert)

import Test.Hspec as H

import qualified Streamly.String as SS
import qualified Streamly.Prelude as S

-- Coverage build takes too long with default number of tests
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif

-- Use quickcheck-unicode instead?
genUnicode :: Gen String
genUnicode = listOf arbitraryUnicodeChar

propDecodeEncodeId :: Property
propDecodeEncodeId =
    forAll genUnicode $ \list ->
        monadicIO $ do
            let wrds = SS.encodeUtf8 $ S.fromList list
            chrs <- S.toList $ SS.decodeUtf8 wrds
            assert (chrs == list)

main :: IO ()
main = hspec
    $ H.parallel
    $ modifyMaxSuccess (const maxTestCount)
    $ do
    describe "UTF8 - Encoding / Decoding" $ do
        prop "decodeUtf8 . encodeUtf8 == id" $ propDecodeEncodeId
