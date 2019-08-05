{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Data.Char (ord, chr)
import Data.Word (Word8)
import Test.Hspec.QuickCheck
import Test.QuickCheck
       (Property, forAll, Gen, listOf, arbitraryUnicodeChar, arbitrary)
import Test.QuickCheck.Monadic (run, monadicIO, assert)

import           Test.Hspec as H

import qualified Streamly.Memory.Array as A
import qualified Streamly.Memory.ArrayStream as AS
import qualified Streamly.Prelude as S
import qualified Streamly.String as SS

-- Coverage build takes too long with default number of tests
{-
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif
-}

-- Use quickcheck-unicode instead?
genUnicode :: Gen String
genUnicode = listOf arbitraryUnicodeChar

genWord8 :: Gen [Word8]
genWord8 = listOf arbitrary

propDecodeEncodeId :: Property
propDecodeEncodeId =
    forAll genUnicode $ \list ->
        monadicIO $ do
            let wrds = SS.encodeUtf8 $ S.fromList list
            chrs <- S.toList $ SS.decodeUtf8 wrds
            assert (chrs == list)

testLines :: Property
testLines =
    forAll genUnicode $ \list ->
        monadicIO $ do
            xs <- S.toList
                $ S.map A.toList
                $ SS.lines
                $ S.fromList list
            assert (xs == lines list)

testLinesArray :: Property
testLinesArray =
    forAll genWord8 $ \list ->
        monadicIO $ do
            xs <- S.toList
                    $ S.map A.toList
                    $ AS.splitOn 10
                    $ S.yield (A.fromList list)
            assert (xs == map (map (fromIntegral . ord))
                              (lines (map (chr .  fromIntegral) list)))

testWords :: Property
testWords =
    forAll genUnicode $ \list ->
        monadicIO $ do
            xs <- S.toList
                $ S.map A.toList
                $ SS.words
                $ S.fromList list
            assert (xs == words list)

testUnlines :: Property
testUnlines =
  forAll genUnicode $ \list ->
      monadicIO $ do
          xs <- S.toList
              $ SS.unlines
              $ SS.lines
              $ S.fromList list
          assert (xs == unlines (lines list))

testUnwords :: Property
testUnwords =
  forAll genUnicode $ \list ->
      monadicIO $ do
          xs <- run
              $ S.toList
              $ SS.unwords
              $ SS.words
              $ S.fromList list
          assert (xs == unwords (words list))

main :: IO ()
main = hspec
    $ H.parallel
    $ modifyMaxSuccess (const 1000)
    $ do
    describe "UTF8 - Encoding / Decoding" $ do
        prop "decodeUtf8 . encodeUtf8 == id" $ propDecodeEncodeId
        prop "Streamly.String.lines == Prelude.lines" $ testLines
        prop "Arrays Streamly.String.lines == Prelude.lines" $ testLinesArray
        prop "Streamly.String.words == Prelude.words" $ testWords
        prop
            "Streamly.String.unlines . Streamly.String.lines == unlines . lines"
             $ testUnlines
        prop
            "Streamly.String.unwords . Streamly.String.words == unwords . words"
             $ testUnwords
