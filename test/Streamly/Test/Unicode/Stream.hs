{-# OPTIONS_GHC -Wno-deprecations #-}

module Streamly.Test.Unicode.Stream (main) where

import Control.Monad (when)
import Data.Char (ord, chr)
import Data.Word (Word8, Word16)
import Test.QuickCheck
    ( Property
    , forAll
    , Gen
    , listOf
    , arbitraryASCIIChar
    , arbitraryUnicodeChar
    , arbitrary
    , expectFailure
    , vectorOf
    , choose
    , elements
    )
import Test.QuickCheck.Monadic (run, monadicIO, assert, PropertyM)
import Streamly.Data.Stream (Stream)

import qualified Streamly.Data.Array as A
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Array.Stream as AS
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Unicode.Stream as SS
import qualified Streamly.Internal.Unicode.Stream as IUS
import qualified Streamly.Internal.Unicode.Array as IUA
import qualified Test.Hspec as H

import Test.Hspec.QuickCheck

-- Coverage build takes too long with default number of tests
{-
maxTestCount :: Int
#ifdef DEVBUILD
maxTestCount = 100
#else
maxTestCount = 10
#endif
-}

assertEq :: (Eq a, Show a) => a -> a -> PropertyM IO ()
assertEq a b = do
    when (a /= b) $ run $ do
        putStrLn $ "A: " ++ show a
        putStrLn $ "B: " ++ show b
    assert (a == b)

-- Use quickcheck-unicode instead?
genUnicode :: Gen String
genUnicode = listOf arbitraryUnicodeChar

genWord8List :: Gen [Word8]
genWord8List = listOf arbitrary

{-
genListOfW8List :: Gen [[Word8]]
genListOfW8List = listOf (listOf arbitrary)
-}

propDecodeEncodeId' :: Property
propDecodeEncodeId' =
    forAll genUnicode $ \list ->
        monadicIO $ do
            let wrds = SS.encodeUtf8' $ Stream.fromList list
            chrs <- run $ Stream.toList $ SS.decodeUtf8' wrds
            assert (chrs == list)

propDecodeEncodeUtf16Id
    :: (Stream IO Char -> Stream IO Word16)
    -> (Stream IO Word16 -> Stream IO Char)
    -> Property
propDecodeEncodeUtf16Id encoder decoder =
    forAll genUnicode $ \list ->
        monadicIO $ do
            let wrds = encoder $ Stream.fromList list
            chrs <- run $ Stream.toList $ decoder wrds
            assertEq chrs list

{-
propMkEvenW8Chunks :: Property
propMkEvenW8Chunks =
    forAll genListOfW8List $ \list ->
        monadicIO $ do
            list1 <-
                run $ Stream.toList
                    $ fmap A.toList
                    $ IUS.mkEvenW8Chunks
                    $ fmap A.fromList $ Stream.fromList list
            let concatedList = concat list
                concatedList1 = concat list1
            assert (and (map (even . length) list1))
            if (odd (length concatedList))
            then assertEq concatedList1 (init concatedList)
            else assertEq concatedList1 concatedList
-}

-- XXX need to use invalid characters
propDecodeEncodeId :: Property
propDecodeEncodeId =
    forAll genUnicode $ \list ->
        monadicIO $ do
            let wrds = SS.encodeUtf8 $ Stream.fromList list
            chrs <- Stream.toList $ SS.decodeUtf8 wrds
            assertEq chrs list

propDecodeEncodeIdArrays :: Property
propDecodeEncodeIdArrays =
    forAll genUnicode $ \list ->
        monadicIO $ do
            let wrds = Array.chunksOf 8 $ SS.encodeUtf8' $ Stream.fromList list
            chrs <- Stream.toList $ IUS.decodeUtf8Chunks wrds
            assertEq chrs list

unicodeTestData :: [Char]
unicodeTestData = "z\72150\83468;L$Wz| ?_i/J ."

latin1TestData :: [Char]
latin1TestData = "z\214\f;L$Wz| ?_i/J ."

propASCIIToLatin1 :: Property
propASCIIToLatin1 =
    forAll (choose (1, 1000)) $ \len ->
        forAll (vectorOf len arbitraryASCIIChar) $ \list ->
            monadicIO $ do
                let wrds = SS.decodeLatin1
                            $ SS.encodeLatin1
                            $ Stream.fromList list
                lst <- run $  Stream.toList wrds
                assertEq list lst

propUnicodeToLatin1 :: Property
propUnicodeToLatin1 =
    monadicIO $ do
        let wrds =
                SS.decodeLatin1
                    $ SS.encodeLatin1
                    $ Stream.fromList unicodeTestData
        lst <- run $ Stream.toList wrds
        assertEq latin1TestData lst

propUnicodeToLatin1' :: Property
propUnicodeToLatin1' =
    monadicIO $ do
        let wrds =
                SS.decodeLatin1
                    $ SS.encodeLatin1'
                    $ Stream.fromList unicodeTestData
        lst <- run $ Stream.toList wrds
        assertEq latin1TestData lst

testLines :: Property
testLines =
    forAll genUnicode $ \list ->
        monadicIO $ do
            xs <- Stream.toList
                $ fmap A.toList
                $ IUA.lines
                $ Stream.fromList list
            assertEq xs (lines list)

testLinesArray :: Property
testLinesArray =
    forAll genWord8List $ \list ->
        monadicIO $ do
            xs <- Stream.toList
                    $ fmap A.toList
                    $ AS.splitOnSuffix 10
                    $ Stream.fromPure (A.fromList list)
            assertEq xs (map (map (fromIntegral . ord))
                              (lines (map (chr .  fromIntegral) list)))

testWords :: Property
testWords =
    forAll genUnicode $ \list ->
        monadicIO $ do
            xs <- Stream.toList
                $ Stream.map A.toList
                $ IUA.words
                $ Stream.fromList list
            assertEq xs (words list)

testUnlines :: Property
testUnlines =
  forAll genUnicode $ \list ->
      monadicIO $ do
          xs <- Stream.toList
              $ IUA.unlines
              $ IUA.lines
              $ Stream.fromList list
          assertEq xs (unlines (lines list))

testUnwords :: Property
testUnwords =
  forAll genUnicode $ \list ->
      monadicIO $ do
          xs <- run
              $ Stream.toList
              $ IUA.unwords
              $ IUA.words
              $ Stream.fromList list
          assertEq xs (unwords (words list))

-- | Decoding a byte stream in chunks gives the same result as decoding the
-- flat stream, also for invalid input.
propDecodeUtf8ChunksEq ::
       (Stream IO Word8 -> Stream IO Char)
    -> (Stream IO (A.Array Word8) -> Stream IO Char)
    -> Property
propDecodeUtf8ChunksEq decode decodeChunks =
    forAll (choose (1, 5)) $ \n ->
        forAll (listOf (elements bytes)) $ \list ->
            monadicIO $ do
                let strm = Stream.fromList list
                xs <- run $ Stream.toList $ decode strm
                ys <- run $ Stream.toList
                    $ decodeChunks (Array.chunksOf n strm)
                assertEq xs ys

    where

    -- Valid and invalid lead bytes, continuation bytes and ASCII.
    bytes = [0x41, 0x7f, 0x80, 0xbf, 0xc3, 0xa9, 0xe2, 0x82, 0xac, 0xf0, 0x9f
            , 0x98, 0x80, 0xff, 0xc0]

testDecodeUtf8ChunksInvalid :: H.Expectation
testDecodeUtf8ChunksInvalid = do
    let decode f xs = Stream.toList $ f $ Stream.fromList $ fmap A.fromList xs
    decode IUS.decodeUtf8Chunks [[0xC3, 0x41]]
        `H.shouldReturn` "\xFFFD\&A"
    decode IUS.decodeUtf8Chunks [[0xC3], [0x41]]
        `H.shouldReturn` "\xFFFD\&A"
    decode IUS.decodeUtf8Chunks_ [[0xC3, 0x41]]
        `H.shouldReturn` "A"

-- | Reference UTF-16 decoder, an unpaired surrogate is replaced by U+FFFD
-- and the next word is decoded on its own.
refDecodeUtf16 :: [Word16] -> String
refDecodeUtf16 (h:l:xs)
    | isHigh h && isLow l =
        chr (0x10000 + (fromIntegral (h - 0xD800) * 0x400)
                + fromIntegral (l - 0xDC00))
            : refDecodeUtf16 xs
    where
    isHigh x = x >= 0xD800 && x <= 0xDBFF
    isLow x = x >= 0xDC00 && x <= 0xDFFF
refDecodeUtf16 (x:xs)
    | x >= 0xD800 && x <= 0xDFFF = '\xFFFD' : refDecodeUtf16 xs
    | otherwise = chr (fromIntegral x) : refDecodeUtf16 xs
refDecodeUtf16 [] = []

propDecodeUtf16Ref :: Property
propDecodeUtf16Ref =
    forAll (listOf (elements words16)) $ \list ->
        monadicIO $ do
            xs <- run
                $ Stream.toList $ IUS.decodeUtf16le $ Stream.fromList list
            assertEq (refDecodeUtf16 list) xs

    where

    words16 = [0x41, 0xD800, 0xDBFF, 0xDC00, 0xDFFF, 0xFFFF, 0xE000]

testDecodeUtf16Unpaired :: H.Expectation
testDecodeUtf16Unpaired = do
    let decode = Stream.toList . IUS.decodeUtf16le . Stream.fromList
    decode [0xD800, 0x41] `H.shouldReturn` "\xFFFD\&A"
    decode [0xD800, 0xD800, 0xDC00] `H.shouldReturn` "\xFFFD\x10000"
    decode [0xD800] `H.shouldReturn` "\xFFFD"

testEncodeUtf16Surrogate :: H.Expectation
testEncodeUtf16Surrogate =
    Stream.toList (IUS.encodeUtf16le (Stream.fromList "a\xD800\&b"))
        `H.shouldReturn` [0x61, 0xFFFD, 0x62]

-- | A parser failure must be a 'Left', not an exception.
testParseCharUtf8EmptyInput :: H.Expectation
testParseCharUtf8EmptyInput = do
    let isLeft = either (const True) (const False)
    r1 <- Stream.parse IUS.writeCharUtf8' (Stream.fromList [])
    isLeft r1 `H.shouldBe` True
    r2 <- Stream.parse
            (IUS.parseCharUtf8With IUS.DropOnCodingFailure)
            (Stream.fromList [0xC3])
    isLeft r2 `H.shouldBe` True

-- | encodeStrings encodes each string to a separate array.
propEncodeStrings :: Property
propEncodeStrings =
    forAll (listOf genUnicode) $ \strs -> monadicIO $ do
        arrs <- run $ Stream.toList
            $ IUS.encodeStrings IUS.encodeUtf8 (Stream.fromList strs)
        decoded <- run
            $ mapM (Stream.toList . IUS.decodeUtf8 . Array.read) arrs
        assertEq strs decoded

moduleName :: String
moduleName = "Unicode.Stream"

main :: IO ()
main = H.hspec
  $ H.parallel
  $ modifyMaxSuccess (const 1000)
  $ H.describe moduleName $ do
    H.describe "UTF8 - Encoding / Decoding" $ do
        prop "decodeUtf8' . encodeUtf8' == id" propDecodeEncodeId'
        prop "decodeUtf8 . encodeUtf8' == id" propDecodeEncodeId
        prop "decodeUtf8Arrays . encodeUtf8' == id"
                propDecodeEncodeIdArrays
        prop "encodeStrings" propEncodeStrings
        prop "decodeUtf8Chunks == decodeUtf8"
            (propDecodeUtf8ChunksEq IUS.decodeUtf8 IUS.decodeUtf8Chunks)
        prop "decodeUtf8Chunks_ == decodeUtf8_"
            (propDecodeUtf8ChunksEq IUS.decodeUtf8_ IUS.decodeUtf8Chunks_)
        H.it "decodeUtf8Chunks invalid subsequent byte"
            testDecodeUtf8ChunksInvalid
        H.it "parseCharUtf8With on insufficient input"
            testParseCharUtf8EmptyInput
        prop "Streamly.Data.String.lines == Prelude.lines" testLines
        prop "Arrays Streamly.Data.String.lines == Prelude.lines"
            testLinesArray
        prop "Streamly.Data.String.words == Prelude.words" testWords
        prop
            "Streamly.Data.String.unlines . Streamly.Data.String.lines == unlines . lines"
            testUnlines
        prop
            "Streamly.Data.String.unwords . Streamly.Data.String.words == unwords . words"
            testUnwords

    H.describe "UTF16 - Encoding / Decoding" $ do
        prop "decodeUtf16le' . encodeUtf16le' == id"
             (propDecodeEncodeUtf16Id IUS.encodeUtf16le' IUS.decodeUtf16le')
        prop "decodeUtf16le . encodeUtf16le == id"
             (propDecodeEncodeUtf16Id IUS.encodeUtf16le' IUS.decodeUtf16le)
        prop "decodeUtf16le == reference decoder" propDecodeUtf16Ref
        H.it "decodeUtf16le unpaired high surrogate" testDecodeUtf16Unpaired
        H.it "encodeUtf16le surrogate" testEncodeUtf16Surrogate
        -- prop "mkEvenW8Chunks" propMkEvenW8Chunks

    H.describe "Latin1 - Encoding / Decoding" $ do
        prop "ASCII to Latin1" propASCIIToLatin1
        prop "Unicode to Latin1" propUnicodeToLatin1
        prop "Unicode to Latin1'" $ expectFailure  propUnicodeToLatin1'
