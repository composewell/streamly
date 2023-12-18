-- |
-- Module      : Streamly.Test.FileSystem.Handle
-- Copyright   : (c) 2020 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.FileSystem.Handle (main) where

import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.System.IO (defaultChunkSize)
import System.FilePath ((</>))
import System.IO
    ( Handle
    , IOMode(..)
    , SeekMode(..)
    , hClose
    , hFlush
    , hSeek
    , openFile
    )
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck (Property, forAll, Gen, vectorOf, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.FileSystem.Handle as Handle
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Test.Hspec as H
import Test.Hspec.QuickCheck

maxArrLen :: Int
maxArrLen = defaultChunkSize * 8

maxTestCount :: Int
maxTestCount = 10

chooseWord8 :: (Word8, Word8) -> Gen Word8
chooseWord8 = choose

toList :: Monad m => Stream m a -> m [a]
toList = Stream.fold Fold.toList

utf8ToString :: Array.Array Word8 -> String
utf8ToString =
    runIdentity . toList . Unicode.decodeUtf8' . Array.read

testData :: String
testData = "This is the test data for FileSystem.Handle ??`!@#$%^&*~~))`]"

testDataLarge :: String
testDataLarge = concat $ replicate 6000 testData

testBinData :: String
testBinData = "01234567890123456789012345678901234567890123456789"

executor :: (Handle -> Stream IO Char) -> IO (Stream IO Char)
executor f =
    withSystemTempDirectory "fs_handle" $ \fp -> do
        let fpath = fp </> "tmp_read.txt"
        writeFile fpath testDataLarge
        h <- openFile fpath ReadMode
        return $ f h

readFromHandle :: IO (Stream IO Char)
readFromHandle =
    let f = Unicode.decodeUtf8 . Stream.unfold Handle.reader
    in executor f

readWithBufferFromHandle :: IO (Stream IO Char)
readWithBufferFromHandle =
    let f1 h = (1024, h)
        f2 = Unicode.decodeUtf8 . Stream.unfold Handle.readerWith . f1
    in executor f2

readChunksFromHandle :: IO (Stream IO Char)
readChunksFromHandle =
    let f =   Unicode.decodeUtf8
            . Stream.concatMap Array.read
            . Stream.unfold Handle.chunkReader
    in executor f

readChunksWithBuffer :: IO (Stream IO Char)
readChunksWithBuffer =
    let f1 h = (1024, h)
        f2 =
              Unicode.decodeUtf8
            . Stream.concatMap Array.read
            . Stream.unfold Handle.chunkReaderWith
            . f1
    in executor f2

testRead :: IO (Stream IO Char) -> Property
testRead fn = monadicIO $ do
    let v2 = Stream.fromList testDataLarge
    v1 <- run fn
    res <- run $ Stream.eqBy (==) v1 v2
    assert res

testWrite :: (Handle -> Fold.Fold IO Word8 ()) -> Property
testWrite hfold =
    forAll (choose (0, maxArrLen)) $ \len ->
        forAll (vectorOf len $ chooseWord8 (0, 255)) $ \list0 ->
            monadicIO $ do
                res <- run $ go list0
                assert res

                where

                go list =
                    withSystemTempDirectory "fs_handle" $ \fp -> do
                        let fpathWrite = fp </> "tmp_write.txt"
                        writeFile fpathWrite ""
                        h <- openFile fpathWrite ReadWriteMode
                        hSeek h AbsoluteSeek 0
                        _ <- Stream.fold (hfold h) $ Stream.fromList list
                        hFlush h
                        hSeek h AbsoluteSeek 0
                        ls <- toList $ Stream.unfold Handle.reader h
                        hClose h
                        return (ls == list)

testWriteWithChunk :: Property
testWriteWithChunk =
    monadicIO $ do
        res <- run go
        assert res

        where

        go =
            withSystemTempDirectory "fs_handle" $ \fp -> do
                let fpathRead = fp </> "tmp_read.txt"
                    fpathWrite = fp </> "tmp_write.txt"
                writeFile fpathRead testDataLarge
                writeFile fpathWrite ""
                hr <- openFile fpathRead ReadMode
                hw <- openFile fpathWrite ReadWriteMode
                hSeek hw AbsoluteSeek 0
                _ <- Stream.fold (Handle.writeChunks hw)
                    $ Stream.unfold Handle.chunkReaderWith (1024, hr)
                hFlush hw
                hSeek hw AbsoluteSeek 0
                ls <- toList $ Stream.unfold Handle.reader hw
                let arr = Array.fromList ls
                return (testDataLarge == utf8ToString arr)

testReadChunksFromToWith :: Int -> Int -> Int -> [[Word8]] -> Property
testReadChunksFromToWith from to buffSize res = monadicIO $ run go

    where

    go =
        withSystemTempDirectory "fs_handle" $ \fp -> do
            let fpathRead = fp </> "tmp_read.txt"
            writeFile fpathRead testBinData
            h <- openFile fpathRead ReadMode
            ls <-
                toList
                    $ Stream.unfold
                        Handle.chunkReaderFromToWith (from, to, buffSize, h)
            return (res `shouldBe` fmap Array.toList ls)

-- Test for first byte
testReadChunksFromToWithFirstByte :: Property
testReadChunksFromToWithFirstByte = testReadChunksFromToWith 0 0 15 [[48]]

-- Test for second byte
testReadChunksFromToWithSecondByte :: Property
testReadChunksFromToWithSecondByte = testReadChunksFromToWith 1 1 15 [[49]]

-- Test for second to 10th bytes
testReadChunksFromToWithSecondToTenthBytes :: Property
testReadChunksFromToWithSecondToTenthBytes =
    testReadChunksFromToWith 1 10 15 [[49,50,51,52,53,54,55,56,57,48]]

-- Test for offset of buffer size
testReadChunksFromToWithBuffSizeOffset :: Property
testReadChunksFromToWithBuffSizeOffset =
    testReadChunksFromToWith 15 25 15 [[53,54,55,56,57,48,49,50,51,52,53]]

-- Test with multi buffer size
testReadChunksFromToWithMultiBuff :: Property
testReadChunksFromToWithMultiBuff =
    testReadChunksFromToWith
        5 22 5 [[53,54,55,56,57],[48,49,50,51,52],[53,54,55,56,57],[48,49,50]]

testReadChunksFromToWithRangeInvalid :: Property
testReadChunksFromToWithRangeInvalid = testReadChunksFromToWith 15 5 15 []

moduleName :: String
moduleName = "FileSystem.Handle"

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
      describe moduleName $ do
        describe "Read From Handle" $ do
            prop "read" $ testRead readFromHandle
            prop "readWith" $ testRead readWithBufferFromHandle
            prop "readChunks" $ testRead readChunksFromHandle
            prop "readChunksWith" $ testRead readChunksWithBuffer
            prop "readChunksFromToWith (0,0,n)"
                testReadChunksFromToWithFirstByte
            prop "readChunksFromToWith (1,1,n)"
                testReadChunksFromToWithSecondByte
            prop "readChunksFromToWith (1,10,n)"
                testReadChunksFromToWithSecondToTenthBytes
            prop "readChunksFromToWith (n,<2n,n)"
                testReadChunksFromToWithBuffSizeOffset
            prop "readChunksFromToWith (n,>2n,n)"
                testReadChunksFromToWithMultiBuff
            prop "readChunksFromToWith (n,<n,n)"
                testReadChunksFromToWithRangeInvalid
        describe "Write To Handle" $ do
            prop "write" $ testWrite Handle.write
            prop "writeWith"
                $ testWrite $ Handle.writeWith 1024
            -- XXX This test needs a lot of stack when built with -O0
            prop "writeChunks" testWriteWithChunk
