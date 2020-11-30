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
import Foreign.Storable (Storable(..))
import Streamly.Internal.Data.Stream.IsStream (IsStream, SerialT)
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

import Streamly.FileSystem.Handle as FH
import Test.Hspec as H
import Test.Hspec.QuickCheck

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
import qualified Streamly.Internal.Unicode.Stream as U

allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

defaultChunkSize :: Int
defaultChunkSize = 32 * k - allocOverhead
   where k = 1024

maxArrLen :: Int
maxArrLen = defaultChunkSize * 8

maxTestCount :: Int
maxTestCount = 10

chooseWord8 :: (Word8, Word8) -> Gen Word8
chooseWord8 = choose

utf8ToString :: A.Array Word8 -> String
utf8ToString = runIdentity . S.toList . U.decodeUtf8' . A.toStream

testData :: String
testData = "This is the test data for FileSystem.Handle ??`!@#$%^&*~~))`]"

testDataLarge :: String
testDataLarge = concat $ replicate 6000 testData

executor :: (Handle -> (SerialT IO Char)) -> IO (SerialT IO Char)
executor f =
    withSystemTempDirectory "fs_handle" $ \fp -> do
        let fpath = fp </> "tmp_read.txt"
        writeFile fpath testDataLarge
        h <- openFile fpath ReadMode
        return $ f h

readFromHandle :: IO (SerialT IO Char)
readFromHandle =
    let f = U.decodeUtf8 . S.unfold FH.read
    in executor f

readWithBufferFromHandle :: IO (SerialT IO Char)
readWithBufferFromHandle =
    let f1 = (\h -> (1024, h))
        f2 = U.decodeUtf8 . S.unfold FH.readWithBufferOf . f1
    in executor f2

readChunksFromHandle :: IO (SerialT IO Char)
readChunksFromHandle =
    let f = U.decodeUtf8 . S.concatMap (A.toStream) . S.unfold FH.readChunks
    in executor f

readChunksWithBuffer :: IO (SerialT IO Char)
readChunksWithBuffer =
    let f1 = (\h -> (1024, h))
        f2 =
              U.decodeUtf8
            . S.concatMap (A.toStream)
            . S.unfold FH.readChunksWithBufferOf
            . f1
    in executor f2

testRead :: (IsStream t) => IO (t IO Char) -> Property
testRead fn = monadicIO $ do
    let v2 = (S.fromList testDataLarge)
    v1 <- run $ fn
    res <- run $ S.eqBy (==) v1 v2
    assert (res)

testWrite :: (Handle -> FL.Fold IO Word8 ()) -> Property
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
                        S.fold (hfold h) $ S.fromList list
                        hFlush h
                        hSeek h AbsoluteSeek 0
                        ls <- S.toList $ S.unfold FH.read h
                        hClose h
                        return (ls == list)

testWriteWithChunk :: Property
testWriteWithChunk =
    monadicIO $ do
        res <- run $ go
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
                S.fold (FH.writeChunks hw)
                    $ S.unfold FH.readChunksWithBufferOf (1024, hr)
                hFlush hw
                hSeek hw AbsoluteSeek 0
                ls <- S.toList $ S.unfold FH.read hw
                let arr = A.fromList ls
                return (testDataLarge == utf8ToString arr)

main :: IO ()
main =
    hspec $
    H.parallel $
    modifyMaxSuccess (const maxTestCount) $ do
        describe "Read" $ do
            prop "testRead" $ testRead readFromHandle
            prop "testReadWithBuffer" $ testRead readWithBufferFromHandle
            prop "testReadChunks" $ testRead readChunksFromHandle
            prop "testReadChunksWithBuffer" $ testRead readChunksWithBuffer
        describe "Write" $ do
            prop "testWrite" $ testWrite FH.write
            prop "testWriteWithBufferOf" $ testWrite $ FH.writeWithBufferOf 1024
            prop "testWriteWithChunk" $ testWriteWithChunk
