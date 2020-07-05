-- |
-- Module      : Streamly.Benchmark.FileSystem.Handle
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

import Control.DeepSeq (NFData)
import Control.Exception (SomeException)
import Data.Char (ord, chr)
import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import System.Directory (getFileSize)
import System.Environment (lookupEnv)
import System.IO (openFile, IOMode(..), Handle, hClose)
import System.Process.Typed (shell, runProcess_)
import Prelude hiding (last, length)

import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Unicode.Stream as SS
import qualified Streamly.FileSystem.Handle as FH
-- import qualified Streamly.Internal.Data.Fold as IFL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.Data.Unicode.Stream as IUS
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Memory.Array as IA
import qualified Streamly.Internal.Memory.Array.Types as AT
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Internal.Memory.Unicode.Array as IUA
import qualified Streamly.Internal.Prelude as IP
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S

import Data.IORef
import Gauge hiding (env)
import Streamly.Benchmark.Common

#ifdef INSPECTION
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..), GroupState)
import Test.Inspection
#endif

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.Handle"

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

scratchDir :: String
scratchDir = "benchmark-tmp"

outfile :: String
outfile = scratchDir ++ "out.txt"

smallFileSize :: Int
smallFileSize = 10 * 1024 * 1024

inFileSmall :: String
inFileSmall = scratchDir ++ "in-10MB.txt"

bigFileSize :: Int
bigFileSize = 100 * 1024 * 1024

inFileBig :: String
inFileBig = scratchDir ++ "in-100MB.txt"

blockSize :: Int
blockSize = 32768

blockCount :: Int -> Int
blockCount size = (size + blockSize - 1) `div` blockSize

data Handles = Handles Handle Handle

mkBench :: NFData b => String -> IORef Handles -> IO b -> Benchmark
mkBench name href action =
    bench name $ perRunEnv (do
            Handles inH outH <- readIORef href

            -- close old handles
            hClose inH
            hClose outH

            -- reopen
            inh <- openFile inFileBig ReadMode
            outh <- openFile outfile WriteMode

            -- update
            writeIORef href $ Handles inh outh
        )
        (\_ -> action)

-------------------------------------------------------------------------------
-- read chunked using toChunks
-------------------------------------------------------------------------------

-- | Get the last byte from a file bytestream.
{-# INLINE toChunksLast #-}
toChunksLast :: Handle -> IO (Maybe Word8)
toChunksLast inh = do
    let s = IFH.toChunks inh
    larr <- S.last s
    return $ case larr of
        Nothing -> Nothing
        Just arr -> IA.readIndex arr (A.length arr - 1)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksLast
inspect $ 'toChunksLast `hasNoType` ''Step
#endif

-- | Count the number of bytes in a file.
{-# INLINE toChunksSumLengths #-}
toChunksSumLengths :: Handle -> IO Int
toChunksSumLengths inh =
    let s = IFH.toChunks inh
    in S.sum (S.map A.length s)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSumLengths
inspect $ 'toChunksSumLengths `hasNoType` ''Step
#endif

-- | Count the number of lines in a file.
{-# INLINE toChunksSplitOnSuffix #-}
toChunksSplitOnSuffix :: Handle -> IO Int
toChunksSplitOnSuffix = S.length . AS.splitOnSuffix 10 . IFH.toChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSplitOnSuffix
inspect $ 'toChunksSplitOnSuffix `hasNoType` ''Step
#endif

-- XXX use a word splitting combinator instead of splitOn and test it.
-- | Count the number of words in a file.
{-# INLINE toChunksSplitOn #-}
toChunksSplitOn :: Handle -> IO Int
toChunksSplitOn = S.length . AS.splitOn 32 . IFH.toChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSplitOn
inspect $ 'toChunksSplitOn `hasNoType` ''Step
#endif

-- | Sum the bytes in a file.
{-# INLINE toChunksCountBytes #-}
toChunksCountBytes :: Handle -> IO Word8
toChunksCountBytes inh = do
    let foldlArr' f z = runIdentity . S.foldl' f z . IA.toStream
    let s = IFH.toChunks inh
    S.foldl' (\acc arr -> acc + foldlArr' (+) 0 arr) 0 s

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksCountBytes
inspect $ 'toChunksCountBytes `hasNoType` ''Step
#endif

{-# INLINE toChunksWithBufferOfDecodeUtf8ArraysLenient #-}
toChunksWithBufferOfDecodeUtf8ArraysLenient :: Handle -> IO ()
toChunksWithBufferOfDecodeUtf8ArraysLenient inh =
   S.drain
     $ IUS.decodeUtf8ArraysLenient
     $ IFH.toChunksWithBufferOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'decodeUtf8Lenient
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''Step
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''AT.FlattenState
-- inspect $ 'decodeUtf8Lenient `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_read_chunked :: IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_read_chunked href _ _ =
    -- read using toChunks instead of read
    [ bgroup "reduce/toChunks"
        [ mkBench "S.last (32K)" href $ do
            Handles inh _ <- readIORef href
            toChunksLast inh
        -- Note: this cannot be fairly compared with GNU wc -c or wc -m as
        -- wc uses lseek to just determine the file size rather than reading
        -- and counting characters.
        , mkBench "S.sum . S.map A.length (32K)" href $ do
            Handles inh _ <- readIORef href
            toChunksSumLengths inh
        , mkBench "AS.splitOnSuffix (32K)" href $ do
            Handles inh _ <- readIORef href
            toChunksSplitOnSuffix inh
        , mkBench "AS.splitOn (32K)" href $ do
            Handles inh _ <- readIORef href
            toChunksSplitOn inh
        , mkBench "countBytes (32K)" href $ do
            Handles inh _ <- readIORef href
            toChunksCountBytes inh
        , mkBench "US.decodeUtf8ArraysLenient (1MB)" href $ do
            Handles inh _ <- readIORef href
            toChunksWithBufferOfDecodeUtf8ArraysLenient inh
        ]
    ]

-- TBD reading with unfold

-------------------------------------------------------------------------------
-- copy chunked
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null
{-# INLINE toChunksWithBufferOf #-}
toChunksWithBufferOf :: Handle -> Handle -> IO ()
toChunksWithBufferOf devNull inh =
    S.fold (IFH.writeChunks devNull) $ IFH.toChunksWithBufferOf (256*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksWithBufferOf
inspect $ 'toChunksWithBufferOf `hasNoType` ''Step
#endif

-- | Copy file
{-# INLINE copyChunks #-}
copyChunks :: Handle -> Handle -> IO ()
copyChunks inh outh =
    let s = IFH.toChunks inh
    in S.fold (IFH.writeChunks outh) s

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksWriteChunks
inspect $ 'toChunksWriteChunks `hasNoType` ''Step
#endif

-- | Copy file
{-# INLINE copyCodecUtf8ArraysLenient #-}
copyCodecUtf8ArraysLenient :: Handle -> Handle -> IO ()
copyCodecUtf8ArraysLenient inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ IUS.decodeUtf8ArraysLenient
     $ IFH.toChunksWithBufferOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyCodecUtf8ArraysLenient
-- inspect $ 'copyCodecUtf8ArraysLenient `hasNoType` ''Step
-- inspect $ 'copyCodecUtf8ArraysLenient `hasNoType` ''AT.FlattenState
-- inspect $ 'copyCodecUtf8ArraysLenient `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_copy_chunked :: IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_copy_chunked href devNull _ =
    [ bgroup "copy/toChunks"
        [ mkBench "toNull (256K)" href $ do
            Handles inh _ <- readIORef href
            toChunksWithBufferOf devNull inh
        , mkBench "raw" href $ do
            Handles inh outh <- readIORef href
            copyChunks inh outh
        , mkBench "decodeEncodeUtf8Lenient" href $ do
            Handles inh outh <- readIORef href
            copyCodecUtf8ArraysLenient inh outh
        ]
    ]

-- TBD reading with unfold

-------------------------------------------------------------------------------
-- Exceptions readChunks
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
{-# INLINE readChunksWithBufferOfOnExceptionUnfold #-}
readChunksWithBufferOfOnExceptionUnfold :: Handle -> Handle -> IO ()
readChunksWithBufferOfOnExceptionUnfold devNull inh =
    let readEx = IUF.onException (\_ -> hClose inh)
                    (IUF.supplyFirst FH.readChunksWithBufferOf (256*1024))
    in IUF.fold readEx (IFH.writeChunks devNull) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readChunksWithBufferOfOnExceptionUnfold
-- inspect $ 'readChunksWithBufferOfOnExceptionUnfold `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
{-# INLINE readChunksWithBufferOfBracketUnfold #-}
readChunksWithBufferOfBracketUnfold :: Handle -> Handle -> IO ()
readChunksWithBufferOfBracketUnfold devNull inh =
    let readEx = IUF.bracket return (\_ -> hClose inh)
                    (IUF.supplyFirst FH.readChunksWithBufferOf (256*1024))
    in IUF.fold readEx (IFH.writeChunks devNull) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readChunksWithBufferOfBracketUnfold
-- inspect $ 'readChunksWithBufferOfBracketUnfold `hasNoType` ''Step
#endif

{-# INLINE readChunksWithBufferOfBracketIOUnfold #-}
readChunksWithBufferOfBracketIOUnfold :: Handle -> Handle -> IO ()
readChunksWithBufferOfBracketIOUnfold devNull inh =
    let readEx = IUF.bracketIO return (\_ -> hClose inh)
                    (IUF.supplyFirst FH.readChunksWithBufferOf (256*1024))
    in IUF.fold readEx (IFH.writeChunks devNull) inh

o_1_space_copy_exceptions_readChunks :: IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_copy_exceptions_readChunks href devNull _ =
    [ bgroup "copy/exceptions/unfold/readChunks"
        [ mkBench "onException (256K)" href $ do
            Handles inh _ <- readIORef href
            readChunksWithBufferOfOnExceptionUnfold devNull inh
        , mkBench "bracket (256K)" href $ do
            Handles inh _ <- readIORef href
            readChunksWithBufferOfBracketUnfold devNull inh
        , mkBench "bracketIO (256K)" href $ do
            Handles inh _ <- readIORef href
            readChunksWithBufferOfBracketIOUnfold devNull inh
        ]
    ]

-------------------------------------------------------------------------------
-- Exceptions toChunks
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
{-# INLINE toChunksWithBufferOfBracketStream #-}
toChunksWithBufferOfBracketStream :: Handle -> Handle -> IO ()
toChunksWithBufferOfBracketStream devNull inh =
    let readEx = S.bracket (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toChunksWithBufferOf (256*1024) inh)
    in S.fold (IFH.writeChunks devNull) $ readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksWithBufferOfBracketStream
-- inspect $ 'toChunksWithBufferOfBracketStream `hasNoType` ''Step
#endif

{-# INLINE toChunksWithBufferOfBracketIOStream #-}
toChunksWithBufferOfBracketIOStream :: Handle -> Handle -> IO ()
toChunksWithBufferOfBracketIOStream devNull inh =
    let readEx = IP.bracketIO (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toChunksWithBufferOf (256*1024) inh)
    in S.fold (IFH.writeChunks devNull) $ readEx

o_1_space_copy_exceptions_toChunks :: IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_copy_exceptions_toChunks href devNull _ =
    [ bgroup "copy/exceptions/stream/toChunks"
        [ mkBench "bracket (256K)" href $ do
            Handles inh _ <- readIORef href
            toChunksWithBufferOfBracketStream devNull inh
        , mkBench "bracketIO (256K)" href $ do
            Handles inh _ <- readIORef href
            toChunksWithBufferOfBracketIOStream devNull inh
        ]
    ]

-------------------------------------------------------------------------------
-- unfold read
-------------------------------------------------------------------------------

-- | Get the last byte from a file bytestream.
{-# INLINE readLast #-}
readLast :: Handle -> IO (Maybe Word8)
readLast = S.last . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readLast
inspect $ 'readLast `hasNoType` ''Step
inspect $ 'readLast `hasNoType` ''AT.FlattenState
inspect $ 'readLast `hasNoType` ''D.ConcatMapUState
#endif

-- assert that flattenArrays constructors are not present
-- | Count the number of bytes in a file.
{-# INLINE readCountBytes #-}
readCountBytes :: Handle -> IO Int
readCountBytes = S.length . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countBytes
inspect $ 'countBytes `hasNoType` ''Step
inspect $ 'countBytes `hasNoType` ''AT.FlattenState
inspect $ 'countBytes `hasNoType` ''D.ConcatMapUState
#endif

-- | Count the number of lines in a file.
{-# INLINE readCountLines #-}
readCountLines :: Handle -> IO Int
readCountLines =
    S.length
        . IUS.lines FL.drain
        . SS.decodeLatin1
        . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readCountLines
inspect $ 'readCountLines `hasNoType` ''Step
inspect $ 'readCountLines `hasNoType` ''AT.FlattenState
inspect $ 'readCountLines `hasNoType` ''D.ConcatMapUState
#endif

-- | Count the number of words in a file.
{-# INLINE readCountWords #-}
readCountWords :: Handle -> IO Int
readCountWords =
    S.length
        . IUS.words FL.drain
        . SS.decodeLatin1
        . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readCountWords
-- inspect $ 'readCountWords `hasNoType` ''Step
-- inspect $ 'readCountWords `hasNoType` ''D.ConcatMapUState
#endif

-- | Sum the bytes in a file.
{-# INLINE readSumBytes #-}
readSumBytes :: Handle -> IO Word8
readSumBytes = S.sum . S.unfold FH.read

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readSumBytes
inspect $ 'readSumBytes `hasNoType` ''Step
inspect $ 'readSumBytes `hasNoType` ''AT.FlattenState
inspect $ 'readSumBytes `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE readDrain #-}
readDrain :: Handle -> IO ()
readDrain inh = S.drain $ S.unfold FH.read inh

{-# INLINE readDecodeLatin1 #-}
readDecodeLatin1 :: Handle -> IO ()
readDecodeLatin1 inh =
   S.drain
     $ SS.decodeLatin1
     $ S.unfold FH.read inh

{-# INLINE readDecodeUtf8Lax #-}
readDecodeUtf8Lax :: Handle -> IO ()
readDecodeUtf8Lax inh =
   S.drain
     $ SS.decodeUtf8Lax
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readDecodeUtf8Lax
-- inspect $ 'readDecodeUtf8Lax `hasNoType` ''Step
-- inspect $ 'readDecodeUtf8Lax `hasNoType` ''AT.FlattenState
-- inspect $ 'readDecodeUtf8Lax `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_reduce_read :: IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_reduce_read href _ _ =
    [ bgroup "reduce/read"
        [ -- read raw bytes without any decoding
          mkBench "S.drain" href $ do
            Handles inh _ <- readIORef href
            readDrain inh
        , mkBench "S.last" href $ do
            Handles inh _ <- readIORef href
            readLast inh
        , mkBench "S.sum" href $ do
            Handles inh _ <- readIORef href
            readSumBytes inh

        -- read with Latin1 decoding
        , mkBench "SS.decodeLatin1" href $ do
            Handles inh _ <- readIORef href
            readDecodeLatin1 inh
        , mkBench "S.length" href $ do
            Handles inh _ <- readIORef href
            readCountBytes inh
        , mkBench "US.lines . SS.decodeLatin1" href $ do
            Handles inh _ <- readIORef href
            readCountLines inh
        , mkBench "US.words . SS.decodeLatin1" href $ do
            Handles inh _ <- readIORef href
            readCountWords inh

        -- read with utf8 decoding
        , mkBench "SS.decodeUtf8Lax" href $ do
            Handles inh _ <- readIORef href
            readDecodeUtf8Lax inh
        ]
    ]

-------------------------------------------------------------------------------
-- stream toBytes
-------------------------------------------------------------------------------

-- | Count the number of lines in a file.
{-# INLINE toChunksConcatUnfoldCountLines #-}
toChunksConcatUnfoldCountLines :: Handle -> IO Int
toChunksConcatUnfoldCountLines inh =
    S.length
        $ IUS.lines FL.drain
        $ SS.decodeLatin1
        -- XXX replace with toBytes
        $ S.concatUnfold A.read (IFH.toChunks inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'countLinesU
inspect $ 'countLinesU `hasNoType` ''Step
inspect $ 'countLinesU `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_reduce_toBytes :: IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_reduce_toBytes href _ _ =
    [ bgroup "reduce/toBytes"
        [ mkBench "US.lines . SS.decodeLatin1" href $ do
            Handles inh _ <- readIORef href
            toChunksConcatUnfoldCountLines inh
        ]
    ]

-------------------------------------------------------------------------------
-- reduce after grouping in chunks
-------------------------------------------------------------------------------

{-# INLINE chunksOfSum #-}
chunksOfSum :: Int -> Handle -> IO Int
chunksOfSum n inh = S.length $ S.chunksOf n FL.sum (S.unfold FH.read inh)

{-# INLINE parseManyChunksOfSum #-}
parseManyChunksOfSum :: Int -> Handle -> IO Int
parseManyChunksOfSum n inh =
    S.length $ IP.parseMany (PR.take n FL.sum) (S.unfold FH.read inh)

-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE chunksOf #-}
chunksOf :: Int -> Handle -> IO Int
chunksOf n inh =
    -- writeNUnsafe gives 2.5x boost here over writeN.
    -- XXX replace with S.arraysOf
    S.length $ S.chunksOf n (AT.writeNUnsafe n) (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'chunksOf
inspect $ 'chunksOf `hasNoType` ''Step
inspect $ 'chunksOf `hasNoType` ''AT.FlattenState
inspect $ 'chunksOf `hasNoType` ''D.ConcatMapUState
inspect $ 'chunksOf `hasNoType` ''GroupState
#endif

-- This is to make sure that the concatMap in FH.read, groupsOf and foldlM'
-- together can fuse.
--
-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE _chunksOfD #-}
_chunksOfD :: Int -> Handle -> IO Int
_chunksOfD n inh =
    D.foldlM' (\i _ -> return $ i + 1) (return 0)
        $ D.groupsOf n (AT.writeNUnsafe n)
        $ D.fromStreamK (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses '_chunksOfD
inspect $ '_chunksOfD `hasNoType` ''Step
inspect $ '_chunksOfD `hasNoType` ''GroupState
inspect $ '_chunksOfD `hasNoType` ''AT.FlattenState
inspect $ '_chunksOfD `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_reduce_read_grouped :: IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_reduce_read_grouped href _ value =
    [ bgroup "reduce/read/chunks"
        [ mkBench ("S.chunksOf " ++ show value ++  " FL.sum") href $ do
            Handles inh _ <- readIORef href
            chunksOfSum value inh
        , mkBench "S.chunksOf 1 FL.sum" href $ do
            Handles inh _ <- readIORef href
            chunksOfSum 1 inh

        -- Chunk using parsers
        , mkBench ("S.parseMany (PR.take " ++ show value ++ " FL.sum)")
            href $ do
                Handles inh _ <- readIORef href
                parseManyChunksOfSum value inh
        , mkBench "S.parseMany (PR.take 1 FL.sum)" href $ do
                Handles inh _ <- readIORef href
                parseManyChunksOfSum 1 inh

        -- folding chunks to arrays
        , mkBench "S.arraysOf 1" href $ do
            Handles inh _ <- readIORef href
            chunksOf 1 inh
        , mkBench "S.arraysOf 10" href $ do
            Handles inh _ <- readIORef href
            chunksOf 10 inh
        , mkBench "S.arraysOf 1000" href $ do
            Handles inh _ <- readIORef href
            chunksOf 1000 inh
        ]
    ]

-------------------------------------------------------------------------------
-- copy unfold
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null
{-# INLINE readWriteNull #-}
readWriteNull :: Handle -> Handle -> IO ()
readWriteNull devNull inh = S.fold (FH.write devNull) $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteNull
inspect $ 'readWriteNull `hasNoType` ''Step
inspect $ 'readWriteNull `hasNoType` ''AT.FlattenState
inspect $ 'readWriteNull `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE copyStream #-}
copyStream :: Handle -> Handle -> IO ()
copyStream inh outh = S.fold (FH.write outh) (S.unfold FH.read inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStream
inspect $ 'copyStream `hasNoType` ''Step
inspect $ 'copyStream `hasNoType` ''AT.FlattenState
inspect $ 'copyStream `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE copyStreamLatin1 #-}
copyStreamLatin1 :: Handle -> Handle -> IO ()
copyStreamLatin1 inh outh =
   S.fold (FH.write outh)
     $ SS.encodeLatin1
     $ SS.decodeLatin1
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStreamLatin1
inspect $ 'copyStreamLatin1 `hasNoType` ''Step
inspect $ 'copyStreamLatin1 `hasNoType` ''AT.FlattenState
inspect $ 'copyStreamLatin1 `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE _copyStreamUtf8 #-}
_copyStreamUtf8 :: Handle -> Handle -> IO ()
_copyStreamUtf8 inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ SS.decodeUtf8
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses '_copyStreamUtf8
-- inspect $ '_copyStreamUtf8 `hasNoType` ''Step
-- inspect $ '_copyStreamUtf8 `hasNoType` ''AT.FlattenState
-- inspect $ '_copyStreamUtf8 `hasNoType` ''D.ConcatMapUState
#endif

-- | Copy file
{-# INLINE copyStreamUtf8Lax #-}
copyStreamUtf8Lax :: Handle -> Handle -> IO ()
copyStreamUtf8Lax inh outh =
   S.fold (FH.write outh)
     $ SS.encodeUtf8
     $ SS.decodeUtf8Lax
     $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStreamUtf8Lax
-- inspect $ 'copyStreamUtf8Lax `hasNoType` ''Step
-- inspect $ 'copyStreamUtf8Lax `hasNoType` ''AT.FlattenState
-- inspect $ 'copyStreamUtf8Lax `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_copy_read :: IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_copy_read href devNull _ =
    [ bgroup "copy/read"
        [ mkBench "rawToNull" href $ do
            Handles inh _ <- readIORef href
            readWriteNull devNull inh
        , mkBench "rawToFile" href $ do
            Handles inh outh <- readIORef href
            copyStream inh outh
        -- This needs an ascii file, as decode just errors out.
        , mkBench "SS.encodeLatin1 . SS.decodeLatin1" href $ do
            Handles inh outh <- readIORef href
            copyStreamLatin1 inh outh
#ifdef DEVBUILD
        , mkBench "copyUtf8" href $ do
            Handles inh outh <- readIORef href
            _copyStreamUtf8 inh outh
#endif
        , mkBench "SS.encodeUtf8 . SS.decodeUtf8Lax" href $ do
            Handles inh outh <- readIORef href
            copyStreamUtf8Lax inh outh
        ]
    ]

-------------------------------------------------------------------------------
-- copy stream
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null
{-# INLINE readFromBytesNull #-}
readFromBytesNull :: Handle -> Handle -> IO ()
readFromBytesNull devNull inh = IFH.fromBytes devNull $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'catStreamWrite
inspect $ 'catStreamWrite `hasNoType` ''Step
inspect $ 'catStreamWrite `hasNoType` ''AT.FlattenState
inspect $ 'catStreamWrite `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_copy_fromBytes :: IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_copy_fromBytes href devNull _ =
    [ bgroup "copy/fromBytes"
        [ mkBench "rawToNull" href $ do
            Handles inh _ <- readIORef href
            readFromBytesNull devNull inh
        ]
    ]

-------------------------------------------------------------------------------
-- unfold exceptions
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
{-# INLINE readWriteOnExceptionUnfold #-}
readWriteOnExceptionUnfold :: Handle -> Handle -> IO ()
readWriteOnExceptionUnfold devNull inh =
    let readEx = IUF.onException (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteOnExceptionUnfold
-- inspect $ 'readWriteOnExceptionUnfold `hasNoType` ''Step
-- inspect $ 'readWriteOnExceptionUnfold `hasNoType` ''AT.FlattenState
-- inspect $ 'readWriteOnExceptionUnfold `hasNoType` ''D.ConcatMapUState
#endif

-- | Send the file contents to /dev/null with exception handling
{-# INLINE readWriteHandleExceptionUnfold #-}
readWriteHandleExceptionUnfold :: Handle -> Handle -> IO ()
readWriteHandleExceptionUnfold devNull inh =
    let handler (_e :: SomeException) = hClose inh >> return 10
        readEx = IUF.handle (IUF.singletonM handler) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteHandleExceptionUnfold
-- inspect $ 'readWriteHandleExceptionUnfold `hasNoType` ''Step
-- inspect $ 'readWriteHandleExceptionUnfold `hasNoType` ''AT.FlattenState
-- inspect $ 'readWriteHandleExceptionUnfold `hasNoType` ''D.ConcatMapUState
#endif

-- | Send the file contents to /dev/null with exception handling
{-# INLINE readWriteFinallyUnfold #-}
readWriteFinallyUnfold :: Handle -> Handle -> IO ()
readWriteFinallyUnfold devNull inh =
    let readEx = IUF.finally (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteFinallyUnfold
-- inspect $ 'readWriteFinallyUnfold `hasNoType` ''Step
-- inspect $ 'readWriteFinallyUnfold `hasNoType` ''AT.FlattenState
-- inspect $ 'readWriteFinallyUnfold `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE readWriteFinallyIOUnfold #-}
readWriteFinallyIOUnfold :: Handle -> Handle -> IO ()
readWriteFinallyIOUnfold devNull inh =
    let readEx = IUF.finallyIO (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

-- | Send the file contents to /dev/null with exception handling
{-# INLINE readWriteBracketUnfold #-}
readWriteBracketUnfold :: Handle -> Handle -> IO ()
readWriteBracketUnfold devNull inh =
    let readEx = IUF.bracket return (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteBracketUnfold
-- inspect $ 'readWriteBracketUnfold `hasNoType` ''Step
-- inspect $ 'readWriteBracketUnfold `hasNoType` ''AT.FlattenState
-- inspect $ 'readWriteBracketUnfold `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE readWriteBracketIOUnfold #-}
readWriteBracketIOUnfold :: Handle -> Handle -> IO ()
readWriteBracketIOUnfold devNull inh =
    let readEx = IUF.bracketIO return (\_ -> hClose inh) FH.read
    in S.fold (FH.write devNull) $ S.unfold readEx inh

o_1_space_copy_read_exceptions ::
    IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_copy_read_exceptions href devNull _ =
    [ bgroup "copy/read/exceptions"
       [ mkBench "UF.onException" href $ do
           Handles inh _ <- readIORef href
           readWriteOnExceptionUnfold devNull inh
       , mkBench "UF.handle" href $ do
           Handles inh _ <- readIORef href
           readWriteHandleExceptionUnfold devNull inh
       , mkBench "UF.finally" href $ do
           Handles inh _ <- readIORef href
           readWriteFinallyUnfold devNull inh
       , mkBench "UF.finallyIO" href $ do
           Handles inh _ <- readIORef href
           readWriteFinallyIOUnfold devNull inh
       , mkBench "UF.bracket" href $ do
           Handles inh _ <- readIORef href
           readWriteBracketUnfold devNull inh
       , mkBench "UF.bracketIO" href $ do
           Handles inh _ <- readIORef href
           readWriteBracketIOUnfold devNull inh
        ]
    ]

-------------------------------------------------------------------------------
-- stream exceptions
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
{-# INLINE readWriteOnExceptionStream #-}
readWriteOnExceptionStream :: Handle -> Handle -> IO ()
readWriteOnExceptionStream devNull inh =
    let readEx = S.onException (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) $ readEx

-- | Send the file contents to /dev/null with exception handling
{-# INLINE readWriteHandleExceptionStream #-}
readWriteHandleExceptionStream :: Handle -> Handle -> IO ()
readWriteHandleExceptionStream devNull inh =
    let handler (_e :: SomeException) = S.yieldM (hClose inh >> return 10)
        readEx = S.handle handler (S.unfold FH.read inh)
    in S.fold (FH.write devNull) $ readEx

-- | Send the file contents to /dev/null with exception handling
{-# INLINE readWriteFinallyStream #-}
readWriteFinallyStream :: Handle -> Handle -> IO ()
readWriteFinallyStream devNull inh =
    let readEx = S.finally (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) readEx

{-# INLINE readWriteFinallyIOStream #-}
readWriteFinallyIOStream :: Handle -> Handle -> IO ()
readWriteFinallyIOStream devNull inh =
    let readEx = IP.finallyIO (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) readEx

-- | Send the file contents to /dev/null with exception handling
{-# INLINE fromToBytesBracketStream #-}
fromToBytesBracketStream :: Handle -> Handle -> IO ()
fromToBytesBracketStream devNull inh =
    let readEx = S.bracket (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toBytes inh)
    in IFH.fromBytes devNull $ readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteBracketStream
-- inspect $ 'readWriteBracketStream `hasNoType` ''Step
#endif

{-# INLINE fromToBytesBracketIOStream #-}
fromToBytesBracketIOStream :: Handle -> Handle -> IO ()
fromToBytesBracketIOStream devNull inh =
    let readEx = IP.bracketIO (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toBytes inh)
    in IFH.fromBytes devNull $ readEx

o_1_space_copy_stream_exceptions ::
    IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_copy_stream_exceptions href devNull _ =
    [ bgroup "copy/read/exceptions"
       [ mkBench "S.onException" href $ do
           Handles inh _ <- readIORef href
           readWriteOnExceptionStream devNull inh
       , mkBench "S.handle" href $ do
           Handles inh _ <- readIORef href
           readWriteHandleExceptionStream devNull inh
       , mkBench "S.finally" href $ do
           Handles inh _ <- readIORef href
           readWriteFinallyStream devNull inh
       , mkBench "S.finallyIO" href $ do
           Handles inh _ <- readIORef href
           readWriteFinallyIOStream devNull inh
       ]
    , bgroup "copy/fromToBytes/exceptions"
       [ mkBench "S.bracket" href $ do
           Handles inh _ <- readIORef href
           fromToBytesBracketStream devNull inh
       , mkBench "S.bracketIO" href $ do
           Handles inh _ <- readIORef href
           fromToBytesBracketIOStream devNull inh
        ]
    ]

-------------------------------------------------------------------------------
-- copy with group/ungroup transformations
-------------------------------------------------------------------------------

-- | Lines and unlines
{-# INLINE copyChunksSplitInterposeSuffix #-}
copyChunksSplitInterposeSuffix :: Handle -> Handle -> IO ()
copyChunksSplitInterposeSuffix inh outh =
    S.fold (IFH.writeWithBufferOf (1024*1024) outh)
        $ AS.interposeSuffix 10
        $ AS.splitOnSuffix 10
        $ IFH.toChunksWithBufferOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'copyChunksSplitInterposeSuffix [''Storable]
-- inspect $ 'copyChunksSplitInterposeSuffix `hasNoType` ''Step
#endif

-- | Words and unwords
{-# INLINE copyChunksSplitInterpose #-}
copyChunksSplitInterpose :: Handle -> Handle -> IO ()
copyChunksSplitInterpose inh outh =
    S.fold (IFH.writeWithBufferOf (1024*1024) outh)
        $ AS.interpose 32
        -- XXX this is not correct word splitting combinator
        $ AS.splitOn 32
        $ IFH.toChunksWithBufferOf (1024*1024) inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'copyChunksSplitInterpose [''Storable]
-- inspect $ 'copyChunksSplitInterpose `hasNoType` ''Step
#endif

o_1_space_copy_toChunks_group_ungroup ::
    IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_copy_toChunks_group_ungroup href _ _ =
    [ bgroup "copy/toChunks/group-ungroup"
        [ mkBench "AS.interposeSuffix . AS.splitOnSuffix" href $ do
            Handles inh outh <- readIORef href
            copyChunksSplitInterposeSuffix inh outh
        , mkBench "AS.interpose . AS.splitOn" href $ do
            Handles inh outh <- readIORef href
            copyChunksSplitInterpose inh outh
        ]
    ]

-------------------------------------------------------------------------------
-- copy with group/ungroup transformations
-------------------------------------------------------------------------------

{-# INLINE linesUnlinesCopy #-}
linesUnlinesCopy :: Handle -> Handle -> IO ()
linesUnlinesCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUS.unlines IUF.fromList
      $ S.splitOnSuffix (== '\n') FL.toList
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

{-# INLINE linesUnlinesArrayWord8Copy #-}
linesUnlinesArrayWord8Copy :: Handle -> Handle -> IO ()
linesUnlinesArrayWord8Copy inh outh =
    S.fold (FH.write outh)
      $ IP.interposeSuffix 10 A.read
      $ S.splitOnSuffix (== 10) A.write
      $ S.unfold FH.read inh

-- XXX splitSuffixOn requires -funfolding-use-threshold=150 for better fusion
-- | Lines and unlines
{-# INLINE linesUnlinesArrayCharCopy #-}
linesUnlinesArrayCharCopy :: Handle -> Handle -> IO ()
linesUnlinesArrayCharCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUA.unlines
      $ IUA.lines
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'linesUnlinesArrayCharCopy [''Storable]
-- inspect $ 'linesUnlinesArrayCharCopy `hasNoType` ''Step
-- inspect $ 'linesUnlinesArrayCharCopy `hasNoType` ''AT.FlattenState
-- inspect $ 'linesUnlinesArrayCharCopy `hasNoType` ''D.ConcatMapUState
#endif

-- XXX to write this we need to be able to map decodeUtf8 on the A.read fold.
-- For that we have to write decodeUtf8 as a Pipe.
{-
{-# INLINE linesUnlinesArrayUtf8Copy #-}
linesUnlinesArrayUtf8Copy :: Handle -> Handle -> IO ()
linesUnlinesArrayUtf8Copy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IP.intercalate (A.fromList [10]) (pipe SS.decodeUtf8P A.read)
      $ S.splitOnSuffix (== '\n') (IFL.lmap SS.encodeUtf8 A.write)
      $ SS.decodeLatin1
      $ S.unfold FH.read inh
-}

foreign import ccall unsafe "u_iswspace"
  iswspace :: Int -> Int

-- Code copied from base/Data.Char to INLINE it
{-# INLINE isSpace #-}
isSpace                 :: Char -> Bool
-- isSpace includes non-breaking space
-- The magic 0x377 isn't really that magical. As of 2014, all the codepoints
-- at or below 0x377 have been assigned, so we shouldn't have to worry about
-- any new spaces appearing below there. It would probably be best to
-- use branchless ||, but currently the eqLit transformation will undo that,
-- so we'll do it like this until there's a way around that.
isSpace c
  | uc <= 0x377 = uc == 32 || uc - 0x9 <= 4 || uc == 0xa0
  | otherwise = iswspace (ord c) /= 0
  where
    uc = fromIntegral (ord c) :: Word

{-# INLINE isSp #-}
isSp :: Word8 -> Bool
isSp = isSpace . chr . fromIntegral

-- | Word, unwords and copy
{-# INLINE wordsUnwordsCopyWord8 #-}
wordsUnwordsCopyWord8 :: Handle -> Handle -> IO ()
wordsUnwordsCopyWord8 inh outh =
    S.fold (FH.write outh)
        $ IP.interposeSuffix 32 IUF.fromList
        $ S.wordsBy isSp FL.toList
        $ S.unfold FH.read inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsUnwordsCopyWord8
-- inspect $ 'wordsUnwordsCopyWord8 `hasNoType` ''Step
-- inspect $ 'wordsUnwordsCopyWord8 `hasNoType` ''D.ConcatMapUState
#endif

-- | Word, unwords and copy
{-# INLINE wordsUnwordsCopy #-}
wordsUnwordsCopy :: Handle -> Handle -> IO ()
wordsUnwordsCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUS.unwords IUF.fromList
      -- XXX This pipeline does not fuse with wordsBy but fuses with splitOn
      -- with -funfolding-use-threshold=300.  With wordsBy it does not fuse
      -- even with high limits for inlining and spec-constr ghc options. With
      -- -funfolding-use-threshold=400 it performs pretty well and there
      -- is no evidence in the core that a join point involving Step
      -- constructors is not getting inlined. Not being able to fuse at all in
      -- this case could be an unknown issue, need more investigation.
      $ S.wordsBy isSpace FL.toList
      -- -- $ S.splitOn isSpace FL.toList
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'wordsUnwordsCopy
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''Step
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''AT.FlattenState
-- inspect $ 'wordsUnwordsCopy `hasNoType` ''D.ConcatMapUState
#endif

{-# INLINE wordsUnwordsCharArrayCopy #-}
wordsUnwordsCharArrayCopy :: Handle -> Handle -> IO ()
wordsUnwordsCharArrayCopy inh outh =
    S.fold (FH.write outh)
      $ SS.encodeLatin1
      $ IUA.unwords
      $ IUA.words
      $ SS.decodeLatin1
      $ S.unfold FH.read inh

o_1_space_copy_read_group_ungroup ::
    IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_copy_read_group_ungroup href _ _ =
    [ bgroup "copy/read/group-ungroup"
        [ mkBench "US.unlines . S.splitOnSuffix ([Word8])" href $ do
            Handles inh outh <- readIORef href
            linesUnlinesCopy inh outh
        , mkBench "S.interposeSuffix . S.splitOnSuffix(Array Word8)" href $ do
            Handles inh outh <- readIORef href
            linesUnlinesArrayWord8Copy inh outh
        , mkBench "UA.unlines . UA.lines (Array Char)" href $ do
            Handles inh outh <- readIORef href
            linesUnlinesArrayCharCopy inh outh

        , mkBench "S.interposeSuffix . S.wordsBy ([Word8])" href $ do
            Handles inh outh <- readIORef href
            wordsUnwordsCopyWord8 inh outh
        , mkBench "US.unwords . S.wordsBy ([Char])" href $ do
            Handles inh outh <- readIORef href
            wordsUnwordsCopy inh outh
        , mkBench "UA.unwords . UA.words (Array Char)" href $ do
            Handles inh outh <- readIORef href
            wordsUnwordsCharArrayCopy inh outh
        ]
    ]

-------------------------------------------------------------------------------
-- reduce with splitting transformations
-------------------------------------------------------------------------------

lf :: Word8
lf = fromIntegral (ord '\n')

toarr :: String -> A.Array Word8
toarr = A.fromList . map (fromIntegral . ord)

-- | Split on line feed.
{-# INLINE splitOn #-}
splitOn :: Handle -> IO Int
splitOn inh =
    (S.length $ S.splitOn (== lf) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOn
inspect $ 'splitOn `hasNoType` ''Step
inspect $ 'splitOn `hasNoType` ''AT.FlattenState
inspect $ 'splitOn `hasNoType` ''D.ConcatMapUState
#endif

-- | Split suffix on line feed.
{-# INLINE splitOnSuffix #-}
splitOnSuffix :: Handle -> IO Int
splitOnSuffix inh =
    (S.length $ S.splitOnSuffix (== lf) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'splitOnSuffix
inspect $ 'splitOnSuffix `hasNoType` ''Step
inspect $ 'splitOnSuffix `hasNoType` ''AT.FlattenState
inspect $ 'splitOnSuffix `hasNoType` ''D.ConcatMapUState
#endif

-- | Split on line feed.
{-# INLINE parseManySepBy #-}
parseManySepBy :: Handle -> IO Int
parseManySepBy inh =
    (S.length $ IP.parseMany (PR.sliceSepBy (== lf) FL.drain)
                             (S.unfold FH.read inh)) -- >>= print

-- | Words by space
{-# INLINE wordsBy #-}
wordsBy :: Handle -> IO Int
wordsBy inh =
    (S.length $ S.wordsBy isSp FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'wordsBy
inspect $ 'wordsBy `hasNoType` ''Step
inspect $ 'wordsBy `hasNoType` ''AT.FlattenState
inspect $ 'wordsBy `hasNoType` ''D.ConcatMapUState
#endif

-- | Split on a word8 sequence.
{-# INLINE splitOnSeq #-}
splitOnSeq :: String -> Handle -> IO Int
splitOnSeq str inh =
    (S.length $ IP.splitOnSeq (toarr str) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSeq
-- inspect $ 'splitOnSeq `hasNoType` ''Step
-- inspect $ 'splitOnSeq `hasNoType` ''AT.FlattenState
-- inspect $ 'splitOnSeq `hasNoType` ''D.ConcatMapUState
#endif

-- | Split on suffix sequence.
{-# INLINE splitOnSuffixSeq #-}
splitOnSuffixSeq :: String -> Handle -> IO Int
splitOnSuffixSeq str inh =
    (S.length $ IP.splitOnSuffixSeq (toarr str) FL.drain
        $ S.unfold FH.read inh) -- >>= print

#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'splitOnSuffixSeq
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''Step
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''AT.FlattenState
-- inspect $ 'splitOnSuffixSeq `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_reduce_read_split ::
    IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_reduce_read_split href _ _ =
    [ bgroup "reduce/read"
        [ mkBench "S.parseMany (PR.sliceSepBy (== lf) FL.drain)" href $ do
            Handles inh _ <- readIORef href
            parseManySepBy inh
        , mkBench "S.wordsBy isSpace FL.drain" href $ do
            Handles inh _ <- readIORef href
            wordsBy inh
        , mkBench "S.splitOn (== lf) FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOn inh
        , mkBench "S.splitOnSuffix (== lf) FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSuffix inh
        , mkBench "S.splitOnSeq \"\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSeq "" inh
        , mkBench "S.splitOnSuffixSeq \"\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSuffixSeq "" inh
        , mkBench "S.splitOnSeq \"\\n\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSeq "\n" inh
        , mkBench "S.splitOnSuffixSeq \"\\n\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSuffixSeq "\n" inh
        , mkBench "S.splitOnSeq \"a\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSeq "a" inh
        , mkBench "S.splitOnSeq \"\\r\\n\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSeq "\r\n" inh
        , mkBench "S.splitOnSuffixSeq \"\\r\\n\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSuffixSeq "\r\n" inh
        , mkBench "S.splitOnSeq \"aa\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSeq "aa" inh
        , mkBench "S.splitOnSeq \"aaaa\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSeq "aaaa" inh
        , mkBench "S.splitOnSeq \"abcdefgh\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSeq "abcdefgh" inh
        , mkBench "S.splitOnSeq \"abcdefghi\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSeq "abcdefghi" inh
        , mkBench "S.splitOnSeq \"catcatcatcatcat\" FL.drain" href $ do
            Handles inh _ <- readIORef href
            splitOnSeq "catcatcatcatcat" inh
        , mkBench "S.splitOnSeq \"abcdefghijklmnopqrstuvwxyz\" FL.drain"
            href $ do
                Handles inh _ <- readIORef href
                splitOnSeq "abcdefghijklmnopqrstuvwxyz" inh
        , mkBench "S.splitOnSuffixSeq \"abcdefghijklmnopqrstuvwxyz\" FL.drain"
            href $ do
                Handles inh _ <- readIORef href
                splitOnSuffixSeq "abcdefghijklmnopqrstuvwxyz" inh
        ]
    ]

-- | Split on a character sequence.
{-# INLINE splitOnSeqUtf8 #-}
splitOnSeqUtf8 :: String -> Handle -> IO Int
splitOnSeqUtf8 str inh =
    (S.length $ IP.splitOnSeq (A.fromList str) FL.drain
        $ IUS.decodeUtf8ArraysLenient
        $ IFH.toChunks inh) -- >>= print

o_1_space_reduce_toChunks_split ::
    IORef Handles -> Handle -> Int -> [Benchmark]
o_1_space_reduce_toChunks_split href _ _ =
    [ bgroup "reduce/toChunks"
        [ mkBench ("S.splitOnSeqUtf8 \"abcdefgh\" FL.drain "
            ++ ". US.decodeUtf8ArraysLenient") href $ do
                Handles inh _ <- readIORef href
                splitOnSeqUtf8 "abcdefgh" inh
        , mkBench "S.splitOnSeqUtf8 \"abcdefghijklmnopqrstuvwxyz\" FL.drain"
            href $ do
                Handles inh _ <- readIORef href
                splitOnSeqUtf8 "abcdefghijklmnopqrstuvwxyz" inh
        ]
    ]

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

main :: IO ()
main = do
    (_, cfg, benches) <- parseCLIOpts defaultStreamSize

    -- XXX The file is cached, so if the older file was a different size it
    -- will not be correct
    --     fileSize = blockSize * blockCount

    r <- lookupEnv "Benchmark.FileSystem.Handle.InputFile"
    (_, big) <-
        case r of
            Just inFileName -> return (inFileName, inFileName)
            Nothing -> do
                -- XXX will this work on windows/msys?
                let cmd infile size =
                        "mkdir -p " ++ scratchDir
                            ++ "; test -e " ++ infile
                            ++ " || { echo \"creating input file " ++ infile
                            ++ "\" && dd if=/dev/random of=" ++ infile
                            ++ " bs=" ++ show blockSize
                            ++ " count=" ++ show (blockCount size)
                            ++ ";}"
                runProcess_ (shell (cmd inFileSmall smallFileSize))
                runProcess_ (shell (cmd inFileBig bigFileSize))
                return (inFileSmall, inFileBig)

    -- smallHandle <- openFile small ReadMode
    bigHandle <- openFile big ReadMode
    outHandle <- openFile outfile WriteMode
    devNull <- openFile "/dev/null" WriteMode

    -- ssize <- getFileSize small
    bsize <- getFileSize big

    ref <- newIORef $ Handles bigHandle outHandle
    runMode (mode cfg) cfg benches (allBenchmarks ref devNull (fromIntegral bsize))

    where

    allBenchmarks env devNull bsize =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [ o_1_space_read_chunked env devNull bsize
            , o_1_space_copy_chunked env devNull bsize
            , o_1_space_copy_exceptions_readChunks env devNull bsize
            , o_1_space_copy_exceptions_toChunks env devNull bsize

            , o_1_space_reduce_read env devNull bsize
            , o_1_space_reduce_toBytes env devNull bsize
            , o_1_space_reduce_read_grouped env devNull bsize
            , o_1_space_reduce_read_split env devNull bsize
            , o_1_space_reduce_toChunks_split env devNull bsize

            , o_1_space_copy_read env devNull bsize
            , o_1_space_copy_fromBytes env devNull bsize
            , o_1_space_copy_read_exceptions env devNull bsize
            , o_1_space_copy_stream_exceptions env devNull bsize
            , o_1_space_copy_read_group_ungroup env devNull bsize
            , o_1_space_copy_toChunks_group_ungroup env devNull bsize
            ]
        ]
