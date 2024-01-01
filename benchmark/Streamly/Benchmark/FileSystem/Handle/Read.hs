{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Benchmark.FileSystem.Handle
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
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

module Handle.Read
    (allBenchmarks)
where

import Data.Word (Word8)
import GHC.Magic (inline)
import GHC.Magic (noinline)
import System.IO (Handle)

import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream as IP
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Unicode.Stream as IUS
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.Unicode.Stream as SS

import Test.Tasty.Bench hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Streamly.Internal.Data.Stream (Step(..), FoldMany)

import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.Unfold as IUF

import Test.Inspection
#endif

-- TBD reading with unfold

-------------------------------------------------------------------------------
-- unfold read
-------------------------------------------------------------------------------

-- | Get the last byte from a file bytestream.
readLast :: Handle -> IO (Maybe Word8)
readLast = S.fold Fold.last . S.unfold FH.reader

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readLast
inspect $ 'readLast `hasNoType` ''Step -- S.unfold
inspect $ 'readLast `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'readLast `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- assert that flattenArrays constructors are not present
-- | Count the number of bytes in a file.
readCountBytes :: Handle -> IO Int
readCountBytes = S.fold Fold.length . S.unfold FH.reader

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readCountBytes
inspect $ 'readCountBytes `hasNoType` ''Step -- S.unfold
inspect $ 'readCountBytes `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'readCountBytes `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Count the number of lines in a file.
readCountLines :: Handle -> IO Int
readCountLines =
    S.fold Fold.length
        . IUS.lines FL.drain
        . SS.decodeLatin1
        . S.unfold FH.reader

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readCountLines
inspect $ 'readCountLines `hasNoType` ''Step
inspect $ 'readCountLines `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'readCountLines `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Count the number of words in a file.
readCountWords :: Handle -> IO Int
readCountWords =
    S.fold Fold.length
        . IUS.words FL.drain
        . SS.decodeLatin1
        . S.unfold FH.reader

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readCountWords
-- inspect $ 'readCountWords `hasNoType` ''Step
#endif

-- | Sum the bytes in a file.
readSumBytes :: Handle -> IO Word8
readSumBytes = S.fold Fold.sum . S.unfold FH.reader

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readSumBytes
inspect $ 'readSumBytes `hasNoType` ''Step
inspect $ 'readSumBytes `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'readSumBytes `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- XXX When we mark this with INLINE and we have two benchmarks using S.drain
-- in one benchmark group then somehow GHC ends up delaying the inlining of
-- readDrain. Since S.drain has an INLINE[2] for proper rule firing, that does
-- not work well because of delyaed inlining and the code does not fuse. We
-- need some way of propagating the inline phase information up so that we can
-- expedite inlining of the callers too automatically. The minimal example for
-- the problem can be created by using just two benchmarks in a bench group
-- both using "readDrain". Either GHC should be fixed or we can use
-- fusion-plugin to propagate INLINE phase information such that this problem
-- does not occur.
readDrain :: Handle -> IO ()
readDrain inh = S.fold Fold.drain $ S.unfold FH.reader inh

-- XXX investigate why we need an INLINE in this case (GHC)
{-# INLINE readDecodeLatin1 #-}
readDecodeLatin1 :: Handle -> IO ()
readDecodeLatin1 inh =
   S.fold Fold.drain
     $ SS.decodeLatin1
     $ S.unfold FH.reader inh

readDecodeUtf8 :: Handle -> IO ()
readDecodeUtf8 inh =
   S.fold Fold.drain
     $ SS.decodeUtf8
     $ S.unfold FH.reader inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readDecodeUtf8
-- inspect $ 'readDecodeUtf8Lax `hasNoType` ''Step
#endif

o_1_space_reduce_read :: BenchEnv -> [Benchmark]
o_1_space_reduce_read env =
    [ bgroup "reduce/read"
        [ -- read raw bytes without any decoding
          mkBench "S.drain" env $ \inh _ ->
            readDrain inh
        , mkBench "S.last" env $ \inh _ ->
            readLast inh
        , mkBench "S.sum" env $ \inh _ ->
            readSumBytes inh

        -- read with Latin1 decoding
        , mkBench "SS.decodeLatin1" env $ \inh _ ->
            readDecodeLatin1 inh
        , mkBench "S.length" env $ \inh _ ->
            readCountBytes inh
        , mkBench "US.lines . SS.decodeLatin1" env $ \inh _ ->
            readCountLines inh
        , mkBench "US.words . SS.decodeLatin1" env $ \inh _ ->
            readCountWords inh

        -- read with utf8 decoding
        , mkBenchSmall "SS.decodeUtf8" env $ \inh _ ->
            readDecodeUtf8 inh
        ]
    ]

-------------------------------------------------------------------------------
-- stream toBytes
-------------------------------------------------------------------------------

-- | Count the number of lines in a file.
getChunksConcatUnfoldCountLines :: Handle -> IO Int
getChunksConcatUnfoldCountLines inh =
    S.fold Fold.length
        $ IUS.lines FL.drain
        $ SS.decodeLatin1
        -- XXX replace with toBytes
        $ S.unfoldMany A.reader (IFH.readChunks inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'getChunksConcatUnfoldCountLines
inspect $ 'getChunksConcatUnfoldCountLines `hasNoType` ''Step
inspect $ 'getChunksConcatUnfoldCountLines `hasNoType` ''D.ConcatMapUState
#endif

o_1_space_reduce_toBytes :: BenchEnv -> [Benchmark]
o_1_space_reduce_toBytes env =
    [ bgroup "reduce/toBytes"
        [ mkBench "US.lines . SS.decodeLatin1" env $ \inh _ ->
            getChunksConcatUnfoldCountLines inh
        ]
    ]

-------------------------------------------------------------------------------
-- reduce after grouping in chunks
-------------------------------------------------------------------------------

chunksOfSum :: Int -> Handle -> IO Int
chunksOfSum n inh =
    S.fold Fold.length $ IP.groupsOf n FL.sum (S.unfold FH.reader inh)

foldManyPostChunksOfSum :: Int -> Handle -> IO Int
foldManyPostChunksOfSum n inh =
    S.fold Fold.length
        $ IP.foldManyPost (FL.take n FL.sum) (S.unfold FH.reader inh)

foldManyChunksOfSum :: Int -> Handle -> IO Int
foldManyChunksOfSum n inh =
    S.fold Fold.length
        $ IP.foldMany (FL.take n FL.sum) (S.unfold FH.reader inh)

-- XXX investigate why we need an INLINE in this case (GHC)
-- Even though allocations remain the same in both cases inlining improves time
-- by 4x.
-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE groupsOf #-}
groupsOf :: Int -> Handle -> IO Int
groupsOf n inh =
    -- writeNUnsafe gives 2.5x boost here over writeN.
    S.fold Fold.length
        $ IP.groupsOf n (A.writeNUnsafe n) (S.unfold FH.reader inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groupsOf
inspect $ 'groupsOf `hasNoType` ''Step
inspect $ 'groupsOf `hasNoType` ''FoldMany
inspect $ 'groupsOf `hasNoType` ''MutArray.ArrayUnsafe -- AT.writeNUnsafe
                                                 -- FH.read/A.read
inspect $ 'groupsOf `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
#endif

{-# INLINE chunksOf #-}
chunksOf :: Int -> Handle -> IO Int
chunksOf n inh =
    S.fold Fold.length $ Stream.chunksOf n (S.unfold FH.reader inh)

o_1_space_reduce_read_grouped :: BenchEnv -> [Benchmark]
o_1_space_reduce_read_grouped env =
    [ bgroup "reduce/read/chunks"
        [ mkBench ("S.groupsOf " ++ show (bigSize env) ++  " FL.sum") env $
            \inh _ ->
                chunksOfSum (bigSize env) inh
        , mkBench "S.groupsOf 1 FL.sum" env $ \inh _ ->
            chunksOfSum 1 inh

        -- XXX investigate why we need inline/noinline in these cases (GHC)
        -- Chunk using parsers
        , mkBench
            ("S.foldManyPost (FL.take " ++ show (bigSize env) ++ " FL.sum)")
            env
            $ \inh _ -> noinline foldManyPostChunksOfSum (bigSize env) inh
        , mkBench
            "S.foldManyPost (FL.take 1 FL.sum)"
            env
            $ \inh _ -> inline foldManyPostChunksOfSum 1 inh
        , mkBench
            ("S.foldMany (FL.take " ++ show (bigSize env) ++ " FL.sum)")
            env
            $ \inh _ -> noinline foldManyChunksOfSum (bigSize env) inh
        , mkBench
            "S.foldMany (FL.take 1 FL.sum)"
            env
            $ \inh _ -> inline foldManyChunksOfSum 1 inh

        -- folding chunks to arrays
        , mkBenchSmall "S.groupsOf 1" env $ \inh _ ->
            groupsOf 1 inh
        , mkBench "S.groupsOf 10" env $ \inh _ ->
            groupsOf 10 inh
        , mkBench "S.groupsOf 1000" env $ \inh _ ->
            groupsOf 1000 inh

        -- chunksOf may use a different impl than groupsOf
        , mkBenchSmall "S.chunksOf 1" env $ \inh _ ->
            chunksOf 1 inh
        , mkBench "S.chunksOf 10" env $ \inh _ ->
            chunksOf 10 inh
        , mkBench "S.chunksOf 1000" env $ \inh _ ->
            chunksOf 1000 inh
        ]
    ]

allBenchmarks :: BenchEnv -> [Benchmark]
allBenchmarks env = Prelude.concat
    [ o_1_space_reduce_read env
    , o_1_space_reduce_toBytes env
    , o_1_space_reduce_read_grouped env
    ]
