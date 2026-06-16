--
-- Module      : CrossModule.FileSystem
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Benchmarks that combine operations from multiple library modules, primarily
-- to test fusion and performance across module boundaries (e.g. file I/O
-- fused with stream folds and chunking operations).

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module CrossModule.FileSystem (benchmarks) where

import Data.Functor.Identity (runIdentity)
import Data.Word (Word8)
import GHC.Magic (inline)
import GHC.Magic (noinline)
import System.IO (Handle)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream as IP
import qualified Streamly.Data.Stream as S

import Test.Tasty.Bench hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Streamly.Internal.Data.MutByteArray (Unbox)
import Streamly.Internal.Data.Stream (Step(..), FoldMany)

import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Producer as Producer

import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Handle read
-------------------------------------------------------------------------------

-- | Get the last byte from a file bytestream.
readLast :: Handle -> IO (Maybe Word8)
readLast = S.fold Fold.latest . S.unfold FH.reader

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readLast
inspect $ 'readLast `hasNoType` ''Step -- S.unfold
inspect $ 'readLast `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
inspect $ 'readLast `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Count the number of bytes in a file.
readCountBytes :: Handle -> IO Int
readCountBytes = S.fold Fold.length . S.unfold FH.reader

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readCountBytes
inspect $ 'readCountBytes `hasNoType` ''Step -- S.unfold
inspect $ 'readCountBytes `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
inspect $ 'readCountBytes `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-- | Sum the bytes in a file.
readSumBytes :: Handle -> IO Word8
readSumBytes = S.fold Fold.sum . S.unfold FH.reader

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readSumBytes
inspect $ 'readSumBytes `hasNoType` ''Step
inspect $ 'readSumBytes `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
inspect $ 'readSumBytes `hasNoType` ''MutArray.ArrayUnsafe  -- FH.read/A.read
#endif

-------------------------------------------------------------------------------
-- reduce after grouping in chunks
-------------------------------------------------------------------------------

chunksOfSum :: Int -> Handle -> IO Int
chunksOfSum n inh =
    S.fold Fold.length $ IP.groupsOf n FL.sum (S.unfold FH.reader inh)

foldMany1ChunksOfSum :: Int -> Handle -> IO Int
foldMany1ChunksOfSum n inh =
    S.fold Fold.length
        $ IP.foldManyPost (FL.take n FL.sum) (S.unfold FH.reader inh)

foldManyChunksOfSum :: Int -> Handle -> IO Int
foldManyChunksOfSum n inh =
    S.fold Fold.length
        $ IP.foldMany (FL.take n FL.sum) (S.unfold FH.reader inh)

parseManyChunksOfSum :: Int -> Handle -> IO Int
parseManyChunksOfSum n inh =
    S.fold Fold.length
        $ IP.parseMany (PR.fromFold $ FL.take n FL.sum) (S.unfold FH.reader inh)

-- XXX investigate why we need an INLINE in this case (GHC)
-- Even though allocations remain the same in both cases inlining improves time
-- by 4x.
-- | Slice in chunks of size n and get the count of chunks.
{-# INLINE groupsOf #-}
groupsOf :: Int -> Handle -> IO Int
groupsOf n inh =
    -- writeNUnsafe gives 2.5x boost here over writeN.
    S.fold Fold.length
        $ IP.groupsOf n (A.unsafeCreateOf n) (S.unfold FH.reader inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'groupsOf
inspect $ 'groupsOf `hasNoType` ''Step
inspect $ 'groupsOf `hasNoType` ''FoldMany
inspect $ 'groupsOf `hasNoType` ''MutArray.ArrayUnsafe -- AT.writeNUnsafe
                                                 -- FH.read/A.read
inspect $ 'groupsOf `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
#endif

{-# INLINE chunksOf #-}
chunksOf :: Int -> Handle -> IO Int
chunksOf n inh =
    S.fold Fold.length $ A.chunksOf n (S.unfold FH.reader inh)

-------------------------------------------------------------------------------
-- read chunked using toChunks
-------------------------------------------------------------------------------

-- | Get the last byte from a file bytestream.
toChunksLast :: Handle -> IO (Maybe Word8)
toChunksLast inh = do
    let s = FH.readChunks inh
    larr <- IP.last s
    return $ case larr of
        Nothing -> Nothing
        Just arr -> A.getIndex (A.length arr - 1) arr

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksLast
inspect $ 'toChunksLast `hasNoType` ''Step
#endif

-- | Count the number of bytes in a file.
toChunksSumLengths :: Handle -> IO Int
toChunksSumLengths inh =
    let s = FH.readChunks inh
    in IP.fold Fold.sum (IP.map A.length s)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSumLengths
inspect $ 'toChunksSumLengths `hasNoType` ''Step
#endif

-- | Sum the bytes in a file.
toChunksCountBytes :: Handle -> IO Word8
toChunksCountBytes inh = do
    let foldlArr' f z = runIdentity . IP.foldl' f z . A.read
    let s = FH.readChunks inh
    IP.foldl' (\acc arr -> acc + foldlArr' (+) 0 arr) 0 s

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksCountBytes
inspect $ 'toChunksCountBytes `hasNoType` ''Step
#endif

-------------------------------------------------------------------------------
-- Splitting
-------------------------------------------------------------------------------

-- | Count the number of lines in a file.
toChunksSplitOnSuffix :: Handle -> IO Int
toChunksSplitOnSuffix =
    IP.fold Fold.length
        . A.compactEndByByte_ 10
        . FH.readChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSplitOnSuffix
inspect $ 'toChunksSplitOnSuffix `hasNoType` ''Step
#endif

-- | Count the number of words in a file.
toChunksSplitOn :: Handle -> IO Int
toChunksSplitOn =
    IP.fold Fold.length
        . A.compactSepByByte_ 32
        . FH.readChunks

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksSplitOn
inspect $ 'toChunksSplitOn `hasNoType` ''Step
#endif

-------------------------------------------------------------------------------
-- copy with group/ungroup transformations
-------------------------------------------------------------------------------

-- | Lines and unlines
{-# NOINLINE copyChunksSplitInterposeSuffix #-}
copyChunksSplitInterposeSuffix :: Handle -> Handle -> IO ()
copyChunksSplitInterposeSuffix inh outh =
    IP.fold (FH.write outh)
        $ A.concatEndBy 10 . A.compactEndByByte_ 10
        $ FH.readChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'copyChunksSplitInterposeSuffix [''Unbox]
inspect $ 'copyChunksSplitInterposeSuffix `hasNoType` ''Step
#endif

-- | Words and unwords
{-# NOINLINE copyChunksSplitInterpose #-}
copyChunksSplitInterpose :: Handle -> Handle -> IO ()
copyChunksSplitInterpose inh outh =
    IP.fold (FH.write outh)
        -- XXX requires @-fspec-constr-recursive=12@.
        -- XXX this is not correct word splitting combinator
        $ A.concatSepBy 32 . A.compactSepByByte_ 32
        $ FH.readChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClassesExcept 'copyChunksSplitInterpose [''Unbox]
inspect $ 'copyChunksSplitInterpose `hasNoType` ''Step
#endif

benchmarks :: BenchEnv -> [(SpaceComplexity, Benchmark)]
benchmarks env =
    map (\b -> (SpaceO_1, b))
        [ mkBench "Fold.latest" env $ \inh _ ->
            readLast inh
        , mkBench "Fold.sum" env $ \inh _ ->
            readSumBytes inh
        , mkBench "Fold.length (wc -c)" env $ \inh _ ->
            readCountBytes inh

        -- XXX all these require @-fspec-constr-recursive=12@.
        , mkBench ("Stream.groupsOf " ++ show (bigSize env) ++ " . Fold.sum") env $
            \inh _ ->
                chunksOfSum (bigSize env) inh
        , mkBench "Stream.groupsOf 1 . Fold.sum" env $ \inh _ ->
            chunksOfSum 1 inh

        -- XXX investigate why we need inline/noinline in these cases (GHC)
        , mkBench
            ("Stream.foldManyPost " ++ show (bigSize env) ++ " . Fold.sum")
            env
            $ \inh _ -> noinline foldMany1ChunksOfSum (bigSize env) inh
        , mkBench
            "Stream.foldManyPost 1 . Fold.sum"
            env
            $ \inh _ -> inline foldMany1ChunksOfSum 1 inh
        , mkBench
            ("Stream.foldMany " ++ show (bigSize env) ++ " . Fold.sum")
            env
            $ \inh _ -> noinline foldManyChunksOfSum (bigSize env) inh
        , mkBench
            "Stream.foldMany 1 . Fold.sum"
            env
            $ \inh _ -> inline foldManyChunksOfSum 1 inh

        -- parseMany with file IO
        , mkBench
            ("Stream.parseMany (Fold.take " ++ show (bigSize env) ++ " Fold.sum)")
            env
            $ \inh _ -> noinline parseManyChunksOfSum (bigSize env) inh
        , mkBench
            "Stream.parseMany (Fold.take 1 Fold.sum)"
            env
            $ \inh _ -> inline parseManyChunksOfSum 1 inh

        -- folding chunks to arrays
        , mkBenchSmall "Stream.groupsOf 1 . Array.unsafeCreateOf" env $ \inh _ ->
            groupsOf 1 inh
        , mkBench "Stream.groupsOf 10 . Array.unsafeCreateOf" env $ \inh _ ->
            groupsOf 10 inh
        , mkBench "Stream.groupsOf 1000 . Array.unsafeCreateOf" env $ \inh _ ->
            groupsOf 1000 inh

        -- chunksOf may use a different impl than groupsOf
        , mkBenchSmall "Array.chunksOf 1" env $ \inh _ ->
            chunksOf 1 inh
        , mkBench "Array.chunksOf 10" env $ \inh _ ->
            chunksOf 10 inh
        , mkBench "Array.chunksOf 1000" env $ \inh _ ->
            chunksOf 1000 inh

        -- read chunked using toChunks
        , mkBench "Stream.last" env $ \inh _ ->
            toChunksLast inh
        -- Note: this cannot be fairly compared with GNU wc -c or wc -m as
        -- wc uses lseek to just determine the file size rather than reading
        -- and counting characters.
        , mkBench "Stream.sum . Stream.map Array.length" env $ \inh _ ->
            toChunksSumLengths inh
        , mkBench "splitOnSuffix" env $ \inh _ ->
            toChunksSplitOnSuffix inh
        , mkBench "splitOn" env $ \inh _ ->
            toChunksSplitOn inh
        , mkBench "countBytes" env $ \inh _ ->
            toChunksCountBytes inh

        -- copy with group/ungroup transformations
        , mkBench "interposeSuffix . splitOnSuffix" env $ \inh outh ->
            copyChunksSplitInterposeSuffix inh outh
        , mkBenchSmall "interpose . splitOn" env $ \inh outh ->
            copyChunksSplitInterpose inh outh
        ]
