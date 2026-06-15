--
-- Module      : Streamly.Benchmark.CrossModule
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
import qualified Streamly.Data.Stream.Prelude as S

import Test.Tasty.Bench hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Streamly.Internal.Data.Stream (Step(..), FoldMany)

import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.Producer as Producer

import Test.Inspection
#endif

moduleName :: String
moduleName = "CrossModule"

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

allBenchmarks :: BenchEnv -> [Benchmark]
allBenchmarks env =
    [ bgroup (o_1_space_prefix moduleName)
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
        ]
    ]

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    defaultMain (allBenchmarks env)
