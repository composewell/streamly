{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Benchmark.FileSystem.Handle
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3-3-Clause
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

module Handle.ReadWrite
    (allBenchmarks)
where

import System.IO (Handle)
import Prelude hiding (last, length)
import Streamly.Internal.System.IO (defaultChunkSize)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Data.Array as A
import qualified Streamly.Data.Stream.Prelude as S

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Streamly.Internal.Data.Stream (Step(..))

import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.Tuple.Strict as Strict
import qualified Streamly.Internal.Data.MutArray as MutArray

import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- copy chunked
-------------------------------------------------------------------------------

-- | Copy file
copyChunks :: Handle -> Handle -> IO ()
copyChunks inh outh = S.fold (IFH.writeChunks outh) $ IFH.readChunks inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyChunks
inspect $ 'copyChunks `hasNoType` ''Step
#endif

o_1_space_copy_chunked :: BenchEnv -> [Benchmark]
o_1_space_copy_chunked env =
    [ bgroup "copy/getChunks"
        [ mkBench "toNull" env $ \inH _ ->
            copyChunks inH (nullH env)
        , mkBench "raw" env $ \inH outH ->
            copyChunks inH outH
        ]
    ]

-------------------------------------------------------------------------------
-- copy unfold
-------------------------------------------------------------------------------

-- | Copy file
copyStream :: Handle -> Handle -> IO ()
copyStream inh outh = S.fold (FH.write outh) (S.unfold FH.reader inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStream
inspect $ 'copyStream `hasNoType` ''Step -- S.unfold
inspect $ 'copyStream `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'copyStream `hasNoType` ''MutArray.ArrayUnsafe -- FH.write/writeNUnsafe
                                                   -- FH.read/A.read
inspect $ 'copyStream `hasNoType` ''Strict.Tuple3' -- FH.write/chunksOf
#endif

o_1_space_copy_read :: BenchEnv -> [Benchmark]
o_1_space_copy_read env =
    [ bgroup "copy/read"
        [ mkBench "rawToNull" env $ \inh _ ->
            copyStream inh (nullH env)
        , mkBench "rawToFile" env $ \inh outh ->
            copyStream inh outh
        ]
    ]

-------------------------------------------------------------------------------
-- copy stream
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null
readFromBytesNull :: Handle -> Handle -> IO ()
readFromBytesNull inh devNull = IFH.putBytes devNull $ S.unfold FH.reader inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readFromBytesNull
inspect $ 'readFromBytesNull `hasNoType` ''Step
inspect $ 'readFromBytesNull `hasNoType` ''MutArray.SpliceState
inspect $ 'readFromBytesNull `hasNoType` ''MutArray.ArrayUnsafe -- FH.fromBytes/S.chunksOf
inspect $ 'readFromBytesNull `hasNoType` ''D.FoldMany
#endif

-- | Send the file contents ('defaultChunkSize') to /dev/null
readWithFromBytesNull :: Handle -> Handle -> IO ()
readWithFromBytesNull inh devNull =
    IFH.putBytes devNull
        $ S.unfold FH.readerWith (defaultChunkSize, inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWithFromBytesNull
inspect $ 'readWithFromBytesNull `hasNoType` ''Step
inspect $ 'readWithFromBytesNull `hasNoType` ''MutArray.SpliceState
inspect $ 'readWithFromBytesNull `hasNoType` ''MutArray.ArrayUnsafe -- FH.fromBytes/S.chunksOf
inspect $ 'readWithFromBytesNull `hasNoType` ''D.FoldMany
#endif

-- | Send the chunk content ('defaultChunkSize') to /dev/null
-- Implicitly benchmarked via 'readFromBytesNull'
_readChunks :: Handle -> Handle -> IO ()
_readChunks inh devNull = IUF.fold fld unf inh

    where

    fld = FH.write devNull
    unf = IUF.many A.reader FH.chunkReader

-- | Send the chunk content to /dev/null
-- Implicitly benchmarked via 'readWithFromBytesNull'
_readChunksWith :: Handle -> Handle -> IO ()
_readChunksWith inh devNull = IUF.fold fld unf (defaultChunkSize, inh)

    where

    fld = FH.write devNull
    unf = IUF.many A.reader FH.chunkReaderWith

o_1_space_copy_fromBytes :: BenchEnv -> [Benchmark]
o_1_space_copy_fromBytes env =
    [ bgroup "copy/putBytes"
        [ mkBench "rawToNull" env $ \inh _ ->
            readFromBytesNull inh (nullH env)
        , mkBench "FH.readWith" env $ \inh _ ->
            readWithFromBytesNull inh (nullH env)
        ]
    ]

-- | Send the file contents ('defaultChunkSize') to /dev/null
writeReadWith :: Handle -> Handle -> IO ()
writeReadWith inh devNull = IUF.fold fld unf (defaultChunkSize, inh)

    where

    fld = FH.writeWith defaultChunkSize devNull
    unf = FH.readerWith

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'writeReadWith
inspect $ 'writeReadWith `hasNoType` ''Step
inspect $ 'writeReadWith `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'writeReadWith `hasNoType` ''MutArray.ArrayUnsafe -- FH.write/writeNUnsafe
                                                      -- FH.read/A.read
#endif

-- | Send the file contents ('AT.defaultChunkSize') to /dev/null
writeRead :: Handle -> Handle -> IO ()
writeRead inh devNull = IUF.fold fld unf inh

    where

    fld = FH.write devNull
    unf = FH.reader

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'writeRead
inspect $ 'writeRead `hasNoType` ''Step
inspect $ 'writeRead `hasNoType` ''IUF.ConcatState -- FH.read/UF.many
inspect $ 'writeRead `hasNoType` ''MutArray.ArrayUnsafe -- FH.write/writeNUnsafe
                                                  -- FH.read/A.read
#endif

o_1_space_copy :: BenchEnv -> [Benchmark]
o_1_space_copy env =
    [ bgroup "copy"
        [ mkBench "FH.write . FH.read" env $ \inh _ ->
            writeRead inh (nullH env)
        , mkBench "FH.writeWith . FH.readWith" env $ \inh _ ->
            writeReadWith inh (nullH env)
        ]
    ]

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

allBenchmarks :: BenchEnv -> [Benchmark]
allBenchmarks env = Prelude.concat
    [ o_1_space_copy_chunked env
    , o_1_space_copy_read env
    , o_1_space_copy_fromBytes env
    , o_1_space_copy env
    ]
