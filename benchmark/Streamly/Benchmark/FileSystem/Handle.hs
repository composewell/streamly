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

import System.IO (Handle)
import Streamly.Internal.System.IO (defaultChunkSize)

import qualified Streamly.Data.Array as A
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as S
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.FileSystem.Handle as IFH

import Test.Tasty.Bench hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Streamly.Internal.Data.Stream (Step(..))

import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.Tuple.Strict as Strict
import qualified Streamly.Internal.Data.MutArray as MutArray

import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Handle read
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- copy unfold
-------------------------------------------------------------------------------

-- | Copy file
copyStream :: Handle -> Handle -> IO ()
copyStream inh outh = S.fold (FH.write outh) (S.unfold FH.reader inh)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'copyStream
inspect $ 'copyStream `hasNoType` ''Step -- S.unfold
inspect $ 'copyStream `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
inspect $ 'copyStream `hasNoType` ''MutArray.ArrayUnsafe -- FH.write/writeNUnsafe
                                                   -- FH.read/A.read
inspect $ 'copyStream `hasNoType` ''Strict.Tuple3' -- FH.write/chunksOf
#endif

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
    unf = IUF.unfoldEach A.reader FH.chunkReader

-- | Send the chunk content to /dev/null
-- Implicitly benchmarked via 'readWithFromBytesNull'
_readChunksWith :: Handle -> Handle -> IO ()
_readChunksWith inh devNull = IUF.fold fld unf (defaultChunkSize, inh)

    where

    fld = FH.write devNull
    unf = IUF.unfoldEach A.reader FH.chunkReaderWith

-- | Send the file contents ('defaultChunkSize') to /dev/null
writeReadWith :: Handle -> Handle -> IO ()
writeReadWith inh devNull = IUF.fold fld unf (defaultChunkSize, inh)

    where

    fld = FH.writeWith defaultChunkSize devNull
    unf = FH.readerWith

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'writeReadWith
inspect $ 'writeReadWith `hasNoType` ''Step
inspect $ 'writeReadWith `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
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
inspect $ 'writeRead `hasNoType` ''Producer.ConcatState -- FH.read/UF.many
inspect $ 'writeRead `hasNoType` ''MutArray.ArrayUnsafe -- FH.write/writeNUnsafe
                                                  -- FH.read/A.read
#endif

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "FileSystem.Handle"

allBenchmarks :: BenchEnv -> [Benchmark]
allBenchmarks env =
    -- read raw bytes without any decoding
    [ mkBench "Fold.drain" env $ \inh _ ->
        readDrain inh

    , mkBench "Handle.readChunks . Handle.writeChunks" env $ \inH _ ->
        copyChunks inH (nullH env)
    , mkBench "Handle.readChunks . Handle.writeChunks (cat)" env $ \inH outH ->
        copyChunks inH outH

    , mkBench "Handle.reader . Handle.write" env $ \inh _ ->
        copyStream inh (nullH env)
    , mkBench "Handle.reader . Handle.write (cat)" env $ \inh outh ->
        copyStream inh outh

    , mkBench "Handle.reader . Handle.putBytes" env $ \inh _ ->
        readFromBytesNull inh (nullH env)
    , mkBench "Handle.readerWith . Handle.putBytes" env $ \inh _ ->
        readWithFromBytesNull inh (nullH env)

    , mkBench "Handle.reader . Handle.write (Unfold.fold)" env $ \inh _ ->
        writeRead inh (nullH env)
    , mkBench "Handle.readerWith . Handle.writeWith (Unfold.fold)" env $ \inh _ ->
        writeReadWith inh (nullH env)
    ]

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    defaultMain
        [ bgroup (o_1_space_prefix moduleName) (allBenchmarks env)
        ]
