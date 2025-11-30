-- |
-- Module      : Stream.Exceptions
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Exceptions (benchmarks) where

import Control.Exception (Exception)

import Control.Exception (SomeException)
import System.IO (Handle, hClose)
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Control.Monad.Catch (MonadCatch)
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- stream exceptions
-------------------------------------------------------------------------------

data BenchException
    = BenchException1
    | BenchException2
    deriving (Show, Eq, Ord)

instance Exception BenchException

-- XXX Move these to FileSystem.Handle benchmarks

-------------------------------------------------------------------------------
-- copy stream exceptions
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
readWriteOnExceptionStream :: Handle -> Handle -> IO ()
readWriteOnExceptionStream inh devNull =
    let readEx = Stream.onException (hClose inh) (Stream.unfold FH.reader inh)
    in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteOnExceptionStream
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteHandleExceptionStream :: Handle -> Handle -> IO ()
readWriteHandleExceptionStream inh devNull =
    let handler (_e :: SomeException) =
            return $ Stream.fromEffect (hClose inh >> return 10)
        readEx = Stream.handle handler (Stream.unfold FH.reader inh)
    in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteHandleExceptionStream
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteFinally_Stream :: Handle -> Handle -> IO ()
readWriteFinally_Stream inh devNull =
    let readEx =
            Stream.finallyUnsafe (hClose inh) (Stream.unfold FH.reader inh)
    in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteFinally_Stream
#endif

-- | Send the file contents to /dev/null with exception handling
fromToBytesBracket_Stream :: Handle -> Handle -> IO ()
fromToBytesBracket_Stream inh devNull =
    let readEx = Stream.bracketUnsafe (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.read inh)
    in IFH.putBytes devNull readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromToBytesBracket_Stream
#endif

readWriteAfter_Stream :: Handle -> Handle -> IO ()
readWriteAfter_Stream inh devNull =
    let readEx = Stream.afterUnsafe (hClose inh) (Stream.unfold FH.reader inh)
     in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteAfter_Stream
inspect $ 'readWriteAfter_Stream `hasNoType` ''Stream.Step
#endif

o_1_space_copy_stream_exceptions :: BenchEnv -> [Benchmark]
o_1_space_copy_stream_exceptions env =
    [ bgroup "exceptions"
       [ mkBenchSmall "Stream.onException" env $ \inh _ ->
           readWriteOnExceptionStream inh (nullH env)
       , mkBenchSmall "Stream.handle" env $ \inh _ ->
           readWriteHandleExceptionStream inh (nullH env)
       , mkBenchSmall "Stream.finally_" env $ \inh _ ->
           readWriteFinally_Stream inh (nullH env)
       , mkBenchSmall "Stream.after_" env $ \inh _ ->
           readWriteAfter_Stream inh (nullH env)
       ]
    , bgroup "exceptions/fromToBytes"
       [ mkBenchSmall "Stream.bracket_" env $ \inh _ ->
           fromToBytesBracket_Stream inh (nullH env)
        ]
    ]

 -------------------------------------------------------------------------------
-- Exceptions readChunks
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
readChunksOnException :: Handle -> Handle -> IO ()
readChunksOnException inh devNull =
    let readEx = IUF.onException (\_ -> hClose inh) FH.chunkReader
    in IUF.fold (IFH.writeChunks devNull) readEx inh

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'readChunksOnException [''MonadCatch]
#else
inspect $ hasNoTypeClasses 'readChunksOnException
#endif
#endif

-- | Send the file contents to /dev/null with exception handling
readChunksBracket_ :: Handle -> Handle -> IO ()
readChunksBracket_ inh devNull =
    let readEx = IUF.bracket_ return (\_ -> hClose inh) FH.chunkReader
    in IUF.fold (IFH.writeChunks devNull) readEx inh

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'readChunksBracket_ [''MonadCatch]
#else
inspect $ hasNoTypeClasses 'readChunksBracket_
#endif
#endif

o_1_space_copy_exceptions_readChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_readChunks env =
    [ bgroup "exceptions/readChunks"
        [ mkBench "UF.onException" env $ \inH _ ->
            readChunksOnException inH (nullH env)
        , mkBench "UF.bracket_" env $ \inH _ ->
            readChunksBracket_ inH (nullH env)
        ]
    ]

-------------------------------------------------------------------------------
-- Exceptions toChunks
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
toChunksBracket_ :: Handle -> Handle -> IO ()
toChunksBracket_ inh devNull =
    let readEx = Stream.bracketUnsafe
            (return ())
            (\_ -> hClose inh)
            (\_ -> IFH.readChunks inh)
    in Stream.fold (IFH.writeChunks devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksBracket_
#endif

o_1_space_copy_exceptions_toChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_toChunks env =
    [ bgroup "exceptions/toChunks"
        [ mkBench "Stream.bracket_" env $ \inH _ ->
            toChunksBracket_ inH (nullH env)
        ]
    ]

benchmarks :: String -> BenchEnv -> Int -> [Benchmark]
benchmarks moduleName _env _size =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_copy_exceptions_readChunks _env
            , o_1_space_copy_exceptions_toChunks _env
            , o_1_space_copy_stream_exceptions _env
            ]
        ]
