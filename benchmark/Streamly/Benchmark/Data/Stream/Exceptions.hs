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

import Control.Exception (Exception, throwIO)
import Stream.Common (drain)
import Streamly.Internal.Data.Stream (Stream)

import qualified Data.IORef as Ref
import qualified Data.Map.Strict as Map
import qualified Stream.Common as Common

#ifndef USE_STREAMLY_CORE
import Control.Exception (SomeException)
import System.IO (Handle, hClose, hPutChar)
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Data.Unfold as IUF
#endif

#ifdef USE_PRELUDE
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
#else
import qualified Streamly.Internal.Data.Stream as Stream
#endif
import qualified Streamly.Data.Stream.Exceptions.Lifted as Stream

import Gauge hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
import Test.Inspection

import qualified Streamly.Internal.Data.Stream.StreamD as D
#endif

-------------------------------------------------------------------------------
-- stream exceptions
-------------------------------------------------------------------------------

{-# INLINE replicateM #-}
replicateM :: Common.MonadAsync m => Int -> m a -> Stream m a
#ifdef USE_PRELUDE
replicateM = Stream.replicateM
#else
replicateM n = Stream.sequence . Stream.replicate n
#endif

data BenchException
    = BenchException1
    | BenchException2
    deriving (Show, Eq, Ord)

instance Exception BenchException

retryNoneSimple :: Int -> Int -> IO ()
retryNoneSimple length from =
    drain
        $ Stream.retry
            (Map.singleton BenchException1 length)
            (const Stream.nil)
            source

    where

    source = Stream.enumerateFromTo from (from + length)

retryNone :: Int -> Int -> IO ()
retryNone length from = do
    ref <- Ref.newIORef (0 :: Int)
    drain
        $ Stream.retry (Map.singleton BenchException1 length) (const Stream.nil)
        $ source ref

    where

    source ref =
        replicateM (from + length)
            $ Ref.modifyIORef' ref (+ 1) >> Ref.readIORef ref

retryAll :: Int -> Int -> IO ()
retryAll length from = do
    ref <- Ref.newIORef 0
    drain
        $ Stream.retry
            (Map.singleton BenchException1 (length + from)) (const Stream.nil)
        $ source ref

    where

    source ref =
        Stream.fromEffect
            $ do
                Ref.modifyIORef' ref (+ 1)
                val <- Ref.readIORef ref
                if val >= length
                then return length
                else throwIO BenchException1

retryUnknown :: Int -> Int -> IO ()
retryUnknown length from = do
    drain
        $ Stream.retry (Map.singleton BenchException1 length) (const source)
        $ throwIO BenchException2 `Stream.before` Stream.nil

    where

    source = Stream.enumerateFromTo from (from + length)


o_1_space_serial_exceptions :: Int -> [Benchmark]
o_1_space_serial_exceptions length =
    [ bgroup
          "exceptions/serial"
          [ benchIOSrc1 "retryNoneSimple" (retryNoneSimple length)
          , benchIOSrc1 "retryNone" (retryNone length)
          , benchIOSrc1 "retryAll" (retryAll length)
          , benchIOSrc1 "retryUnknown" (retryUnknown length)
          ]
    ]

-- XXX Move these to FileSystem.Handle benchmarks
#ifndef USE_STREAMLY_CORE

-------------------------------------------------------------------------------
-- copy stream exceptions
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
readWriteOnExceptionStream :: Handle -> Handle -> IO ()
readWriteOnExceptionStream inh devNull =
    let readEx = Stream.onException (hClose inh) (Stream.unfold FH.read inh)
    in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteOnExceptionStream
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteHandleExceptionStream :: Handle -> Handle -> IO ()
readWriteHandleExceptionStream inh devNull =
    let handler (_e :: SomeException) = Stream.fromEffect (hClose inh >> return 10)
        readEx = Stream.handle handler (Stream.unfold FH.read inh)
    in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteHandleExceptionStream
#endif

-- | Send the file contents to /dev/null with exception handling
readWriteFinally_Stream :: Handle -> Handle -> IO ()
readWriteFinally_Stream inh devNull =
    let readEx = Stream.finally_ (hClose inh) (Stream.unfold FH.read inh)
    in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteFinally_Stream
#endif

readWriteFinallyStream :: Handle -> Handle -> IO ()
readWriteFinallyStream inh devNull =
    let readEx = Stream.finally (hClose inh) (Stream.unfold FH.read inh)
    in Stream.fold (FH.write devNull) readEx

-- | Send the file contents to /dev/null with exception handling
fromToBytesBracket_Stream :: Handle -> Handle -> IO ()
fromToBytesBracket_Stream inh devNull =
    let readEx = Stream.bracket_ (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.getBytes inh)
    in IFH.putBytes devNull readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromToBytesBracket_Stream
#endif

fromToBytesBracketStream :: Handle -> Handle -> IO ()
fromToBytesBracketStream inh devNull =
    let readEx = Stream.bracket (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.getBytes inh)
    in IFH.putBytes devNull readEx

readWriteBeforeAfterStream :: Handle -> Handle -> IO ()
readWriteBeforeAfterStream inh devNull =
    let readEx =
            Stream.after (hClose inh)
                $ Stream.before (hPutChar devNull 'A') (Stream.unfold FH.read inh)
     in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ 'readWriteBeforeAfterStream `hasNoType` ''D.Step
#endif

readWriteAfterStream :: Handle -> Handle -> IO ()
readWriteAfterStream inh devNull =
    let readEx = Stream.after (hClose inh) (Stream.unfold FH.read inh)
     in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ 'readWriteAfterStream `hasNoType` ''D.Step
#endif

readWriteAfter_Stream :: Handle -> Handle -> IO ()
readWriteAfter_Stream inh devNull =
    let readEx = Stream.after_ (hClose inh) (Stream.unfold FH.read inh)
     in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readWriteAfter_Stream
inspect $ 'readWriteAfter_Stream `hasNoType` ''D.Step
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
       , mkBenchSmall "Stream.finally" env $ \inh _ ->
           readWriteFinallyStream inh (nullH env)
       , mkBenchSmall "Stream.after . Stream.before" env $ \inh _ ->
           readWriteBeforeAfterStream inh (nullH env)
       , mkBenchSmall "Stream.after" env $ \inh _ ->
           readWriteAfterStream inh (nullH env)
       , mkBenchSmall "Stream.after_" env $ \inh _ ->
           readWriteAfter_Stream inh (nullH env)
       ]
    , bgroup "exceptions/fromToBytes"
       [ mkBenchSmall "Stream.bracket_" env $ \inh _ ->
           fromToBytesBracket_Stream inh (nullH env)
       , mkBenchSmall "Stream.bracket" env $ \inh _ ->
           fromToBytesBracketStream inh (nullH env)
        ]
    ]

 -------------------------------------------------------------------------------
-- Exceptions readChunks
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
readChunksOnException :: Handle -> Handle -> IO ()
readChunksOnException inh devNull =
    let readEx = IUF.onException (\_ -> hClose inh) FH.readChunks
    in IUF.fold (IFH.writeChunks devNull) readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readChunksOnException
#endif

-- | Send the file contents to /dev/null with exception handling
readChunksBracket_ :: Handle -> Handle -> IO ()
readChunksBracket_ inh devNull =
    let readEx = IUF.bracket_ return (\_ -> hClose inh) FH.readChunks
    in IUF.fold (IFH.writeChunks devNull) readEx inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readChunksBracket_
#endif

readChunksBracket :: Handle -> Handle -> IO ()
readChunksBracket inh devNull =
    let readEx = IUF.bracket return (\_ -> hClose inh) FH.readChunks
    in IUF.fold (IFH.writeChunks devNull) readEx inh

o_1_space_copy_exceptions_readChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_readChunks env =
    [ bgroup "exceptions/readChunks"
        [ mkBench "UF.onException" env $ \inH _ ->
            readChunksOnException inH (nullH env)
        , mkBench "UF.bracket_" env $ \inH _ ->
            readChunksBracket_ inH (nullH env)
        , mkBench "UF.bracket" env $ \inH _ ->
            readChunksBracket inH (nullH env)
        ]
    ]

-------------------------------------------------------------------------------
-- Exceptions toChunks
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
toChunksBracket_ :: Handle -> Handle -> IO ()
toChunksBracket_ inh devNull =
    let readEx = Stream.bracket_
            (return ())
            (\_ -> hClose inh)
            (\_ -> IFH.getChunks inh)
    in Stream.fold (IFH.writeChunks devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksBracket_
#endif

toChunksBracket :: Handle -> Handle -> IO ()
toChunksBracket inh devNull =
    let readEx = Stream.bracket
            (return ())
            (\_ -> hClose inh)
            (\_ -> IFH.getChunks inh)
    in Stream.fold (IFH.writeChunks devNull) readEx

o_1_space_copy_exceptions_toChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_toChunks env =
    [ bgroup "exceptions/toChunks"
        [ mkBench "Stream.bracket_" env $ \inH _ ->
            toChunksBracket_ inH (nullH env)
        , mkBench "Stream.bracket" env $ \inH _ ->
            toChunksBracket inH (nullH env)
        ]
    ]

#endif

benchmarks :: String -> BenchEnv -> Int -> [Benchmark]
benchmarks moduleName _env size =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_serial_exceptions size
#ifndef USE_STREAMLY_CORE
            , o_1_space_copy_exceptions_readChunks _env
            , o_1_space_copy_exceptions_toChunks _env
            , o_1_space_copy_stream_exceptions _env
#endif
            ]
        ]
