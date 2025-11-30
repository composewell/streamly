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

module Main (main) where

import Control.Exception (Exception, throwIO)
import Stream.Common (drain)

import qualified Data.IORef as Ref
import qualified Data.Map.Strict as Map

import System.IO (Handle, hClose, hPutChar)
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.Data.Unfold.Prelude as IUF

import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Stream

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
        Stream.replicateM (from + length)
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

-------------------------------------------------------------------------------
-- copy stream exceptions
-------------------------------------------------------------------------------

readWriteFinallyStream :: Handle -> Handle -> IO ()
readWriteFinallyStream inh devNull =
    let readEx = Stream.finally (hClose inh) (Stream.unfold FH.reader inh)
    in Stream.fold (FH.write devNull) readEx

fromToBytesBracketStream :: Handle -> Handle -> IO ()
fromToBytesBracketStream inh devNull =
    let readEx = Stream.bracket (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.read inh)
    in IFH.putBytes devNull readEx

readWriteBeforeAfterStream :: Handle -> Handle -> IO ()
readWriteBeforeAfterStream inh devNull =
    let readEx =
            Stream.after (hClose inh)
                $ Stream.before (hPutChar devNull 'A') (Stream.unfold FH.reader inh)
     in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ 'readWriteBeforeAfterStream `hasNoType` ''Stream.Step
#endif

readWriteAfterStream :: Handle -> Handle -> IO ()
readWriteAfterStream inh devNull =
    let readEx = Stream.after (hClose inh) (Stream.unfold FH.reader inh)
     in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ 'readWriteAfterStream `hasNoType` ''Stream.Step
#endif

o_1_space_copy_stream_exceptions :: BenchEnv -> [Benchmark]
o_1_space_copy_stream_exceptions env =
    [ bgroup "exceptions"
       [ mkBenchSmall "Stream.finally" env $ \inh _ ->
           readWriteFinallyStream inh (nullH env)
       , mkBenchSmall "Stream.after . Stream.before" env $ \inh _ ->
           readWriteBeforeAfterStream inh (nullH env)
       , mkBenchSmall "Stream.after" env $ \inh _ ->
           readWriteAfterStream inh (nullH env)
       ]
    , bgroup "exceptions/fromToBytes"
       [ mkBenchSmall "Stream.bracket" env $ \inh _ ->
           fromToBytesBracketStream inh (nullH env)
        ]
    ]

 -------------------------------------------------------------------------------
-- Exceptions readChunks
-------------------------------------------------------------------------------

readChunksBracket :: Handle -> Handle -> IO ()
readChunksBracket inh devNull =
    let readEx = IUF.bracket return (\_ -> hClose inh) FH.chunkReader
    in IUF.fold (IFH.writeChunks devNull) readEx inh

o_1_space_copy_exceptions_readChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_readChunks env =
    [ bgroup "exceptions/readChunks"
        [ mkBench "UF.bracket" env $ \inH _ ->
            readChunksBracket inH (nullH env)
        ]
    ]

-------------------------------------------------------------------------------
-- Exceptions toChunks
-------------------------------------------------------------------------------

toChunksBracket :: Handle -> Handle -> IO ()
toChunksBracket inh devNull =
    let readEx = Stream.bracket
            (return ())
            (\_ -> hClose inh)
            (\_ -> IFH.readChunks inh)
    in Stream.fold (IFH.writeChunks devNull) readEx

o_1_space_copy_exceptions_toChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_toChunks env =
    [ bgroup "exceptions/toChunks"
        [ mkBench "Stream.bracket" env $ \inH _ ->
            toChunksBracket inH (nullH env)
        ]
    ]

moduleName :: String
moduleName = "Data.Stream.Prelude.Exceptions"

benchmarks :: BenchEnv -> Int -> [Benchmark]
benchmarks env size =
    [ bgroup (o_1_space_prefix moduleName) $ concat
        [ o_1_space_serial_exceptions size
        , o_1_space_copy_exceptions_readChunks env
        , o_1_space_copy_exceptions_toChunks env
        , o_1_space_copy_stream_exceptions env
        ]
    ]

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (benchmarks env)
