-- |
-- Module      : Stream.Exception
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

module Stream.Exception (benchmarks) where

import Control.Exception (SomeException, Exception, throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Foreign.Type (Array(..), byteLength)
import Streamly.Internal.System.IO (defaultChunkSize)
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import System.IO (Handle, hClose, hPutChar)

import qualified Data.IORef as Ref
import qualified Data.Map.Strict as Map

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

import Gauge hiding (env)
import Prelude hiding (last, length)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

#ifdef INSPECTION
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
    Stream.fold Fold.drain
        $ Stream.retry (Map.singleton BenchException1 length) (const Stream.nil) source

    where

    source = Stream.unfold IUF.enumerateFromTo (from, from + length)

retryNone :: Int -> Int -> IO ()
retryNone length from = do
    ref <- Ref.newIORef (0 :: Int)
    Stream.fold Fold.drain
        $ Stream.retry (Map.singleton BenchException1 length) (const Stream.nil)
        $ source ref

    where

    source ref =
        Stream.unfold (IUF.replicateM (from + length))
            $ Ref.modifyIORef' ref (+ 1) >> Ref.readIORef ref

retryAll :: Int -> Int -> IO ()
retryAll length from = do
    ref <- Ref.newIORef 0
    Stream.fold Fold.drain
        $ Stream.retry (Map.singleton BenchException1 (length + from)) (const Stream.nil)
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
    Stream.fold Fold.drain
        $ Stream.retry (Map.singleton BenchException1 length) (const source)
        $ throwIO BenchException2 `Stream.before` Stream.nil

    where

    source = Stream.unfold IUF.enumerateFromTo (from, from + length)


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

getChunks :: MonadIO m => Handle -> Stream.Stream m (Array Word8)
getChunks h = Stream.fromStreamD (D.Stream step ())

    where

    step _ _ = do
        arr <- IFH.getChunk defaultChunkSize h
        return $
            case byteLength arr of
                0 -> D.Stop
                _ -> D.Yield arr ()

{-# INLINE concatArr #-}
concatArr :: (Monad m) => Stream.Stream m (Array Word8) -> Stream.Stream m Word8
concatArr m = Stream.fromStreamD $ D.unfoldMany A.read (Stream.toStreamD m)

-- | Send the file contents to /dev/null with exception handling
fromToBytesBracket_Stream :: Handle -> Handle -> IO ()
fromToBytesBracket_Stream inh devNull =
    let readEx =
              SerialT
            $ Stream.toStreamK
            $ Stream.bracket_ (return ()) (\_ -> hClose inh)
                    (\_ -> concatArr $ getChunks inh)
     in IFH.putBytes devNull readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromToBytesBracket_Stream
#endif

fromToBytesBracketStream :: Handle -> Handle -> IO ()
fromToBytesBracketStream inh devNull =
    let readEx =
              SerialT
            $ Stream.toStreamK
            $ Stream.bracket (return ()) (\_ -> hClose inh)
                    (\_ -> concatArr $ getChunks inh)
        in IFH.putBytes devNull readEx

readWriteFinallyStream :: Handle -> Handle -> IO ()
readWriteFinallyStream inh devNull =
    let readEx = Stream.finally (hClose inh) (Stream.unfold FH.read inh)
    in Stream.fold (FH.write devNull) readEx

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
       [ mkBenchSmall "S.onException" env $ \inh _ ->
           readWriteOnExceptionStream inh (nullH env)
       , mkBenchSmall "S.handle" env $ \inh _ ->
           readWriteHandleExceptionStream inh (nullH env)
       , mkBenchSmall "S.finally_" env $ \inh _ ->
           readWriteFinally_Stream inh (nullH env)
       , mkBenchSmall "S.finally" env $ \inh _ ->
           readWriteFinallyStream inh (nullH env)
       , mkBenchSmall "S.after . S.before" env $ \inh _ ->
           readWriteBeforeAfterStream inh (nullH env)
       , mkBenchSmall "S.after" env $ \inh _ ->
           readWriteAfterStream inh (nullH env)
       , mkBenchSmall "S.after_" env $ \inh _ ->
           readWriteAfter_Stream inh (nullH env)
       ]
       , bgroup "exceptions/fromToBytes"
       [ mkBenchSmall "S.bracket_" env $ \inh _ ->
           fromToBytesBracket_Stream inh (nullH env)
       , mkBenchSmall "S.bracket" env $ \inh _ ->
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
            (\_ -> getChunks inh)
     in Stream.fold (IFH.writeChunks devNull) readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksBracket_
#endif

toChunksBracket :: Handle -> Handle -> IO ()
toChunksBracket inh devNull =
    let readEx = Stream.bracket
            (return ())
            (\_ -> hClose inh)
            (\_ -> getChunks inh)
    in Stream.fold (IFH.writeChunks devNull) readEx

o_1_space_copy_exceptions_toChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_toChunks env =
    [ bgroup "exceptions/toChunks"
        [ mkBench "S.bracket_" env $ \inH _ ->
            toChunksBracket_ inH (nullH env)
        , mkBench "S.bracket" env $ \inH _ ->
            toChunksBracket inH (nullH env)
        ]
    ]

benchmarks :: String -> BenchEnv -> Int -> [Benchmark]
benchmarks moduleName env size =
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_copy_exceptions_readChunks env
            , o_1_space_copy_exceptions_toChunks env
            , o_1_space_copy_stream_exceptions env
            , o_1_space_serial_exceptions size
            ]
        ]
