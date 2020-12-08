-- |
-- Module      : Streamly.Benchmark.Prelude.Serial.Exceptions
-- Copyright   : (c) 2020 Composewell Technologies
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

module Serial.Exceptions
    (allBenchmarks)
where

import Control.Exception (SomeException)
import System.IO (Handle, hClose, hPutChar)
import Prelude hiding (last, length)

import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Data.Stream.IsStream as IP
import qualified Streamly.Prelude as S

import Gauge hiding (env)
-- import Handle.Common
import Streamly.Benchmark.CommonH

#ifdef INSPECTION
import Foreign.Storable (Storable)
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))

import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Tuple.Strict as Strict
import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as AT

import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- stream exceptions
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
readWriteOnExceptionStream :: Handle -> Handle -> IO ()
readWriteOnExceptionStream inh devNull =
    let readEx = S.onException (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) $ readEx

-- | Send the file contents to /dev/null with exception handling
readWriteHandleExceptionStream :: Handle -> Handle -> IO ()
readWriteHandleExceptionStream inh devNull =
    let handler (_e :: SomeException) = S.yieldM (hClose inh >> return 10)
        readEx = S.handle handler (S.unfold FH.read inh)
    in S.fold (FH.write devNull) $ readEx

-- | Send the file contents to /dev/null with exception handling
readWriteFinally_Stream :: Handle -> Handle -> IO ()
readWriteFinally_Stream inh devNull =
    let readEx = IP.finally_ (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) readEx

readWriteFinallyStream :: Handle -> Handle -> IO ()
readWriteFinallyStream inh devNull =
    let readEx = S.finally (hClose inh) (S.unfold FH.read inh)
    in S.fold (FH.write devNull) readEx

-- | Send the file contents to /dev/null with exception handling
fromToBytesBracket_Stream :: Handle -> Handle -> IO ()
fromToBytesBracket_Stream inh devNull =
    let readEx = IP.bracket_ (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toBytes inh)
    in IFH.fromBytes devNull $ readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'fromToBytesBracket_Stream
-- inspect $ 'fromToBytesBracketStream `hasNoType` ''Step
#endif

fromToBytesBracketStream :: Handle -> Handle -> IO ()
fromToBytesBracketStream inh devNull =
    let readEx = S.bracket (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.toBytes inh)
    in IFH.fromBytes devNull $ readEx

readWriteBeforeAfterStream :: Handle -> Handle -> IO ()
readWriteBeforeAfterStream inh devNull =
    let readEx =
            IP.after (hClose inh)
                $ IP.before (hPutChar devNull 'A') (S.unfold FH.read inh)
     in S.fold (FH.write devNull) readEx

readWriteAfterStream :: Handle -> Handle -> IO ()
readWriteAfterStream inh devNull =
    let readEx = IP.after (hClose inh) (S.unfold FH.read inh)
     in S.fold (FH.write devNull) readEx

readWriteAfter_Stream :: Handle -> Handle -> IO ()
readWriteAfter_Stream inh devNull =
    let readEx = IP.after_ (hClose inh) (S.unfold FH.read inh)
     in S.fold (FH.write devNull) readEx

o_1_space_copy_stream_exceptions :: BenchEnv -> [Benchmark]
o_1_space_copy_stream_exceptions env =
    [ bgroup "copy/read/exceptions"
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
    , bgroup "copy/fromToBytes/exceptions"
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
    in IUF.fold readEx (IFH.writeChunks devNull) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readChunksOnException
-- inspect $ 'readChunksOnException `hasNoType` ''Step
#endif

-- | Send the file contents to /dev/null with exception handling
readChunksBracket_ :: Handle -> Handle -> IO ()
readChunksBracket_ inh devNull =
    let readEx = IUF.bracket_ return (\_ -> hClose inh) FH.readChunks
    in IUF.fold readEx (IFH.writeChunks devNull) inh

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'readChunksBracket_
-- inspect $ 'readChunksBracket `hasNoType` ''Step
#endif

readChunksBracket :: Handle -> Handle -> IO ()
readChunksBracket inh devNull =
    let readEx = IUF.bracket return (\_ -> hClose inh) FH.readChunks
    in IUF.fold readEx (IFH.writeChunks devNull) inh

o_1_space_copy_exceptions_readChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_readChunks env =
    [ bgroup "copy/readChunks/exceptions"
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
    let readEx = IP.bracket_
            (return ())
            (\_ -> hClose inh)
            (\_ -> IFH.toChunks inh)
    in S.fold (IFH.writeChunks devNull) $ readEx

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'toChunksBracket_
-- inspect $ 'toChunksBracket `hasNoType` ''Step
#endif

toChunksBracket :: Handle -> Handle -> IO ()
toChunksBracket inh devNull =
    let readEx = S.bracket
            (return ())
            (\_ -> hClose inh)
            (\_ -> IFH.toChunks inh)
    in S.fold (IFH.writeChunks devNull) $ readEx

o_1_space_copy_exceptions_toChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_toChunks env =
    [ bgroup "copy/toChunks/exceptions"
        [ mkBench "S.bracket_" env $ \inH _ ->
            toChunksBracket_ inH (nullH env)
        , mkBench "S.bracket" env $ \inH _ ->
            toChunksBracket inH (nullH env)
        ]
    ]


allBenchmarks :: BenchEnv -> [Benchmark]
allBenchmarks env = Prelude.concat
    [ o_1_space_copy_exceptions_readChunks env
    , o_1_space_copy_exceptions_toChunks env
    , o_1_space_copy_stream_exceptions env
    ]
