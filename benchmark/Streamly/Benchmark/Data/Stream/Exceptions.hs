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
import qualified Streamly.Internal.Data.Fold as FL
import GHC.Types (SPEC(..))
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

-- | Send the file contents to /dev/null with exception handling
readWriteHandleExceptionStream :: Handle -> Handle -> IO ()
readWriteHandleExceptionStream inh devNull =
    let handler (_e :: SomeException) =
            return $ Stream.fromEffect (hClose inh >> return 10)
        readEx = Stream.handle handler (Stream.unfold FH.reader inh)
    in Stream.fold (FH.write devNull) readEx

-- | Send the file contents to /dev/null with exception handling
readWriteFinally_Stream :: Handle -> Handle -> IO ()
readWriteFinally_Stream inh devNull =
    let readEx =
            Stream.finallyUnsafe (hClose inh) (Stream.unfold FH.reader inh)
    in Stream.fold (FH.write devNull) readEx

-- | Send the file contents to /dev/null with exception handling
fromToBytesBracket_Stream :: Handle -> Handle -> IO ()
fromToBytesBracket_Stream inh devNull =
    let readEx = Stream.bracketUnsafe (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.read inh)
    in IFH.putBytes devNull readEx

readWriteAfter_Stream :: Handle -> Handle -> IO ()
readWriteAfter_Stream inh devNull =
    let readEx = Stream.afterUnsafe (hClose inh) (Stream.unfold FH.reader inh)
     in Stream.fold (FH.write devNull) readEx

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

-- | Send the file contents to /dev/null with exception handling
readChunksBracket_ :: Handle -> Handle -> IO ()
readChunksBracket_ inh devNull =
    let readEx = IUF.bracket_ return (\_ -> hClose inh) FH.chunkReader
    in IUF.fold (IFH.writeChunks devNull) readEx inh

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

-------------------------------------------------------------------------------
-- Inspection
-------------------------------------------------------------------------------

#ifdef INSPECTION
-- stream exceptions
-- onException/finallyUnsafe/bracketUnsafe wrap the stream in a try/catch,
-- keeping Either SomeException (Step ...) in the exception path; Step survives.
inspect $ hasNoTypeClasses 'readWriteOnExceptionStream
-- inspect $ 'readWriteOnExceptionStream `hasNoType` ''Stream.Step
inspect $ 'readWriteOnExceptionStream `hasNoType` ''FL.Step
inspect $ 'readWriteOnExceptionStream `hasNoType` ''SPEC

-- handle provides an alternative stream on exception; Step survives.
inspect $ hasNoTypeClasses 'readWriteHandleExceptionStream
-- inspect $ 'readWriteHandleExceptionStream `hasNoType` ''Stream.Step
inspect $ 'readWriteHandleExceptionStream `hasNoType` ''FL.Step
inspect $ 'readWriteHandleExceptionStream `hasNoType` ''SPEC

inspect $ hasNoTypeClasses 'readWriteFinally_Stream
-- inspect $ 'readWriteFinally_Stream `hasNoType` ''Stream.Step
inspect $ 'readWriteFinally_Stream `hasNoType` ''FL.Step
inspect $ 'readWriteFinally_Stream `hasNoType` ''SPEC

inspect $ hasNoTypeClasses 'fromToBytesBracket_Stream
-- inspect $ 'fromToBytesBracket_Stream `hasNoType` ''Stream.Step
inspect $ 'fromToBytesBracket_Stream `hasNoType` ''FL.Step
inspect $ 'fromToBytesBracket_Stream `hasNoType` ''SPEC

-- afterUnsafe runs a cleanup action after the stream ends with no try/catch
-- around the stream body, so Step constructors are fully eliminated.
inspect $ hasNoTypeClasses 'readWriteAfter_Stream
inspect $ 'readWriteAfter_Stream `hasNoType` ''Stream.Step
inspect $ 'readWriteAfter_Stream `hasNoType` ''FL.Step
inspect $ 'readWriteAfter_Stream `hasNoType` ''SPEC

-- readChunks (unfold-based; exception path keeps Step constructors)
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'readChunksOnException [''MonadCatch]
#else
inspect $ hasNoTypeClasses 'readChunksOnException
#endif
-- inspect $ 'readChunksOnException `hasNoType` ''Stream.Step
inspect $ 'readChunksOnException `hasNoType` ''FL.Step
inspect $ 'readChunksOnException `hasNoType` ''SPEC

#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'readChunksBracket_ [''MonadCatch]
#else
inspect $ hasNoTypeClasses 'readChunksBracket_
#endif
-- inspect $ 'readChunksBracket_ `hasNoType` ''Stream.Step
inspect $ 'readChunksBracket_ `hasNoType` ''FL.Step
inspect $ 'readChunksBracket_ `hasNoType` ''SPEC

-- toChunks (bracketUnsafe wraps readChunks; Step constructors survive)
inspect $ hasNoTypeClasses 'toChunksBracket_
-- inspect $ 'toChunksBracket_ `hasNoType` ''Stream.Step
inspect $ 'toChunksBracket_ `hasNoType` ''FL.Step
inspect $ 'toChunksBracket_ `hasNoType` ''SPEC
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
