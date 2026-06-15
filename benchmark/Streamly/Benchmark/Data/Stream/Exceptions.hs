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

-- toChunks (bracketUnsafe wraps readChunks; Step constructors survive)
inspect $ hasNoTypeClasses 'toChunksBracket_
-- inspect $ 'toChunksBracket_ `hasNoType` ''Stream.Step
inspect $ 'toChunksBracket_ `hasNoType` ''FL.Step
inspect $ 'toChunksBracket_ `hasNoType` ''SPEC
#endif

benchmarks :: BenchEnv -> Int -> [(SpaceComplexity, Benchmark)]
benchmarks _env _size =
      [ (SpaceO_1, mkBench "Stream.bracket_ (toChunks)" _env $ \inH _ ->
            toChunksBracket_ inH (nullH _env))
      , (SpaceO_1, mkBenchSmall "Stream.onException" _env $ \inh _ ->
            readWriteOnExceptionStream inh (nullH _env))
      , (SpaceO_1, mkBenchSmall "Stream.handle" _env $ \inh _ ->
            readWriteHandleExceptionStream inh (nullH _env))
      , (SpaceO_1, mkBenchSmall "Stream.finally_" _env $ \inh _ ->
            readWriteFinally_Stream inh (nullH _env))
      , (SpaceO_1, mkBenchSmall "Stream.after_" _env $ \inh _ ->
            readWriteAfter_Stream inh (nullH _env))
      , (SpaceO_1, mkBenchSmall "Stream.bracket_ (fromToBytes)" _env $ \inh _ ->
            fromToBytesBracket_Stream inh (nullH _env))
      ]
