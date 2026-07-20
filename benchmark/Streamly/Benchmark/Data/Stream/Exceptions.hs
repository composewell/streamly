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
onException_CopyFileBytes :: Handle -> Handle -> IO ()
onException_CopyFileBytes inh devNull =
    let readEx = Stream.onException (hClose inh) (Stream.unfold FH.reader inh)
    in Stream.fold (FH.write devNull) readEx

-- | Send the file contents to /dev/null with exception handling
handle_CopyFileBytes :: Handle -> Handle -> IO ()
handle_CopyFileBytes inh devNull =
    let handler (_e :: SomeException) =
            return $ Stream.fromEffect (hClose inh >> return 10)
        readEx = Stream.handle handler (Stream.unfold FH.reader inh)
    in Stream.fold (FH.write devNull) readEx

-- | Send the file contents to /dev/null with exception handling
finallyUnsafe_CopyFileBytes :: Handle -> Handle -> IO ()
finallyUnsafe_CopyFileBytes inh devNull =
    let readEx =
            Stream.finallyUnsafe (hClose inh) (Stream.unfold FH.reader inh)
    in Stream.fold (FH.write devNull) readEx

-- | Send the file contents to /dev/null with exception handling
bracketUnsafe_CopyFileBytes :: Handle -> Handle -> IO ()
bracketUnsafe_CopyFileBytes inh devNull =
    let readEx = Stream.bracketUnsafe (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.read inh)
    in IFH.putBytes devNull readEx

afterUnsafe_CopyFileBytes :: Handle -> Handle -> IO ()
afterUnsafe_CopyFileBytes inh devNull =
    let readEx = Stream.afterUnsafe (hClose inh) (Stream.unfold FH.reader inh)
     in Stream.fold (FH.write devNull) readEx

 -------------------------------------------------------------------------------
-- Exceptions toChunks
-------------------------------------------------------------------------------

-- | Send the file contents to /dev/null with exception handling
bracketUnsafe_CopyFileChunks :: Handle -> Handle -> IO ()
bracketUnsafe_CopyFileChunks inh devNull =
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
inspect $ hasNoTypeClasses 'onException_CopyFileBytes
-- inspect $ 'onException_CopyFileBytes `hasNoType` ''Stream.Step
inspect $ 'onException_CopyFileBytes `hasNoType` ''FL.Step
inspect $ 'onException_CopyFileBytes `hasNoType` ''SPEC

-- handle provides an alternative stream on exception; Step survives.
inspect $ hasNoTypeClasses 'handle_CopyFileBytes
-- inspect $ 'handle_CopyFileBytes `hasNoType` ''Stream.Step
inspect $ 'handle_CopyFileBytes `hasNoType` ''FL.Step
inspect $ 'handle_CopyFileBytes `hasNoType` ''SPEC

inspect $ hasNoTypeClasses 'finallyUnsafe_CopyFileBytes
-- inspect $ 'finallyUnsafe_CopyFileBytes `hasNoType` ''Stream.Step
inspect $ 'finallyUnsafe_CopyFileBytes `hasNoType` ''FL.Step
inspect $ 'finallyUnsafe_CopyFileBytes `hasNoType` ''SPEC

inspect $ hasNoTypeClasses 'bracketUnsafe_CopyFileBytes
-- inspect $ 'bracketUnsafe_CopyFileBytes `hasNoType` ''Stream.Step
inspect $ 'bracketUnsafe_CopyFileBytes `hasNoType` ''FL.Step
inspect $ 'bracketUnsafe_CopyFileBytes `hasNoType` ''SPEC

-- afterUnsafe runs a cleanup action after the stream ends with no try/catch
-- around the stream body, so Step constructors are fully eliminated.
inspect $ hasNoTypeClasses 'afterUnsafe_CopyFileBytes
inspect $ 'afterUnsafe_CopyFileBytes `hasNoType` ''Stream.Step
inspect $ 'afterUnsafe_CopyFileBytes `hasNoType` ''FL.Step
inspect $ 'afterUnsafe_CopyFileBytes `hasNoType` ''SPEC

-- toChunks (bracketUnsafe wraps readChunks; Step constructors survive)
inspect $ hasNoTypeClasses 'bracketUnsafe_CopyFileChunks
-- inspect $ 'bracketUnsafe_CopyFileChunks `hasNoType` ''Stream.Step
inspect $ 'bracketUnsafe_CopyFileChunks `hasNoType` ''FL.Step
inspect $ 'bracketUnsafe_CopyFileChunks `hasNoType` ''SPEC
#endif

benchmarks :: BenchEnv -> Int -> [(SpaceComplexity, Benchmark)]
benchmarks _env _size =
      [ (SpaceO_1, mkBench "bracketUnsafe_CopyFileChunks" _env $ \inH _ ->
            bracketUnsafe_CopyFileChunks inH (nullH _env))
      , (SpaceO_1, mkBenchSmall "onException_CopyFileBytes" _env $ \inh _ ->
            onException_CopyFileBytes inh (nullH _env))
      , (SpaceO_1, mkBenchSmall "handle_CopyFileBytes" _env $ \inh _ ->
            handle_CopyFileBytes inh (nullH _env))
      , (SpaceO_1, mkBenchSmall "finallyUnsafe_CopyFileBytes" _env $ \inh _ ->
            finallyUnsafe_CopyFileBytes inh (nullH _env))
      , (SpaceO_1, mkBenchSmall "afterUnsafe_CopyFileBytes" _env $ \inh _ ->
            afterUnsafe_CopyFileBytes inh (nullH _env))
      , (SpaceO_1, mkBenchSmall "bracketUnsafe_CopyFileBytes" _env $ \inh _ ->
            bracketUnsafe_CopyFileBytes inh (nullH _env))
      ]
