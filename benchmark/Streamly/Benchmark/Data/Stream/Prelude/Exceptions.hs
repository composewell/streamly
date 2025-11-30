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
import Data.HashMap.Strict (HashMap)
import Data.Proxy (Proxy(..))
import Stream.Common (drain, benchIOSink)
import Streamly.Internal.Data.IsMap.HashMap ()
import Streamly.Internal.Data.Stream (Stream)
import System.IO (Handle, hClose, hPutChar)

import qualified Data.IORef as Ref
import qualified Data.Map.Strict as Map

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.Data.Unfold.Prelude as IUF

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

excBenchmarks :: BenchEnv -> Int -> [Benchmark]
excBenchmarks env size =
    [ bgroup (o_1_space_prefix moduleName) $ concat
        [ o_1_space_serial_exceptions size
        , o_1_space_copy_exceptions_readChunks env
        , o_1_space_copy_exceptions_toChunks env
        , o_1_space_copy_stream_exceptions env
        ]
    ]

{-# INLINE pollCounts #-}
pollCounts :: Stream IO Int -> IO ()
pollCounts = drain . Stream.parTapCount (const True) f

    where

    f = Stream.drain . Stream.rollingMap2 (-) . Stream.delayPost 1

{-# INLINE takeInterval #-}
takeInterval :: Double -> Stream IO Int -> IO ()
takeInterval i = drain . Stream.takeInterval i

-- Inspection testing is disabled for takeInterval
-- Enable it when looking at it throughly
#ifdef INSPECTION
-- inspect $ hasNoType 'takeInterval ''SPEC
-- inspect $ hasNoTypeClasses 'takeInterval
-- inspect $ 'takeInterval `hasNoType` ''D.Step
#endif

{-# INLINE dropInterval #-}
dropInterval :: Double -> Stream IO Int -> IO ()
dropInterval i = drain . Stream.dropInterval i

-- Inspection testing is disabled for dropInterval
-- Enable it when looking at it throughly
#ifdef INSPECTION
-- inspect $ hasNoTypeClasses 'dropInterval
-- inspect $ 'dropInterval `hasNoType` ''D.Step
#endif

-- XXX Decide on the time interval
{-# INLINE _intervalsOfSum #-}
_intervalsOfSum :: Stream.MonadAsync m => Double -> Stream m Int -> m ()
_intervalsOfSum i = drain . Stream.intervalsOf i Fold.sum

timeBenchmarks :: BenchEnv -> Int -> [Benchmark]
timeBenchmarks _env size =
    [ benchIOSink size "parTapCount 1 second" pollCounts
    , benchIOSink size "takeInterval-all" (takeInterval 10000)
    , benchIOSink size "dropInterval-all" (dropInterval 10000)
    ]

-------------------------------------------------------------------------------
-- Grouping/Splitting
-------------------------------------------------------------------------------

{-# INLINE classifySessionsOf #-}
classifySessionsOf :: Stream.MonadAsync m => (Int -> Int) -> Stream m Int -> m ()
classifySessionsOf getKey =
      Common.drain
    . Stream.classifySessionsOf
        (const (return False)) 3 (Fold.take 10 Fold.sum)
    . Stream.timestamped
    . fmap (\x -> (getKey x, x))

{-# INLINE classifySessionsOfHash #-}
classifySessionsOfHash :: Stream.MonadAsync m =>
    (Int -> Int) -> Stream m Int -> m ()
classifySessionsOfHash getKey =
      Common.drain
    . Stream.classifySessionsByGeneric
        (Proxy :: Proxy (HashMap k))
        1 False (const (return False)) 3 (Fold.take 10 Fold.sum)
    . Stream.timestamped
    . fmap (\x -> (getKey x, x))

o_1_space_grouping :: BenchEnv -> Int -> [Benchmark]
o_1_space_grouping _env value =
    -- Buffering operations using heap proportional to group/window sizes.
    [ bgroup "grouping"
        [ benchIOSink value "classifySessionsOf (10000 buckets)"
            (classifySessionsOf (getKey 10000))
        , benchIOSink value "classifySessionsOf (64 buckets)"
            (classifySessionsOf (getKey 64))
        , benchIOSink value "classifySessionsOfHash (10000 buckets)"
            (classifySessionsOfHash (getKey 10000))
        , benchIOSink value "classifySessionsOfHash (64 buckets)"
            (classifySessionsOfHash (getKey 64))
        ]
    ]

    where

    getKey :: Int -> Int -> Int
    getKey n = (`mod` n)

moduleName :: String
moduleName = "Data.Stream.Prelude"

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    runWithCLIOpts defaultStreamSize (allBenchmarks env)

    where

    allBenchmarks env size =
        excBenchmarks env size
            ++ timeBenchmarks env size
            ++ o_1_space_grouping env size
