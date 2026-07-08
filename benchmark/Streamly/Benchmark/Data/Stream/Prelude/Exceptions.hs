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

import Control.DeepSeq (NFData)
import Control.Exception (Exception, throwIO)
import Data.HashMap.Strict (HashMap)
import Data.Proxy (Proxy(..))
import Stream.Common (drain)
import Streamly.Internal.Data.IsMap.HashMap ()
import Streamly.Internal.Data.Stream (Stream)
import System.IO (Handle, hClose, hPutChar)
import System.Random (randomRIO)

import qualified Data.IORef as Ref
import qualified Data.Map.Strict as Map

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.FileSystem.Handle as IFH
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

{-# NOINLINE retryNoneSimple #-}
retryNoneSimple :: Int -> Int -> IO ()
retryNoneSimple length from =
    drain
        $ Stream.retry
            (Map.singleton BenchException1 length)
            (const Stream.nil)
            source

    where

    source = Stream.enumerateFromTo from (from + length)

{-# NOINLINE retryNone #-}
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

{-# NOINLINE retryAll #-}
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

{-# NOINLINE retryUnknown #-}
retryUnknown :: Int -> Int -> IO ()
retryUnknown length from = do
    drain
        $ Stream.retry (Map.singleton BenchException1 length) (const source)
        $ throwIO BenchException2 `Stream.before` Stream.nil

    where

    source = Stream.enumerateFromTo from (from + length)


o_1_space_serial_exceptions :: Int -> [Benchmark]
o_1_space_serial_exceptions length =
    [ benchIOSrc1 "retryNoneSimple" (retryNoneSimple length)
    , benchIOSrc1 "retryNone" (retryNone length)
    , benchIOSrc1 "retryAll" (retryAll length)
    , benchIOSrc1 "retryUnknown" (retryUnknown length)
    ]

-- XXX Move these to FileSystem.Handle benchmarks

-------------------------------------------------------------------------------
-- copy stream exceptions
-------------------------------------------------------------------------------

{-# NOINLINE readWriteFinallyStream #-}
readWriteFinallyStream :: Handle -> Handle -> IO ()
readWriteFinallyStream inh devNull =
    let readEx = Stream.finally (hClose inh) (Stream.unfold FH.reader inh)
    in Stream.fold (FH.write devNull) readEx

{-# NOINLINE fromToBytesBracketStream #-}
fromToBytesBracketStream :: Handle -> Handle -> IO ()
fromToBytesBracketStream inh devNull =
    let readEx = Stream.bracket (return ()) (\_ -> hClose inh)
                    (\_ -> IFH.read inh)
    in IFH.putBytes devNull readEx

{-# NOINLINE readWriteBeforeAfterStream #-}
readWriteBeforeAfterStream :: Handle -> Handle -> IO ()
readWriteBeforeAfterStream inh devNull =
    let readEx =
            Stream.after (hClose inh)
                $ Stream.before (hPutChar devNull 'A') (Stream.unfold FH.reader inh)
     in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ 'readWriteBeforeAfterStream `hasNoType` ''Stream.Step
#endif

{-# NOINLINE readWriteAfterStream #-}
readWriteAfterStream :: Handle -> Handle -> IO ()
readWriteAfterStream inh devNull =
    let readEx = Stream.after (hClose inh) (Stream.unfold FH.reader inh)
     in Stream.fold (FH.write devNull) readEx

#ifdef INSPECTION
inspect $ 'readWriteAfterStream `hasNoType` ''Stream.Step
#endif

o_1_space_copy_stream_exceptions :: BenchEnv -> [Benchmark]
o_1_space_copy_stream_exceptions env =
    [ mkBenchSmall "Stream.finally" env $ \inh _ ->
        readWriteFinallyStream inh (nullH env)
    , mkBenchSmall "Stream.after . Stream.before" env $ \inh _ ->
        readWriteBeforeAfterStream inh (nullH env)
    , mkBenchSmall "Stream.after" env $ \inh _ ->
        readWriteAfterStream inh (nullH env)
    , mkBenchSmall "Stream.bracket fromToBytes" env $ \inh _ ->
        fromToBytesBracketStream inh (nullH env)
    ]

-------------------------------------------------------------------------------
-- Exceptions toChunks
-------------------------------------------------------------------------------

{-# NOINLINE toChunksBracket #-}
toChunksBracket :: Handle -> Handle -> IO ()
toChunksBracket inh devNull =
    let readEx = Stream.bracket
            (return ())
            (\_ -> hClose inh)
            (\_ -> IFH.readChunks inh)
    in Stream.fold (IFH.writeChunks devNull) readEx

o_1_space_copy_exceptions_toChunks :: BenchEnv -> [Benchmark]
o_1_space_copy_exceptions_toChunks env =
    [ mkBench "Stream.bracket toChunks" env $ \inH _ ->
        toChunksBracket inH (nullH env)
    ]

excBenchmarks :: BenchEnv -> Int -> [Benchmark]
excBenchmarks env size =
    [ bgroup (o_1_space_prefix moduleName) $ concat
        [ o_1_space_serial_exceptions size
        , o_1_space_copy_exceptions_toChunks env
        , o_1_space_copy_stream_exceptions env
        ]
    ]

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream value f = randomRIO (1, 1 :: Int) >>= f . Common.sourceUnfoldrM value

{-# NOINLINE pollCounts #-}
pollCounts :: Int -> IO ()
pollCounts value = withStream value $ drain . Stream.parTapCount (const True) f

    where

    f = Stream.drain . Stream.rollingMap2 (-) . Stream.delayPost 1

{-# NOINLINE takeInterval #-}
takeInterval :: Double -> Int -> IO ()
takeInterval i value = withStream value $ drain . Stream.takeInterval i

-- Inspection testing is disabled for takeInterval
-- Enable it when looking at it throughly
#ifdef INSPECTION
-- inspect $ hasNoType 'takeInterval ''SPEC
-- inspect $ hasNoTypeClasses 'takeInterval
-- inspect $ 'takeInterval `hasNoType` ''D.Step
#endif

{-# NOINLINE dropInterval #-}
dropInterval :: Double -> Int -> IO ()
dropInterval i value = withStream value $ drain . Stream.dropInterval i

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
    [ benchIO "parTapCount 1 second" (pollCounts size)
    , benchIO "takeInterval-all" (takeInterval 10000 size)
    , benchIO "dropInterval-all" (dropInterval 10000 size)
    ]

-------------------------------------------------------------------------------
-- Grouping/Splitting
-------------------------------------------------------------------------------

{-# INLINE getKey #-}
getKey :: Int -> Int -> Int
getKey n = (`mod` n)

{-# INLINE classifySessionsOf #-}
classifySessionsOf :: (Int -> Int) -> Int -> IO ()
classifySessionsOf getKeyF value = withStream value $
      Common.drain
    . Stream.classifySessionsOf
        (const (return False)) 3 (Fold.take 10 Fold.sum)
    . Stream.timestamped
    . fmap (\x -> (getKeyF x, x))

{-# NOINLINE classifySessionsOf10k #-}
classifySessionsOf10k :: Int -> IO ()
classifySessionsOf10k = classifySessionsOf (getKey 10000)

{-# NOINLINE classifySessionsOf64 #-}
classifySessionsOf64 :: Int -> IO ()
classifySessionsOf64 = classifySessionsOf (getKey 64)

{-# INLINE classifySessionsOfHash #-}
classifySessionsOfHash :: (Int -> Int) -> Int -> IO ()
classifySessionsOfHash getKeyF value = withStream value $
      Common.drain
    . Stream.classifySessionsByGeneric
        (Proxy :: Proxy (HashMap k))
        1 False (const (return False)) 3 (Fold.take 10 Fold.sum)
    . Stream.timestamped
    . fmap (\x -> (getKeyF x, x))

{-# NOINLINE classifySessionsOfHash10k #-}
classifySessionsOfHash10k :: Int -> IO ()
classifySessionsOfHash10k = classifySessionsOfHash (getKey 10000)

{-# NOINLINE classifySessionsOfHash64 #-}
classifySessionsOfHash64 :: Int -> IO ()
classifySessionsOfHash64 = classifySessionsOfHash (getKey 64)

o_1_space_grouping :: BenchEnv -> Int -> [Benchmark]
o_1_space_grouping _env value =
    -- Buffering operations using heap proportional to group/window sizes.
    [ benchIO "classifySessionsOf (10000 buckets)"
        (classifySessionsOf10k value)
    , benchIO "classifySessionsOf (64 buckets)"
        (classifySessionsOf64 value)
    , benchIO "classifySessionsOfHash (10000 buckets)"
        (classifySessionsOfHash10k value)
    , benchIO "classifySessionsOfHash (64 buckets)"
        (classifySessionsOfHash64 value)
    ]

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
