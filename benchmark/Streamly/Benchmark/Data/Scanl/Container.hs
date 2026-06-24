-- |
-- Module      : Scanl.Container
-- Copyright   : (c) 2024 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Benchmarks for operations exported from Streamly.Internal.Data.Scanl.Container.
module Scanl.Container (benchmarks) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Data.Map.Strict (Map)
import Streamly.Internal.Data.Scanl (Scanl(..))
import Streamly.Internal.Data.Stream (Stream)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Streamly.Benchmark.Common
import Test.Tasty.Bench

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Stream (Step(..))
import Test.Inspection
#endif

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE source #-}
source :: (Monad m, Num a, Stream.Enumerable a) =>
    Int -> a -> Stream m a
source len from =
    Stream.enumerateFromThenTo from (from + 1) (from + fromIntegral len)

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream len f = randomRIO (1, 1 :: Int) >>= f . source len

{-# INLINE withPostscanl #-}
withPostscanl :: Int -> Scanl IO Int b -> IO ()
withPostscanl n s = withStream n $ Stream.fold FL.drain . Stream.postscanl s

{-# INLINE benchIO #-}
benchIO :: String -> (Int -> IO ()) -> Int -> Benchmark
benchIO name f value = bench name $ nfIO $ f value

{-# INLINE getKey #-}
getKey :: Int -> Int -> Int
getKey buckets = (`mod` buckets)

{-# INLINE limitedSum #-}
limitedSum :: Int -> Scanl IO Int Int
limitedSum n = Scanl.take n Scanl.sum

{-# INLINE afterDone #-}
afterDone :: IO () -> Scanl IO a b -> Scanl IO a b
afterDone action (Scanl step i e f) = Scanl step1 i e f
    where
    step1 x a = do
        res <- step x a
        case res of
            Scanl.Partial s1 -> pure $ Scanl.Partial s1
            Scanl.Done b -> action >> pure (Scanl.Done b)

{-# NOINLINE ref #-}
ref :: IORef (Set.Set Int)
ref = unsafePerformIO $ newIORef Set.empty

{-# INLINE getScanl #-}
getScanl :: Int -> IO (Maybe (Scanl IO Int Int))
getScanl k = do
    set <- readIORef ref
    if Set.member k set
    then pure Nothing
    else pure
             $ Just
             $ afterDone (modifyIORef ref (Set.insert k)) (limitedSum 100)

-------------------------------------------------------------------------------
-- Set operations
-------------------------------------------------------------------------------

{-# INLINE toSet #-}
toSet :: Int -> IO ()
toSet n = withPostscanl n Scanl.toSet

{-# INLINE toIntSet #-}
toIntSet :: Int -> IO ()
toIntSet n = withPostscanl n Scanl.toIntSet

{-# INLINE countDistinct #-}
countDistinct :: Int -> IO ()
countDistinct n = withPostscanl n Scanl.countDistinct

{-# INLINE countDistinctInt #-}
countDistinctInt :: Int -> IO ()
countDistinctInt n = withPostscanl n Scanl.countDistinctInt

{-# INLINE nub #-}
nub :: Int -> IO ()
nub n = withPostscanl n Scanl.nub

{-# INLINE nubInt #-}
nubInt :: Int -> IO ()
nubInt n = withPostscanl n Scanl.nubInt

-------------------------------------------------------------------------------
-- Demultiplexing
-------------------------------------------------------------------------------

{-# INLINE demuxIOOneShot #-}
demuxIOOneShot :: Int -> IO ()
demuxIOOneShot len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.demuxIO (getKey 64) getScanl)

{-# INLINE demuxIOSum #-}
demuxIOSum :: Int -> IO ()
demuxIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxIO (getKey 64) (const (pure (Just Scanl.sum))))

#ifdef INSPECTION
inspect $ 'demuxIOSum `hasNoType` ''SPEC
#endif

{-# INLINE demuxSum #-}
demuxSum :: Int -> IO ()
demuxSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demux (getKey 64) (const (pure (Just Scanl.sum))))

{-# INLINE demuxGenericSum #-}
demuxGenericSum :: Int -> IO ()
demuxGenericSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxGeneric (getKey 64) (const (pure (Just Scanl.sum)))
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

{-# INLINE demuxGenericIOSum #-}
demuxGenericIOSum :: Int -> IO ()
demuxGenericIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.demuxGenericIO (getKey 64) (const (pure (Just Scanl.sum)))
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

-------------------------------------------------------------------------------
-- Classifying
-------------------------------------------------------------------------------

{-# INLINE classifyLimitedSum #-}
classifyLimitedSum :: Int -> IO ()
classifyLimitedSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classifyIO (getKey 64) (limitedSum 100))

#ifdef INSPECTION
inspect $ 'classifyLimitedSum `hasNoType` ''FL.Step
inspect $ 'classifyLimitedSum `hasNoType` ''SPEC
#endif

{-# INLINE classifyIOSum #-}
classifyIOSum :: Int -> IO ()
classifyIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classifyIO (getKey 64) Scanl.sum)

#ifdef INSPECTION
inspect $ 'classifyIOSum `hasNoType` ''FL.Step
inspect $ 'classifyIOSum `hasNoType` ''SPEC
#endif

{-# INLINE classifySum #-}
classifySum :: Int -> IO ()
classifySum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classify (getKey 64) Scanl.sum)

{-# INLINE classifyGenericSum #-}
classifyGenericSum :: Int -> IO ()
classifyGenericSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.classifyGeneric (getKey 64) Scanl.sum
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

{-# INLINE classifyGenericIOSum #-}
classifyGenericIOSum :: Int -> IO ()
classifyGenericIOSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl
            (Scanl.classifyGenericIO (getKey 64) Scanl.sum
                :: Scanl IO Int (IO (Map Int Int), Maybe (Int, Int)))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks value =
    fmap (SpaceO_1,)
        [ benchIO "demuxIO (1-shot) (64 buckets) [sum 100]" demuxIOOneShot value
        , benchIO "demuxIO (64 buckets) [sum]" demuxIOSum value
        , benchIO "classifyIO (64 buckets) [sum 100]" classifyLimitedSum value
        , benchIO "classifyIO (64 buckets) [sum]" classifyIOSum value
        ]
    ++ fmap (HeapO_n,)
        [ benchIO "toSet" toSet value
        , benchIO "toIntSet" toIntSet value
        , benchIO "countDistinct" countDistinct value
        , benchIO "countDistinctInt" countDistinctInt value
        , benchIO "nub" nub value
        , benchIO "nubInt" nubInt value
        , benchIO "demux (64 buckets) [sum]" demuxSum value
        , benchIO "demuxGeneric (64 buckets) [sum]" demuxGenericSum value
        , benchIO "demuxGenericIO (64 buckets) [sum]" demuxGenericIOSum value
        , benchIO "classify (64 buckets) [sum]" classifySum value
        , benchIO "classifyGeneric (64 buckets) [sum]" classifyGenericSum value
        , benchIO "classifyGenericIO (64 buckets) [sum]" classifyGenericIOSum value
        ]
