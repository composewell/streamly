-- |
-- Module      : Streamly.Benchmark.Data.Scanl
-- Copyright   : (c) 2018 Composewell
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Control.DeepSeq (NFData(..))
import Data.Functor.Identity (Identity(..))
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Scanl (Scanl(..))
import Streamly.Internal.Data.MutArray (MutArray)

import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench hiding (env)
import Streamly.Benchmark.Common
import Prelude hiding (last, length, all, any, take, unzip, sequence_)

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

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withStream #-}
withStream :: Int -> (Stream IO Int -> IO b) -> IO b
withStream len f = randomRIO (1, 1 :: Int) >>= f . source len

{-# INLINE limitedSum #-}
limitedSum :: Int -> Scanl IO Int Int
limitedSum n = Scanl.take n Scanl.sum

{-# INLINE getKey #-}
getKey :: Int -> Int -> Int
getKey buckets = (`mod` buckets)

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
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Scanl"

instance NFData (MutArray a) where
    {-# INLINE rnf #-}
    rnf _ = ()

instance NFData a => NFData (Stream Identity a) where
    {-# INLINE rnf #-}
    rnf xs = runIdentity $ Stream.fold (FL.foldl' (\_ x -> rnf x) ()) xs

{-# INLINE demuxIOOneShot #-}
demuxIOOneShot :: Int -> IO ()
demuxIOOneShot len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.demuxIO (getKey 64) getScanl)

{-# INLINE demuxSum #-}
demuxSum :: Int -> IO ()
demuxSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.demuxIO (getKey 64) (const (pure (Just Scanl.sum))))

#ifdef INSPECTION
-- inspect $ 'demuxSum `hasNoType` ''Step
-- inspect $ 'demuxSum `hasNoType` ''FL.Step
inspect $ 'demuxSum `hasNoType` ''SPEC
#endif

{-# INLINE classifyLimitedSum #-}
classifyLimitedSum :: Int -> IO ()
classifyLimitedSum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classifyIO (getKey 64) (limitedSum 100))

#ifdef INSPECTION
-- inspect $ 'classifyLimitedSum `hasNoType` ''Step
inspect $ 'classifyLimitedSum `hasNoType` ''FL.Step
inspect $ 'classifyLimitedSum `hasNoType` ''SPEC
#endif

{-# INLINE classifySum #-}
classifySum :: Int -> IO ()
classifySum len =
    withStream len $
        Stream.fold FL.drain
        . Stream.postscanl (Scanl.classifyIO (getKey 64) Scanl.sum)

#ifdef INSPECTION
-- inspect $ 'classifySum `hasNoType` ''Step
inspect $ 'classifySum `hasNoType` ''FL.Step
inspect $ 'classifySum `hasNoType` ''SPEC
#endif

o_1_space_serial :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_serial value =
    [ (SpaceO_1, benchIO "demuxIO (1-shot) (64 buckets) [sum 100]" $ demuxIOOneShot value)
    , (SpaceO_1, benchIO "demuxIO (64 buckets) [sum]" $ demuxSum value)
    , (SpaceO_1, benchIO "classifyIO (64 buckets) [sum 100]" $ classifyLimitedSum value)
    , (SpaceO_1, benchIO "classifyIO (64 buckets) [sum]" $ classifySum value)
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        let allBenches = o_1_space_serial value
            get x = map snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        ]
