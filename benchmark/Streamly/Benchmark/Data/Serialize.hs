{-# LANGUAGE TemplateHaskell #-}

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

-- |
-- Module      : Streamly.Benchmark.Data.Serialize
-- Copyright   : (c) 2023 Composewell
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.DeepSeq (NFData(..), force)
import Control.Exception (assert)
import Control.Monad (replicateM_)
import GHC.Generics (Generic)
import System.Random (randomRIO)
import Test.QuickCheck

import Streamly.Internal.Data.Unbox (newBytes)
import Streamly.Internal.Data.Serialize
#define USE_TH
#ifdef USE_TH
import Streamly.Internal.Data.Serialize.TH
#endif

import Gauge
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- Simple non-recursive ADT
data CustomDT1
    = CDT1C1
    | CDT1C2 Int
    | CDT1C3 Int Bool
    deriving (Generic, Show, Eq)

#ifndef USE_TH
instance Serialize CustomDT1
#else
$(deriveSerialize ''CustomDT1)
#endif

-- Recursive ADT
data BinTree a
  = Tree (BinTree a) (BinTree a)
  | Leaf a
  deriving (Show, Read, Eq, Generic)

#ifndef USE_TH
instance Serialize (BinTree a)
#else
$(deriveSerialize ''BinTree)
#endif

instance NFData a => NFData (BinTree a) where
  rnf (Leaf a) = rnf a `seq` ()
  rnf (Tree l r) = rnf l `seq` rnf r `seq` ()

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = oneof [Leaf <$> arbitrary, Tree <$> arbitrary <*> arbitrary]

mkBinTree :: (Arbitrary a) => Int -> IO (BinTree a)
mkBinTree = go (generate $ arbitrary)

    where

    go r 0 = Leaf <$> r
    go r n = Tree <$> go r (n - 1) <*> go r (n - 1)

prop :: Property
prop = forAll arbitrary (ioProperty . test)

    where

    test :: BinTree Int -> IO Bool
    test t = do
        let n =
                case size :: Size (BinTree Int) of
                    ConstSize x -> x
                    VarSize f -> f t
        arr <- newBytes n
        _ <- serialize 0 arr t
        (_, t1) <- deserialize 0 arr
        return $ t1 == t

runQC :: IO ()
runQC = quickCheckWith stdArgs {maxSuccess = 100} prop

data Enumeration = Enum1 | Enum2 | Enum3 | Enum4 | Enum5
  deriving (Show, Read, Eq, Generic)

instance NFData Enumeration where
  rnf a = rnf a `seq` ()

instance Arbitrary Enumeration where
    arbitrary = elements [Enum1, Enum2, Enum3, Enum4, Enum5]

#ifndef USE_TH
instance Serialize Enumeration
#else
$(deriveSerialize ''Enumeration)
#endif

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

{-# INLINE pokeTimes #-}
pokeTimes :: forall a. Serialize a => a -> Int -> IO ()
pokeTimes val times = do
    let n =
            case size :: Size a of
                ConstSize x -> x
                VarSize f -> f val
    arr <- newBytes n
    replicateM_ times $ do
        serialize 0 arr val

{-# INLINE peekTimes #-}
peekTimes :: forall a. Serialize a => a -> Int -> IO ()
peekTimes val times = do
    let n =
            case size :: Size a of
                ConstSize x -> x
                VarSize f -> f val
    arr <- newBytes n
    _ <- serialize 0 arr val
    replicateM_ times $ do
        (_, _ :: a) <- deserialize 0 arr
        return ()

{-# INLINE roundtrip #-}
roundtrip :: forall a. (Eq a, Serialize a) => a -> Int -> IO ()
roundtrip val times = do
    let n =
            case size :: Size a of
                ConstSize x -> x
                VarSize f -> f val
    arr <- newBytes n
    replicateM_ times $ do
        _ <- serialize 0 arr val
        (_, val1) <- deserialize 0 arr
        assert (val == val1) (pure ())

benchSink :: NFData b => String -> Int -> (Int -> IO b) -> Benchmark
benchSink name times f = bench name (nfIO (randomRIO (times, times) >>= f))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

allBenchmarks :: BinTree Int -> BinTree Enumeration -> Int -> [Benchmark]
allBenchmarks tInt _tEnum times =
    [ bgroup "poke"
        [ benchSink "C1" times
            (pokeTimes (CDT1C1 :: CustomDT1))
        , benchSink "C2" times
            (pokeTimes ((CDT1C2 (5 :: Int)) :: CustomDT1))
        , benchSink "C3" times
            (pokeTimes ((CDT1C3 (5 :: Int) True) :: CustomDT1))
        ]
    , bgroup "peek"
        [ benchSink "C1" times
            (peekTimes (CDT1C1 :: CustomDT1))
        , benchSink "C2" times
            (peekTimes ((CDT1C2 (5 :: Int)) :: CustomDT1))
        , benchSink "C3" times
            (peekTimes ((CDT1C3 (5 :: Int) True) :: CustomDT1))
        ]
    , bgroup "roundtrip"
        [ benchSink "C1" times
            (roundtrip (CDT1C1 :: CustomDT1))
        , benchSink "C2" times
            (roundtrip ((CDT1C2 (5 :: Int)) :: CustomDT1))
        , benchSink "C3" times
            (roundtrip ((CDT1C3 (5 :: Int) True) :: CustomDT1))
        , benchSink "bintree-int" 1
            (roundtrip tInt)
        -- , benchSink "bintree-enum" 1
        --      (roundtrip tEnum)
        ]
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
#ifndef FUSION_CHECK
    runQC
    !tInt <- force <$> mkBinTree 16
    -- Hangs
    -- !tEnum <- force <$> (mkBinTree 16 :: BinTree Enumeration)
    runWithCLIOpts defaultStreamSize (allBenchmarks tInt undefined)
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    -- peekTimes ((CDT1C2 (5 :: Int)) :: CustomDT1) value
    roundtrip ((CDT1C2 (5 :: Int)) :: CustomDT1) value
    return ()
#endif
