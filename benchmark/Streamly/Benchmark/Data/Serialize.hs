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

import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import System.Random (randomRIO)
#ifndef USE_UNBOX
import Test.QuickCheck (Arbitrary, arbitrary)
#endif

#ifdef USE_UNBOX
import Data.Proxy (Proxy(..))
import Streamly.Internal.Data.Unbox
#else
import Control.DeepSeq (force)
import Test.QuickCheck (oneof, generate)
import Streamly.Internal.Data.Unbox (newBytes, MutableByteArray)
import Streamly.Internal.Data.Serialize
#endif

#ifdef USE_TH
#ifdef USE_UNBOX
import Streamly.Internal.Data.Unbox.TH
#else
import Streamly.Internal.Data.Serialize.TH
#endif
#endif

import Gauge
import Streamly.Benchmark.Common

#ifdef USE_UNBOX
#define SERIALIZE_CLASS Unbox
#define DERIVE_CLASS deriveUnbox
#define SERIALIZE_OP pokeByteIndex
#define DESERIALIZE_OP peekByteIndex
#else
#define SERIALIZE_CLASS Serialize
#define DERIVE_CLASS deriveSerialize
#define SERIALIZE_OP serialize
#define DESERIALIZE_OP deserialize
#endif

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Unit = Unit
    deriving (Generic, Show, Eq)

#ifndef USE_TH
instance SERIALIZE_CLASS Unit
#else
$(DERIVE_CLASS ''Unit)
#endif

data Sum2
    = Sum21
    | Sum22
    deriving (Generic, Show, Eq)

#ifndef USE_TH
instance SERIALIZE_CLASS Sum2
#else
$(DERIVE_CLASS ''Sum2)
#endif

data Sum25
    = Sum251
    | Sum252
    | Sum253
    | Sum254
    | Sum255
    | Sum256
    | Sum257
    | Sum258
    | Sum259
    | Sum2510
    | Sum2511
    | Sum2512
    | Sum2513
    | Sum2514
    | Sum2515
    | Sum2516
    | Sum2517
    | Sum2518
    | Sum2519
    | Sum2520
    | Sum2521
    | Sum2522
    | Sum2523
    | Sum2524
    | Sum2525
    deriving (Generic, Show, Eq)

#ifndef USE_TH
instance SERIALIZE_CLASS Sum25
#else
$(DERIVE_CLASS ''Sum25)
#endif

data Product25
    = Product25
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
        Int
    deriving (Generic, Show)

#ifndef USE_TH
instance SERIALIZE_CLASS Product25
#else
$(DERIVE_CLASS ''Product25)
#endif

-- XXX derived Eq instance is not inlined
instance Eq Product25 where
    {-# INLINE (==) #-}
    (==)
        (Product25 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22 a23 a24 a25)
        (Product25 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19 b20 b21 b22 b23 b24 b25)
           =   a1  == b1
            && a2  == b2
            && a3  == b3
            && a4  == b4
            && a5  == b5
            && a6  == b6
            && a7  == b7
            && a8  == b8
            && a9  == b9
            && a10 == b10
            && a11 == b11
            && a12 == b12
            && a13 == b13
            && a14 == b14
            && a15 == b15
            && a16 == b16
            && a17 == b17
            && a18 == b18
            && a19 == b19
            && a20 == b20
            && a21 == b21
            && a22 == b22
            && a23 == b23
            && a24 == b24
            && a25 == b25

-------------------------------------------------------------------------------
-- Simple non-recursive ADT
-------------------------------------------------------------------------------

data CustomDT1
    = CDT1C1
    | CDT1C2 Int
    | CDT1C3 Int Int
    deriving (Generic, Show, Eq)

#ifndef USE_TH
instance SERIALIZE_CLASS CustomDT1
#else
$(DERIVE_CLASS ''CustomDT1)
#endif

-------------------------------------------------------------------------------
-- Recursive ADT
-------------------------------------------------------------------------------

#ifndef USE_UNBOX
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

#endif

-------------------------------------------------------------------------------
-- Size helpers
-------------------------------------------------------------------------------

{-# INLINE getSize #-}
getSize :: forall a. SERIALIZE_CLASS a => a -> Int
#ifdef USE_UNBOX
getSize _ = sizeOf (Proxy :: Proxy a)
#else
getSize val =
    case size :: Size a of
        Size f -> f 0 val
#endif

-------------------------------------------------------------------------------
-- Common helpers
-------------------------------------------------------------------------------

-- Parts of "f" that are dependent on val will not be optimized out.
{-# INLINE loop #-}
loop :: Int -> (a -> IO b) -> a -> IO ()
loop count f val = go count val
    where

    go n x = do
        if n > 0
        then f x >> go (n-1) x
        else return ()

-- The first arg of "f" is the environment which is not threaded around in the
-- loop.
{-# INLINE loopWith #-}
loopWith :: Int -> (env -> a -> IO b) -> env -> a -> IO ()
loopWith count f e val = go count val
    where

    go n x = do
        if n > 0
        then f e x >> go (n-1) x
        else return ()

benchSink :: NFData b => String -> Int -> (Int -> IO b) -> Benchmark
benchSink name times f = bench name (nfIO (randomRIO (times, times) >>= f))

-------------------------------------------------------------------------------
-- Serialization Helpers
-------------------------------------------------------------------------------

{-# INLINE pokeWithSize #-}
pokeWithSize :: SERIALIZE_CLASS a => MutableByteArray -> a -> IO ()
pokeWithSize arr val = do
    let n = getSize val
    n `seq` SERIALIZE_OP 0 arr val >> return ()

{-# INLINE pokeTimesWithSize #-}
pokeTimesWithSize :: SERIALIZE_CLASS a => a -> Int -> IO ()
pokeTimesWithSize val times = do
    let n = getSize val
    arr <- newBytes n
    loopWith times pokeWithSize arr val

{-# INLINE poke #-}
poke :: SERIALIZE_CLASS a => MutableByteArray -> a -> IO ()
poke arr val = SERIALIZE_OP 0 arr val >> return ()

{-# INLINE pokeTimes #-}
pokeTimes :: SERIALIZE_CLASS a => a -> Int -> IO ()
pokeTimes val times = do
    let n = getSize val
    arr <- newBytes n
    loopWith times poke arr val

{-# INLINE peek #-}
peek :: forall a. (Eq a, SERIALIZE_CLASS a) => a -> MutableByteArray -> IO ()
peek val arr = do
#ifdef USE_UNBOX
        (val1 :: a)
#else
        (_, val1 :: a)
#endif
            <- DESERIALIZE_OP 0 arr
        -- Ensure that we are actually constructing the type and using it. This
        -- is important, otherwise the structure is created and discarded, the
        -- cost of creation of the structure is not accounted. Otherwise we may
        -- just read the values and discard them. The comparison adds to the
        -- cost though. We could use deepseq but then we need to write
        -- instances of NFData and ensure that they are correct and perform
        -- well. Equality check also ensures correctness.
        if (val1 /= val)
        then error "peek: no match"
        else return ()

{-# INLINE peekTimes #-}
peekTimes :: (Eq a, SERIALIZE_CLASS a) => Int -> a -> Int -> IO ()
peekTimes n val times = do
    arr <- newBytes n
    _ <- SERIALIZE_OP 0 arr val
    loopWith times peek val arr

{-# INLINE trip #-}
trip :: forall a. (Eq a, SERIALIZE_CLASS a) => a -> IO ()
trip val = do
    let n = getSize val
    arr <- newBytes n
    _ <- SERIALIZE_OP 0 arr val
#ifdef USE_UNBOX
    val1
#else
    (_, val1)
#endif
        <- DESERIALIZE_OP 0 arr
    -- Do not remove this, see the comments in peek.
    if (val1 /= val)
    then error "roundtrip: no match"
    else return ()

{-# INLINE roundtrip #-}
roundtrip :: (Eq a, SERIALIZE_CLASS a) => a -> Int -> IO ()
roundtrip val times = loop times trip val

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

{-# INLINE benchConst #-}
benchConst ::
       String
    -> (forall a. (Eq a, SERIALIZE_CLASS a) => Int -> a -> Int -> IO ())
    -> Int
    -> Benchmark
benchConst gname f times =
    bgroup gname
       [ let !n = getSize Unit
          in benchSink "Unit" times (f n Unit)
       , let !n = getSize CDT1C1
          in benchSink "C1" times (f n CDT1C1)
       , let val = CDT1C2 5
             !n = getSize val
          in benchSink "C2" (times `div` 2) (f n val)
       , let val = CDT1C3 5 2
             !n = getSize val
          in benchSink "C3" (times `div` 3) (f n val)
       , let !n = getSize Sum21
          in benchSink "Sum2" times (f n Sum21)
       , let !n = getSize Sum2525
          in benchSink "Sum25" times (f n Sum2525)
       , let val = Product25 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
             !n = getSize val
          in benchSink "Product25" (times `div` 26) (f n val)
        ]

#ifndef USE_UNBOX
{-# INLINE benchVar #-}
benchVar ::
       String
    -> (forall a. (Eq a, SERIALIZE_CLASS a) => Int -> a -> Int -> IO ())
    -> BinTree Int
    -> [Int]
    -> Int
    -> Benchmark
benchVar gname f tInt lInt times =
    bgroup gname
       [ let !n = getSize tInt
           in benchSink "bintree-int" times (f n tInt)
        , let !n = getSize lInt
           in benchSink "list-int" times (f n lInt)
        ]
#endif

-- Times is scaled by the number of constructors to normalize
#ifdef USE_UNBOX
allBenchmarks :: Int -> [Benchmark]
allBenchmarks times =
#else
allBenchmarks :: BinTree Int -> [Int] -> Int -> [Benchmark]
allBenchmarks tInt lInt times =
#endif
    [ bgroup "sizeOf"
        [
#ifndef USE_UNBOX
          bench "bintree-int" $ nf getSize tInt
        , bench "list-int" $ nf getSize lInt
#endif
        ]
    , benchConst "poke" (const pokeTimes) times
    , benchConst "pokeWithSize" (const pokeTimesWithSize) times
    , benchConst "peek" peekTimes times
    , benchConst "roundtrip" (const roundtrip) times
#ifndef USE_UNBOX
    , benchVar "poke" (const pokeTimes) tInt lInt 1
    , benchVar "pokeWithSize" (const pokeTimesWithSize) tInt lInt 1
    , benchVar "peek" peekTimes tInt lInt 1
    , benchVar "roundtrip" (const roundtrip) tInt lInt 1
#endif
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
#ifndef USE_UNBOX
    -- Approximately 100000 constructors
    -- Assuming Leaf nodes constitute two constructors (Leaf, Int) and internal
    -- nodes 1 level = log_2 (100001/3) + 1 = 16
    !(tInt :: BinTree Int) <- force <$> mkBinTree 16

    -- Approximately 100000 constructors, assuming two constructors (Cons, Int)
    -- per element.
    let lInt = [1..50000 :: Int]
    let !len = length lInt -- evaluate the list
#endif
#ifndef FUSION_CHECK
    -- This can take too much memory/CPU, need to restrict the test
    -- runQC
#ifdef USE_UNBOX
    runWithCLIOpts defaultStreamSize allBenchmarks
#else
    len `seq` runWithCLIOpts defaultStreamSize (allBenchmarks tInt lInt)
#endif
#else
    -- Enable FUSION_CHECK macro at the beginning of the file
    -- Enable one benchmark below, and run the benchmark
    -- Check the .dump-simpl output
    let value = 100000
    -- print $ getSize (CDT1C3 4 2)
    -- print $ getSize tInt
    -- print $ getSize lInt

    -- pokeTimes tInt 1

    -- peekTimes ((CDT1C2 (5 :: Int)) :: CustomDT1) value
    -- peekTimes (Sum2525) value
    -- peekTimes (Product25 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25) value
    -- peekTimes tInt 1
    let !n = getSize lInt
    peekTimes n lInt 1

    -- roundtrip ((CDT1C2 (5 :: Int)) :: CustomDT1) value
    return ()
#endif
