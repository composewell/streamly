{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Streamly.Benchmark.Data.Array.Unboxed.Mut
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

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

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import System.Random (randomRIO)
import Prelude
    ( IO
    , Int
    , Integral(..)
    , Maybe(..)
    , Monad(..)
    , Num(..)
    , Ord(..)
    , String
    , ($)
    , (.)
    , (||)
    , concat
    , const
    , fmap
    , id
    , undefined
    )
import Streamly.Internal.Data.MutArray (MutArray)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common hiding (benchPureSrc)

type Stream = MutArray

instance NFData (MutArray a) where
    {-# INLINE rnf #-}
    rnf _ = ()

-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO a) -> (a -> b) -> Benchmark
benchIO name src f = bench name $ nfIO $
    (randomRIO (1,1) >>= src) <&> f

{-# INLINE benchPureSink #-}
benchPureSink :: NFData b => Int -> String -> (Stream Int -> b) -> Benchmark
benchPureSink value name = benchIO name (sourceIntFromTo value)

{-# INLINE benchIO' #-}
benchIO' :: NFData b => String -> (Int -> IO a) -> (a -> IO b) -> Benchmark
benchIO' name src f = bench name $ nfIO $
    randomRIO (1,1) >>= src >>= f

{-# INLINE benchIOSink #-}
benchIOSink :: NFData b => Int -> String -> (Stream Int -> IO b) -> Benchmark
benchIOSink value name = benchIO' name (sourceIntFromTo value)

-- Drain a source that generates an array in the IO monad
{-# INLINE benchIOSrc #-}
benchIOSrc :: String -> (Int -> IO (Stream a)) -> Benchmark
benchIOSrc name src = benchIO name src id

drain :: Monad m => Stream.Stream m a -> m ()
drain = Stream.fold Fold.drain

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: MonadIO m => Int -> Int -> m (Stream Int)
sourceUnfoldr value n =
    Stream.fold (MArray.writeN value) $ Stream.unfoldr step n

    where

    step cnt =
        if cnt > n + value
        then Nothing
        else Just (cnt, cnt + 1)

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromTo value n =
    Stream.fold (MArray.writeN value) $ Stream.enumerateFromTo n (n + value)

{-# INLINE sourceFromList #-}
sourceFromList :: MonadIO m => Int -> Int -> m (Stream Int)
sourceFromList value n =
    Stream.fold (MArray.writeN value) $ Stream.fromList [n .. n + value]

{-# INLINE sourceIntFromToFromList #-}
sourceIntFromToFromList :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromToFromList value n = MArray.fromListN value [n..n + value]

{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromToFromStream value n =
    Stream.fold MArray.write $ Stream.enumerateFromTo n (n + value)

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream.Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

{-# INLINE composeN #-}
composeN :: Monad m
    => Int -> (Stream Int -> m (Stream Int)) -> Stream Int -> m (Stream Int)
composeN n f x =
    case n of
        1 -> f x
        2 -> f x >>= f
        3 -> f x >>= f >>= f
        4 -> f x >>= f >>= f >>= f
        _ -> undefined

{-# INLINE scanl' #-}
scanl' :: MonadIO m => Int -> Int -> Stream Int -> m (Stream Int)
scanl' value n = composeN n $ onArray value $ Stream.scan (Fold.foldl' (+) 0)

{-# INLINE scanl1' #-}
scanl1' :: MonadIO m => Int -> Int -> Stream Int -> m (Stream Int)
scanl1' value n = composeN n $ onArray value $ Stream.scanl1' (+)

{-# INLINE map #-}
map :: MonadIO m => Int -> Int -> Stream Int -> m (Stream Int)
map value n = composeN n $ onArray value $ fmap (+ 1)

{-# INLINE onArray #-}
onArray
    :: MonadIO m => Int -> (Stream.Stream m Int -> Stream.Stream m Int)
    -> Stream Int
    -> m (Stream Int)
onArray value f arr =
    Stream.fold (MArray.writeN value) $ f $ Stream.unfold MArray.reader arr

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE unfoldReadDrain #-}
unfoldReadDrain :: MonadIO m => Stream Int -> m ()
unfoldReadDrain = drain . Stream.unfold MArray.reader

{-# INLINE unfoldReadRevDrain #-}
unfoldReadRevDrain :: MonadIO m => Stream Int -> m ()
unfoldReadRevDrain = drain . Stream.unfold MArray.readerRev

{-# INLINE toStreamDRevDrain #-}
toStreamDRevDrain :: MonadIO m => Stream Int -> m ()
toStreamDRevDrain = drain . MArray.readRev

{-# INLINE toStreamDDrain #-}
toStreamDDrain :: MonadIO m => Stream Int -> m ()
toStreamDDrain = drain . MArray.read

{-# INLINE unfoldFold #-}
unfoldFold :: MonadIO m => Stream Int -> m Int
unfoldFold = Stream.fold (Fold.foldl' (+) 0) . Stream.unfold MArray.reader

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ bgroup
        "generation"
        [ benchIOSrc "writeN . intFromTo" (sourceIntFromTo value)
        , benchIOSrc
              "fromList . intFromTo"
              (sourceIntFromToFromList value)
        , benchIOSrc "writeN . unfoldr" (sourceUnfoldr value)
        , benchIOSrc "writeN . fromList" (sourceFromList value)
        , benchIOSrc "write . intFromTo" (sourceIntFromToFromStream value)
        ]
    ]

o_1_space_elimination :: Int -> [Benchmark]
o_1_space_elimination value =
    [ bgroup "elimination"
        [ benchPureSink value "id" id
        , benchIOSink value "foldl'" unfoldFold
        , benchIOSink value "read" unfoldReadDrain
        , benchIOSink value "readRev" unfoldReadRevDrain
        , benchIOSink value "toStream" toStreamDDrain
        , benchIOSink value "toStreamRev" toStreamDRevDrain
        ]
      ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ bgroup "elimination"
        [
        -- Converting the stream to an array
            benchFold "writeN" (Stream.fold (MArray.writeN value))
                (sourceUnfoldrM value)
         ]
    ]

o_1_space_transformation :: Int -> [Benchmark]
o_1_space_transformation value =
   [ bgroup "transformation"
        [ benchIOSink value "scanl'" (scanl' value 1)
        , benchIOSink value "scanl1'" (scanl1' value 1)
        , benchIOSink value "map" (map value 1)
        ]
   ]

o_1_space_transformationX4 :: Int -> [Benchmark]
o_1_space_transformationX4 value =
    [ bgroup "transformationX4"
        [ benchIOSink value "scanl'" (scanl' value 4)
        , benchIOSink value "scanl1'" (scanl1' value 4)
        , benchIOSink value "map" (map value 4)
        ]
      ]

o_1_space_serial_marray ::
    Int -> (MutArray Int, Array.Array Int) -> [Benchmark]
o_1_space_serial_marray value ~(array, indices) =
    [ benchIO' "partitionBy (< 0)" (const (return array))
        $ MArray.partitionBy (< 0)
    , benchIO' "partitionBy (> 0)" (const (return array))
        $ MArray.partitionBy (> 0)
    , benchIO' "partitionBy (< value/2)" (const (return array))
        $ MArray.partitionBy (< (value `div` 2))
    , benchIO' "partitionBy (> value/2)" (const (return array))
        $ MArray.partitionBy (> (value `div` 2))
    , benchIO' "strip (< value/2 || > value/2)" (const (return array))
        $ MArray.strip (\x -> x < value `div` 2 || x > value `div` 2)
    , benchIO' "strip (> 0)" (const (return array))
        $ MArray.strip (> 0)
    , benchIO' "modifyIndices (+ 1)" (const (return indices))
        $ Stream.fold (MArray.modifyIndices array (\_idx val -> val + 1))
        . Stream.unfold Array.reader
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.MutArray"

main :: IO ()
main = do
    runWithCLIOptsEnv defaultStreamSize alloc allBenchmarks

    where

    alloc value = do
        marr <- MArray.fromStream (sourceUnfoldrM value 0 :: Stream.Stream IO Int)
        indices <- Array.fromStream (sourceUnfoldrM value 0 :: Stream.Stream IO Int)
        return (marr, indices)

    allBenchmarks array value =
        [ bgroup (o_1_space_prefix moduleName)
              $ concat
                    [ o_1_space_serial_marray value array
                    , o_1_space_generation value
                    , o_1_space_elimination value
                    , o_1_space_transformation value
                    , o_1_space_transformationX4 value
                    ]
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
