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
#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif
import System.Random (randomRIO)
import Prelude
    ( IO
    , Int
    , Integral(..)
    , Eq(..)
    , Maybe(..)
    , Monad(..)
    , Num(..)
    , Ord(..)
    , String
    , ($)
    , (.)
    , (||)
    , (++)
    , concat
    , filter
    , fmap
    , fst
    , snd
    , undefined
    )
import Streamly.Internal.Data.MutArray (MutArray)

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.MutArray as MArray
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common hiding (benchPureSrc)

#if __GLASGOW_HASKELL__ >= 810
type Stream :: Type -> Type
#endif
type Stream = MutArray

instance NFData (MutArray a) where
    {-# INLINE rnf #-}
    rnf _ = ()

-------------------------------------------------------------------------------
-- Benchmark helpers
-------------------------------------------------------------------------------

{-# INLINE withRandomIntIO #-}
withRandomIntIO :: (Int -> IO b) -> IO b
withRandomIntIO f = randomRIO (1, 1 :: Int) >>= f

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withArray #-}
withArray :: Int -> (Stream Int -> IO b) -> IO b
withArray value f = sourceIntFromTo value >>= f

{-# INLINE withStream #-}
withStream :: Int -> (Stream.Stream IO Int -> IO b) -> IO b
withStream value f = withRandomIntIO $ \n -> f $ sourceUnfoldrM value n

drain :: Monad m => Stream.Stream m a -> m ()
drain = Stream.fold Fold.drain

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: Int -> IO (Stream Int)
sourceUnfoldr value = withRandomIntIO $ \n ->
    let step cnt =
            if cnt > n + value
            then Nothing
            else Just (cnt, cnt + 1)
    in Stream.fold (MArray.createOf value) $ Stream.unfoldr step n

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: Int -> IO (Stream Int)
sourceIntFromTo value = withRandomIntIO $ \n ->
    Stream.fold (MArray.createOf value) $ Stream.enumerateFromTo n (n + value)

{-# INLINE sourceFromList #-}
sourceFromList :: Int -> IO (Stream Int)
sourceFromList value = withRandomIntIO $ \n ->
    Stream.fold (MArray.createOf value) $ Stream.fromList [n .. n + value]

{-# INLINE sourceIntFromToFromList #-}
sourceIntFromToFromList :: Int -> IO (Stream Int)
sourceIntFromToFromList value = withRandomIntIO $ \n ->
    MArray.fromListN value [n..n + value]

{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: Int -> IO (Stream Int)
sourceIntFromToFromStream value = withRandomIntIO $ \n ->
    Stream.fold MArray.create $ Stream.enumerateFromTo n (n + value)

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

{-# INLINE onArray #-}
onArray
    :: MonadIO m => Int -> (Stream.Stream m Int -> Stream.Stream m Int)
    -> Stream Int
    -> m (Stream Int)
onArray value f arr =
    Stream.fold (MArray.createOf value) $ f $ Stream.unfold MArray.reader arr

{-# INLINE scanl' #-}
scanl' :: Int -> IO (Stream Int)
scanl' value = withArray value $ composeN 1 $ onArray value $ Stream.scanl (Scanl.scanl' (+) 0)

{-# INLINE scanl'X4 #-}
scanl'X4 :: Int -> IO (Stream Int)
scanl'X4 value = withArray value $ composeN 4 $ onArray value $ Stream.scanl (Scanl.scanl' (+) 0)

{-# INLINE scanl1' #-}
scanl1' :: Int -> IO (Stream Int)
scanl1' value = withArray value $ composeN 1 $ onArray value $ Stream.scanl1' (+)

{-# INLINE scanl1'X4 #-}
scanl1'X4 :: Int -> IO (Stream Int)
scanl1'X4 value = withArray value $ composeN 4 $ onArray value $ Stream.scanl1' (+)

{-# INLINE map #-}
map :: Int -> IO (Stream Int)
map value = withArray value $ composeN 1 $ onArray value $ fmap (+ 1)

{-# INLINE mapX4 #-}
mapX4 :: Int -> IO (Stream Int)
mapX4 value = withArray value $ composeN 4 $ onArray value $ fmap (+ 1)

{-# INLINE idArr #-}
idArr :: Int -> IO (Stream Int)
idArr value = withArray value return

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

{-# INLINE unfoldReadDrain #-}
unfoldReadDrain :: Int -> IO ()
unfoldReadDrain value = withArray value $ drain . Stream.unfold MArray.reader

{-# INLINE unfoldReadRevDrain #-}
unfoldReadRevDrain :: Int -> IO ()
unfoldReadRevDrain value = withArray value $ drain . Stream.unfold MArray.readerRev

{-# INLINE toStreamDRevDrain #-}
toStreamDRevDrain :: Int -> IO ()
toStreamDRevDrain value = withArray value $ drain . MArray.readRev

{-# INLINE toStreamDDrain #-}
toStreamDDrain :: Int -> IO ()
toStreamDDrain value = withArray value $ drain . MArray.read

{-# INLINE unfoldFold #-}
unfoldFold :: Int -> IO Int
unfoldFold value = withArray value $ Stream.fold (Fold.foldl' (+) 0) . Stream.unfold MArray.reader

{-# INLINE writeN #-}
writeN :: Int -> IO (Stream Int)
writeN value = withStream value (Stream.fold (MArray.createOf value))

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

o_1_space_generation :: Int -> [Benchmark]
o_1_space_generation value =
    [ benchIO "createOf . intFromTo" $ sourceIntFromTo value
    , benchIO "fromList . intFromTo" $ sourceIntFromToFromList value
    , benchIO "createOf . unfoldr" $ sourceUnfoldr value
    , benchIO "createOf . fromList" $ sourceFromList value
    , benchIO "write . intFromTo" $ sourceIntFromToFromStream value
    ]

o_1_space_elimination :: Int -> [Benchmark]
o_1_space_elimination value =
    [ benchIO "id" $ idArr value
    , benchIO "foldl'" $ unfoldFold value
    , benchIO "read" $ unfoldReadDrain value
    , benchIO "readRev" $ unfoldReadRevDrain value
    , benchIO "toStream" $ toStreamDDrain value
    , benchIO "toStreamRev" $ toStreamDRevDrain value
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ benchIO "createOf" $ writeN value
    ]

o_1_space_transformation :: Int -> [Benchmark]
o_1_space_transformation value =
    [ benchIO "scanl'" $ scanl' value
    , benchIO "scanl1'" $ scanl1' value
    , benchIO "map" $ map value
    ]

o_1_space_transformationX4 :: Int -> [Benchmark]
o_1_space_transformationX4 value =
    [ benchIO "scanl'X4" $ scanl'X4 value
    , benchIO "scanl1'X4" $ scanl1'X4 value
    , benchIO "mapX4" $ mapX4 value
    ]

o_1_space_serial_marray ::
    Int -> (MutArray Int, Array.Array Int) -> [Benchmark]
o_1_space_serial_marray value ~(array, indices) =
    [ benchIO "partitionBy (< 0)" $ MArray.partitionBy (< 0) array
    , benchIO "partitionBy (> 0)" $ MArray.partitionBy (> 0) array
    , benchIO "partitionBy (< value/2)" $
        MArray.partitionBy (< (value `div` 2)) array
    , benchIO "partitionBy (> value/2)" $
        MArray.partitionBy (> (value `div` 2)) array
    , benchIO "strip (< value/2 || > value/2)" $
        MArray.dropAround (\x -> x < value `div` 2 || x > value `div` 2) array
    , benchIO "strip (> 0)" $ MArray.dropAround (> 0) array
    , benchIO "modifyIndices (+ 1)" $
        Stream.fold (MArray.modifyIndices array (\_idx val -> val + 1))
        $ Stream.unfold Array.reader indices
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.MutArray"

benchmarks ::
    (MutArray Int, Array.Array Int) -> Int -> [(SpaceComplexity, Benchmark)]
benchmarks array value =
       fmap (SpaceO_1,)
            (concat
                [ o_1_space_serial_marray value array
                , o_1_space_generation value
                , o_1_space_elimination value
                , o_1_space_transformation value
                , o_1_space_transformationX4 value
                ])
    ++ fmap (HeapO_n,) (o_n_heap_serial value)

main :: IO ()
main = do
    runWithCLIOptsEnv defaultStreamSize alloc allBenchmarks

    where

    alloc value = do
        marr <- MArray.fromStream (sourceUnfoldrM value 0 :: Stream.Stream IO Int)
        indices <- Array.fromStream (sourceUnfoldrM value 0 :: Stream.Stream IO Int)
        return (marr, indices)

    allBenchmarks array value =
        let allBenches = benchmarks array value
            get x = fmap snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_heap = get HeapO_n
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
