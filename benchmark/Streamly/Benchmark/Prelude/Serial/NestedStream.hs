-- |
-- Module      : Serial.NestedStream
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Serial.NestedStream (benchmarks) where

import Control.Monad.Trans.Class (lift)

import qualified Control.Applicative as AP

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream.StreamD as D
#endif

import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Stream.IsStream as Internal
import qualified Streamly.Internal.Data.Unfold as UF

import Gauge
import Streamly.Prelude (SerialT, fromSerial, serial)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Prelude
import Prelude hiding (concatMap)

-------------------------------------------------------------------------------
-- Iteration/looping utilities
-------------------------------------------------------------------------------

{-# INLINE iterateN #-}
iterateN :: (Int -> a -> a) -> a -> Int -> a
iterateN g initial count = f count initial

    where

    f (0 :: Int) x = x
    f i x = f (i - 1) (g i x)

-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton :: S.MonadAsync m
    => (Int -> SerialT m Int -> SerialT m Int)
    -> Int
    -> Int
    -> SerialT m Int
iterateSingleton g count n = iterateN g (return n) count

-- XXX need to check why this is slower than the explicit recursion above, even
-- if the above code is written in a foldr like head recursive way. We also
-- need to try this with foldlM' once #150 is fixed.
-- However, it is perhaps best to keep the iteration benchmarks independent of
-- foldrM and any related fusion issues.
{-# INLINE _iterateSingleton #-}
_iterateSingleton ::
       S.MonadAsync m
    => (Int -> SerialT m Int -> SerialT m Int)
    -> Int
    -> Int
    -> SerialT m Int
_iterateSingleton g value n = S.foldrM g (return n) $ sourceIntFromTo value n

-------------------------------------------------------------------------------
-- Multi-Stream
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

{-# INLINE serial2 #-}
serial2 :: Int -> Int -> IO ()
serial2 count n =
    S.drain $
        S.serial (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1))

{-# INLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    S.drain $
    S.serial
        (S.serial (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1)))
        (S.serial
              (sourceUnfoldrM count (n + 2))
              (sourceUnfoldrM count (n + 3)))

{-# INLINE append2 #-}
append2 :: Int -> Int -> IO ()
append2 count n =
    S.drain $
    Internal.append (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1))

{-# INLINE append4 #-}
append4 :: Int -> Int -> IO ()
append4 count n =
    S.drain $
    Internal.append
        (Internal.append
              (sourceUnfoldrM count n)
              (sourceUnfoldrM count (n + 1)))
        (Internal.append
              (sourceUnfoldrM count (n + 2))
              (sourceUnfoldrM count (n + 3)))

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'append2
inspect $ 'append2 `hasNoType` ''SPEC
inspect $ 'append2 `hasNoType` ''D.AppendState
#endif

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc1 "serial (2,x/2)" (serial2 (value `div` 2))
        , benchIOSrc1 "append (2,x/2)" (append2 (value `div` 2))
        , benchIOSrc1 "serial (2,2,x/4)" (serial4 (value `div` 4))
        , benchIOSrc1 "append (2,2,x/4)" (append4 (value `div` 4))
        ]
    ]

-------------------------------------------------------------------------------
-- Concat Foldable containers
-------------------------------------------------------------------------------

o_1_space_concatFoldable :: Int -> [Benchmark]
o_1_space_concatFoldable value =
    [ bgroup "concat-foldable"
        [ benchIOSrc fromSerial "foldMapWith (<>) (List)"
            (sourceFoldMapWith value)
        , benchIOSrc fromSerial "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream value)
        , benchIOSrc fromSerial "foldMapWithM (<>) (List)"
            (sourceFoldMapWithM value)
        , benchIOSrc fromSerial "S.concatFoldableWith (<>) (List)"
            (concatFoldableWith value)
        , benchIOSrc fromSerial "S.concatForFoldableWith (<>) (List)"
            (concatForFoldableWith value)
        , benchIOSrc fromSerial "foldMapM (List)" (sourceFoldMapM value)
        ]
    ]

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

-- concatMap unfoldrM/unfoldrM

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    S.drain $ S.concatMap
        (\_ -> sourceUnfoldrM inner n)
        (sourceUnfoldrM outer n)

{-# INLINE concatMapM #-}
concatMapM :: Int -> Int -> Int -> IO ()
concatMapM outer inner n =
    S.drain $ S.concatMapM
        (\_ -> return $ sourceUnfoldrM inner n)
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
inspect $ 'concatMap `hasNoType` ''SPEC
#endif

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    S.drain $ S.concatMap
        (\_ -> sourceUnfoldr inner n)
        (sourceUnfoldr outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapPure
inspect $ 'concatMapPure `hasNoType` ''SPEC
#endif

-- concatMap replicate/unfoldrM

{-# INLINE concatMapRepl #-}
concatMapRepl :: Int -> Int -> Int -> IO ()
concatMapRepl outer inner n =
    S.drain $ S.concatMap (S.replicate inner) (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapRepl
inspect $ 'concatMapRepl `hasNoType` ''SPEC
#endif

-- concatMapWith

{-# INLINE concatMapWithSerial #-}
concatMapWithSerial :: Int -> Int -> Int -> IO ()
concatMapWithSerial = concatStreamsWith S.serial

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithSerial
inspect $ 'concatMapWithSerial `hasNoType` ''SPEC
#endif

{-# INLINE concatMapWithAppend #-}
concatMapWithAppend :: Int -> Int -> Int -> IO ()
concatMapWithAppend = concatStreamsWith Internal.append

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithAppend
inspect $ 'concatMapWithAppend `hasNoType` ''SPEC
#endif

-- concatPairWith

{-# INLINE concatPairWithSerial #-}
concatPairWithSerial :: Int -> Int -> Int -> IO ()
concatPairWithSerial = concatPairsWith Internal.serial

{-# INLINE concatPairWithAppend #-}
concatPairWithAppend :: Int -> Int -> Int -> IO ()
concatPairWithAppend = concatPairsWith Internal.append

-- unfoldMany

-- unfoldMany replicate/unfoldrM

{-# INLINE unfoldManyRepl #-}
unfoldManyRepl :: Int -> Int -> Int -> IO ()
unfoldManyRepl outer inner n =
    S.drain
         $ S.unfoldMany
               (UF.lmap return (UF.replicateM inner))
               (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldManyRepl
inspect $ 'unfoldManyRepl `hasNoType` ''D.ConcatMapUState
inspect $ 'unfoldManyRepl `hasNoType` ''SPEC
#endif

o_1_space_concat :: Int -> [Benchmark]
o_1_space_concat value = sqrtVal `seq`
    [ bgroup "concat"
        [ benchIOSrc1 "concatMapPure (n of 1)"
            (concatMapPure value 1)
        , benchIOSrc1 "concatMapPure (sqrt n of sqrt n)"
            (concatMapPure sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapPure (1 of n)"
            (concatMapPure 1 value)

        -- This is for comparison with foldMapWith
        , benchIOSrc fromSerial "concatMapId (n of 1) (fromFoldable)"
            (S.concatMap id . sourceConcatMapId value)

        , benchIOSrc1 "concatMap (n of 1)"
            (concatMap value 1)
        , benchIOSrc1 "concatMap (sqrt n of sqrt n)"
            (concatMap sqrtVal sqrtVal)
        , benchIOSrc1 "concatMap (1 of n)"
            (concatMap 1 value)

        , benchIOSrc1 "concatMapM (n of 1)"
            (concatMapM value 1)
        , benchIOSrc1 "concatMapM (sqrt n of sqrt n)"
            (concatMapM sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapM (1 of n)"
            (concatMapM 1 value)

        -- This is for comparison with foldMapWith
        , benchIOSrc fromSerial "concatMapWithId (n of 1) (fromFoldable)"
            (S.concatMapWith serial id . sourceConcatMapId value)

        , benchIOSrc1 "concatMapWith (n of 1)"
            (concatMapWithSerial value 1)
        , benchIOSrc1 "concatMapWith (sqrt n of sqrt n)"
            (concatMapWithSerial sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapWith (1 of n)"
            (concatMapWithSerial 1 value)

        -- quadratic with number of outer streams
        , benchIOSrc1 "concatMapWithAppend (2 of n/2)"
            (concatMapWithAppend 2 (value `div` 2))

        -- concatMap vs unfoldMany
        , benchIOSrc1 "concatMapRepl (sqrt n of sqrt n)"
            (concatMapRepl sqrtVal sqrtVal)
        , benchIOSrc1 "unfoldManyRepl (sqrt n of sqrt n)"
            (unfoldManyRepl sqrtVal sqrtVal)
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

o_n_space_concat :: Int -> [Benchmark]
o_n_space_concat value = sqrtVal `seq`
    [ bgroup "concat"
        [
        -------------------concatPairsWith-----------------

        -- Use large number of streams to check scalability

          benchIOSrc1 "concatPairWithSerial (n of 1)"
            (concatPairWithSerial value 1)
        , benchIOSrc1 "concatPairWithSerial (sqrtVal of sqrtVal)"
            (concatPairWithSerial sqrtVal sqrtVal)
        , benchIOSrc1 "concatPairWithSerial (2 of n/2)"
            (concatPairWithSerial 2 (value `div` 2))

        , benchIOSrc1 "concatPairWithAppend (n of 1)"
            (concatPairWithAppend value 1)
        , benchIOSrc1 "concatPairWithAppend (sqrtVal of sqrtVal)"
            (concatPairWithAppend sqrtVal sqrtVal)
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

o_1_space_applicative :: Int -> [Benchmark]
o_1_space_applicative value =
    [ bgroup "Applicative"
        [ benchIO "(*>) (sqrt n x sqrt n)" $ apDiscardFst value fromSerial
        , benchIO "(<*) (sqrt n x sqrt n)" $ apDiscardSnd value fromSerial
        , benchIO "(<*>) (sqrt n x sqrt n)" $ toNullAp value fromSerial
        , benchIO "liftA2 (sqrt n x sqrt n)" $ apLiftA2 value fromSerial
        ]
    ]

o_n_space_applicative :: Int -> [Benchmark]
o_n_space_applicative value =
    [ bgroup "Applicative"
        [ benchIOSrc fromSerial "(*>) (n times)" $
            iterateSingleton ((*>) . pure) value
        , benchIOSrc fromSerial "(<*) (n times)" $
            iterateSingleton (\x xs -> xs <* pure x) value
        , benchIOSrc fromSerial "(<*>) (n times)" $
            iterateSingleton (\x xs -> pure (+ x) <*> xs) value
        , benchIOSrc fromSerial "liftA2 (n times)" $
            iterateSingleton (AP.liftA2 (+) . pure) value
        ]
    ]

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

o_1_space_monad :: Int -> [Benchmark]
o_1_space_monad value =
    [ bgroup "Monad"
        [ benchIO "(>>) (sqrt n x sqrt n)" $ monadThen value fromSerial
        , benchIO "(>>=) (sqrt n x sqrt n)" $ toNullM value fromSerial
        , benchIO "(>>=) (sqrt n x sqrt n) (filterAllOut)" $
            filterAllOutM value fromSerial
        , benchIO "(>>=) (sqrt n x sqrt n) (filterAllIn)" $
            filterAllInM value fromSerial
        , benchIO "(>>=) (sqrt n x sqrt n) (filterSome)" $
            filterSome value fromSerial
        , benchIO "(>>=) (sqrt n x sqrt n) (breakAfterSome)" $
            breakAfterSome value fromSerial
        , benchIO "(>>=) (cubert n x cubert n x cubert n)" $
            toNullM3 value fromSerial
        ]
    ]

-- This is a good benchmark but inefficient way to compute primes. As we see a
-- new prime we keep appending a division filter for all the future numbers.
{-# INLINE sieve #-}
sieve :: Monad m => SerialT m Int -> SerialT m Int
sieve s = do
    r <- lift $ S.uncons s
    case r of
        Just (prime, rest) ->
            prime `S.cons` sieve (S.filter (\n -> n `mod` prime /= 0) rest)
        Nothing -> S.nil

o_n_space_monad :: Int -> [Benchmark]
o_n_space_monad value =
    [ bgroup "Monad"
        [ benchIOSrc fromSerial "(>>) (n times)" $
            iterateSingleton ((>>) . pure) value
        , benchIOSrc fromSerial "(>>=) (n times)" $
            iterateSingleton (\x xs -> xs >>= \y -> return (x + y)) value
        , benchIO "(>>=) (sqrt n x sqrt n) (toList)" $
            toListM value fromSerial
        , benchIO "(>>=) (sqrt n x sqrt n) (toListSome)" $
            toListSome value fromSerial
        , benchIO "naive prime sieve (n/4)"
            (\n -> S.sum $ sieve $ S.enumerateFromTo 2 (value `div` 4 + n))
        ]
    ]

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

toKv :: Int -> (Int, Int)
toKv p = (p, p)

{-# INLINE joinWith #-}
joinWith :: (S.MonadAsync m) =>
       ((Int -> Int -> Bool) -> SerialT m Int -> SerialT m Int -> SerialT m b)
    -> Int
    -> Int
    -> m ()
joinWith j val i =
    S.drain $ j (==) (sourceUnfoldrM val i) (sourceUnfoldrM val (val `div` 2))

{-# INLINE joinMapWith #-}
joinMapWith :: (S.MonadAsync m) =>
       (SerialT m (Int, Int) -> SerialT m (Int, Int) -> SerialT m b)
    -> Int
    -> Int
    -> m ()
joinMapWith j val i =
    S.drain
        $ j
            (fmap toKv (sourceUnfoldrM val i))
            (fmap toKv (sourceUnfoldrM val (val `div` 2)))

o_n_heap_buffering :: Int -> [Benchmark]
o_n_heap_buffering value =
    [ bgroup "buffered"
        [
          benchIOSrc1 "joinInner (sqrtVal)"
            $ joinWith Internal.joinInner sqrtVal
        , benchIOSrc1 "joinInnerMap"
            $ joinMapWith Internal.joinInnerMap halfVal
        , benchIOSrc1 "joinLeft (sqrtVal)"
            $ joinWith Internal.joinLeft sqrtVal
        , benchIOSrc1 "joinLeftMap "
            $ joinMapWith Internal.joinLeftMap halfVal
        , benchIOSrc1 "joinOuter (sqrtVal)"
            $ joinWith Internal.joinOuter sqrtVal
        , benchIOSrc1 "joinOuterMap"
            $ joinMapWith Internal.joinOuterMap halfVal
        , benchIOSrc1 "intersectBy (sqrtVal)"
            $ joinWith Internal.intersectBy sqrtVal
        , benchIOSrc1 "intersectBySorted"
            $ joinMapWith (Internal.intersectBySorted compare) halfVal
        -- XXX It hangs forever
        --, benchIOSrc1 "differenceBy"
        --    $ joinMapWith (Internal.differenceBy (==)) halfVal
        , benchIOSrc1 "differenceBySorted"
            $ joinMapWith (Internal.differenceBySorted compare) sqrtVal
        ]
    ]

    where

    halfVal = value `div` 2
    sqrtVal = round $ sqrt (fromIntegral value :: Double)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [
            -- multi-stream
              o_1_space_joining size
            , o_1_space_concatFoldable size
            , o_1_space_concat size

            , o_1_space_applicative size
            , o_1_space_monad size

            ]
        , bgroup (o_n_space_prefix moduleName) $ Prelude.concat
            [
            -- multi-stream
              o_n_space_applicative size
            , o_n_space_monad size
            , o_n_space_concat size
            ]
       , bgroup (o_n_heap_prefix moduleName) $
            -- multi-stream
            o_n_heap_buffering size
        ]
