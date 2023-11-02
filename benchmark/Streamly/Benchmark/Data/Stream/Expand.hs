-- |
-- Module      : Stream.Expand
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

#ifdef USE_PRELUDE
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Stream.Expand (benchmarks) where

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream as D
#endif

import qualified Stream.Common as Common
import qualified Streamly.Internal.Data.Unfold as UF

#ifdef USE_PRELUDE
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Internal.Data.Stream.IsStream as StreamK
import Streamly.Benchmark.Prelude
    ( sourceFoldMapM, sourceFoldMapWith, sourceFoldMapWithM
    , sourceFoldMapWithStream, concatFoldableWith, concatForFoldableWith)
#else
import qualified Streamly.Internal.Data.Stream as S
#ifdef USE_STREAMK
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.StreamK (StreamK, CrossStreamK)
import qualified Control.Applicative as AP
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.StreamK as StreamK
#else
import qualified Streamly.Internal.Data.Stream as StreamK
#endif
#endif

import Test.Tasty.Bench
import Stream.Common
import Streamly.Benchmark.Common
import Prelude hiding (concatMap)

-------------------------------------------------------------------------------
-- Iteration/looping utilities
-------------------------------------------------------------------------------

#ifdef USE_STREAMK
{-# INLINE iterateN #-}
iterateN :: (Int -> a -> a) -> a -> Int -> a
iterateN g initial count = f count initial

    where

    f (0 :: Int) x = x
    f i x = f (i - 1) (g i x)

-- Iterate a transformation over a singleton stream
{-# INLINE iterateSingleton #-}
iterateSingleton :: Applicative m =>
       (Int -> CrossStreamK m Int -> CrossStreamK m Int)
    -> Int
    -> Int
    -> Stream m Int
iterateSingleton g count n =
    toStream
        $ StreamK.unCross
        $ iterateN g (StreamK.mkCross (StreamK.fromPure n)) count
#endif

{-
-- XXX need to check why this is slower than the explicit recursion above, even
-- if the above code is written in a foldr like head recursive way. We also
-- need to try this with foldlM' once #150 is fixed.
-- However, it is perhaps best to keep the iteration benchmarks independent of
-- foldrM and any related fusion issues.
{-# INLINE _iterateSingleton #-}
_iterateSingleton ::
       Monad m
    => (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
_iterateSingleton g value n = S.foldrM g (return n) $ sourceIntFromTo value n
-}

-------------------------------------------------------------------------------
-- Multi-Stream
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Appending
-------------------------------------------------------------------------------

{-# INLINE serial2 #-}
serial2 :: Int -> Int -> IO ()
serial2 count n =
    drain $ toStream $
        Common.append
            (fromStream $ sourceUnfoldrM count n)
            (fromStream $ sourceUnfoldrM count (n + 1))

{-# INLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    drain $ toStream $
    Common.append
        (Common.append
            (fromStream $ sourceUnfoldrM count n)
            (fromStream $ sourceUnfoldrM count (n + 1)))
        (Common.append
              (fromStream $ sourceUnfoldrM count (n + 2))
              (fromStream $ sourceUnfoldrM count (n + 3)))

o_1_space_joining :: Int -> [Benchmark]
o_1_space_joining value =
    [ bgroup "joining"
        [ benchIOSrc1 "serial (2,x/2)" (serial2 (value `div` 2))
        , benchIOSrc1 "serial (2,2,x/4)" (serial4 (value `div` 4))
        ]
    ]

-------------------------------------------------------------------------------
-- Concat Foldable containers
-------------------------------------------------------------------------------

#ifdef USE_PRELUDE
o_1_space_concatFoldable :: Int -> [Benchmark]
o_1_space_concatFoldable value =
    [ bgroup "concat-foldable"
        [ benchIOSrc "foldMapWith (<>) (List)"
            (sourceFoldMapWith value)
        , benchIOSrc "foldMapWith (<>) (Stream)"
            (sourceFoldMapWithStream value)
        , benchIOSrc "foldMapWithM (<>) (List)"
            (sourceFoldMapWithM value)
        , benchIOSrc "S.concatFoldableWith (<>) (List)"
            (concatFoldableWith value)
        , benchIOSrc "S.concatForFoldableWith (<>) (List)"
            (concatForFoldableWith value)
        , benchIOSrc "foldMapM (List)" (sourceFoldMapM value)
        ]
    ]
#endif

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

-- concatMap unfoldrM/unfoldrM

{-# INLINE concatMap #-}
concatMap :: Int -> Int -> Int -> IO ()
concatMap outer inner n =
    drain $ toStream $ StreamK.concatMap
        (\_ -> fromStream $ sourceUnfoldrM inner n)
        (fromStream $ sourceUnfoldrM outer n)

#ifndef USE_STREAMK
{-# INLINE concatMapM #-}
concatMapM :: Int -> Int -> Int -> IO ()
concatMapM outer inner n =
    drain $ S.concatMapM
        (\_ -> return $ sourceUnfoldrM inner n)
        (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMap
inspect $ 'concatMap `hasNoType` ''SPEC
#endif
#endif

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    drain $ toStream $ StreamK.concatMap
        (\_ -> fromStream $ sourceUnfoldr inner n)
        (fromStream $ sourceUnfoldr outer n)

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'concatMapPure [''Applicative]
#else
inspect $ hasNoTypeClasses 'concatMapPure
#endif
inspect $ 'concatMapPure `hasNoType` ''SPEC
#endif

-- concatMap replicate/unfoldrM

{-# INLINE concatMapRepl #-}
concatMapRepl :: Int -> Int -> Int -> IO ()
concatMapRepl outer inner n =
    drain $ toStream $ StreamK.concatMap
        (fromStream . S.replicate inner) (fromStream $ sourceUnfoldrM outer n)

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'concatMapRepl [''Applicative]
#else
inspect $ hasNoTypeClasses 'concatMapRepl
#endif
inspect $ 'concatMapRepl `hasNoType` ''SPEC
#endif

-- concatMapWith

#ifdef USE_STREAMK
{-# INLINE concatMapWithSerial #-}
concatMapWithSerial :: Int -> Int -> Int -> IO ()
concatMapWithSerial = concatStreamsWith Common.append

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithSerial
inspect $ 'concatMapWithSerial `hasNoType` ''SPEC
#endif

{-
{-# INLINE concatMapWithAppend #-}
concatMapWithAppend :: Int -> Int -> Int -> IO ()
concatMapWithAppend = concatStreamsWith Common.append2

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithAppend
inspect $ 'concatMapWithAppend `hasNoType` ''SPEC
#endif
-}

-- mergeMapWith

{-# INLINE mergeMapWithSerial #-}
mergeMapWithSerial :: Int -> Int -> Int -> IO ()
mergeMapWithSerial = mergeMapWith Common.append

{-
{-# INLINE mergeMapWithAppend #-}
mergeMapWithAppend :: Int -> Int -> Int -> IO ()
mergeMapWithAppend = mergeMapWith Common.append2
-}
#endif

-- unfoldMany

-- unfoldMany replicate/unfoldrM

{-# INLINE unfoldManyRepl #-}
unfoldManyRepl :: Int -> Int -> Int -> IO ()
unfoldManyRepl outer inner n =
    drain
         $ S.unfoldMany
               UF.replicateM
               (fmap ((inner,) . return) (sourceUnfoldrM outer n))


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

        , benchIOSrc1 "concatMap (n of 1)"
            (concatMap value 1)
        , benchIOSrc1 "concatMap (sqrt n of sqrt n)"
            (concatMap sqrtVal sqrtVal)
        , benchIOSrc1 "concatMap (1 of n)"
            (concatMap 1 value)

#ifndef USE_STREAMK
        -- This is for comparison with foldMapWith
        , benchIOSrc "concatMapId (n of 1) (fromFoldable)"
            (S.concatMap id . sourceConcatMapId value)

        , benchIOSrc1 "concatMapM (n of 1)"
            (concatMapM value 1)
        , benchIOSrc1 "concatMapM (sqrt n of sqrt n)"
            (concatMapM sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapM (1 of n)"
            (concatMapM 1 value)
#endif

#ifdef USE_STREAMK
        {-
        -- This is for comparison with foldMapWith
        , benchIOSrc "concatMapWithId (n of 1) (fromFoldable)"
            (toStream . S.concatMapWith Common.append id . sourceConcatMapId value)
        -}

        , benchIOSrc1 "concatMapWith (n of 1)"
            (concatMapWithSerial value 1)
        , benchIOSrc1 "concatMapWith (sqrt n of sqrt n)"
            (concatMapWithSerial sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapWith (1 of n)"
            (concatMapWithSerial 1 value)

        {-
        -- quadratic with number of outer streams
        , benchIOSrc1 "concatMapWithAppend (2 of n/2)"
            (concatMapWithAppend 2 (value `div` 2))
        -}
#endif

        -- concatMap vs unfoldMany
        , benchIOSrc1 "concatMapRepl (sqrt n of sqrt n)"
            (concatMapRepl sqrtVal sqrtVal)
        , benchIOSrc1 "unfoldManyRepl (sqrt n of sqrt n)"
            (unfoldManyRepl sqrtVal sqrtVal)
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)

#ifdef USE_STREAMK
o_n_space_merge :: Int -> [Benchmark]
o_n_space_merge value = sqrtVal `seq`
    [ bgroup "concat"
        [
        -------------------mergeMapWith-----------------

        -- Use large number of streams to check scalability

          benchIOSrc1 "mergeMapWithSerial (n of 1)"
            (mergeMapWithSerial value 1)
        , benchIOSrc1 "mergeMapWithSerial (sqrtVal of sqrtVal)"
            (mergeMapWithSerial sqrtVal sqrtVal)
        , benchIOSrc1 "mergeMapWithSerial (2 of n/2)"
            (mergeMapWithSerial 2 (value `div` 2))

        {-
        , benchIOSrc1 "mergeMapWithAppend (n of 1)"
            (mergeMapWithAppend value 1)
        , benchIOSrc1 "mergeMapWithAppend (sqrtVal of sqrtVal)"
            (mergeMapWithAppend sqrtVal sqrtVal)
        -}
        ]
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral value :: Double)
#endif

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

o_1_space_applicative :: Int -> [Benchmark]
o_1_space_applicative value =
    [ bgroup "Applicative"
        [ benchIO "(*>) (sqrt n x sqrt n)" $ apDiscardFst value
        , benchIO "(<*) (sqrt n x sqrt n)" $ apDiscardSnd value
        , benchIO "(<*>) (sqrt n x sqrt n)" $ toNullAp value
        , benchIO "liftA2 (sqrt n x sqrt n)" $ apLiftA2 value
        ]
    ]

#ifdef USE_STREAMK
o_n_space_applicative :: Int -> [Benchmark]
o_n_space_applicative value =
    [ bgroup "iterated"
        [ benchIOSrc "(*>) (n times)" $
            iterateSingleton ((*>) . pure) value
        , benchIOSrc "(<*) (n times)" $
            iterateSingleton (\x xs -> xs <* pure x) value
        , benchIOSrc "(<*>) (n times)" $
            iterateSingleton (\x xs -> pure (+ x) <*> xs) value
        , benchIOSrc "liftA2 (n times)" $
            iterateSingleton (AP.liftA2 (+) . pure) value
        ]
    ]
#endif

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

o_1_space_monad :: Int -> [Benchmark]
o_1_space_monad value =
    [ bgroup "Monad"
        [ benchIO "(>>) (sqrt n x sqrt n)" $ monadThen value
        , benchIO "(>>=) (sqrt n x sqrt n)" $ toNullM value
        , benchIO "(>>=) (sqrt n x sqrt n) (filterAllOut)" $
            filterAllOutM value
        , benchIO "(>>=) (sqrt n x sqrt n) (filterAllIn)" $
            filterAllInM value
        , benchIO "(>>=) (sqrt n x sqrt n) (filterSome)" $
            filterSome value
        , benchIO "(>>=) (sqrt n x sqrt n) (breakAfterSome)" $
            breakAfterSome value
        , benchIO "(>>=) (cubert n x cubert n x cubert n)" $
            toNullM3 value
        ]
    ]

#ifdef USE_STREAMK
-- This is a good benchmark but inefficient way to compute primes. As we see a
-- new prime we keep appending a division filter for all the future numbers.
{-# INLINE sieve #-}
sieve :: Monad m => StreamK m Int -> StreamK m Int
sieve s = StreamK.concatEffect $ do
    r <- StreamK.uncons s
    case r of
        Just (prime, rest) ->
            -- XXX Use K.filter or rewrite to K.filter
            let f = S.filter (\n -> n `mod` prime /= 0)
             in pure $ prime `StreamK.cons` sieve (fromStream $ f $ toStream rest)
        Nothing -> pure StreamK.nil

o_n_space_iterated :: Int -> [Benchmark]
o_n_space_iterated value =
    [ bgroup "iterated"
        [
          benchIO "concatEffect prime sieve (n/4)"
            (\n ->
                  S.fold Fold.sum
                $ toStream
                $ sieve
                $ fromStream
                $ S.enumerateFromTo 2 (value `div` 4 + n))
        , benchIOSrc "(>>) (n times)" $
            iterateSingleton ((>>) . pure) value
        , benchIOSrc "(>>=) (n times)" $
            iterateSingleton (\x xs -> xs >>= \y -> return (x + y)) value
        ]
    ]
#endif

o_n_space_monad :: Int -> [Benchmark]
o_n_space_monad value =
    [ bgroup "Monad"
        [ benchIO "(>>=) (sqrt n x sqrt n) (toList)" $
            toListM value
        , benchIO "(>>=) (sqrt n x sqrt n) (toListSome)" $
            toListSome value
        ]
    ]

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

{-
toKv :: Int -> (Int, Int)
toKv p = (p, p)

{-# INLINE joinWith #-}
joinWith :: Common.MonadAsync m =>
       ((Int -> Int -> Bool) -> Stream m Int -> Stream m Int -> Stream m b)
    -> Int
    -> Int
    -> m ()
joinWith j val i =
    drain $ j (==) (sourceUnfoldrM val i) (sourceUnfoldrM val (val `div` 2))

{-# INLINE joinMapWith #-}
joinMapWith :: Common.MonadAsync m =>
       (Stream m (Int, Int) -> Stream m (Int, Int) -> Stream m b)
    -> Int
    -> Int
    -> m ()
joinMapWith j val i =
    drain
        $ j
            (fmap toKv (sourceUnfoldrM val i))
            (fmap toKv (sourceUnfoldrM val (val `div` 2)))

o_n_heap_buffering :: Int -> [Benchmark]
o_n_heap_buffering value =
    [ bgroup "buffered"
        [
          benchIOSrc1 "joinInnerGeneric (sqrtVal)"
            $ joinWith S.joinInnerGeneric sqrtVal
        , benchIOSrc1 "joinInner"
            $ joinMapWith S.joinInner halfVal
        , benchIOSrc1 "joinLeftGeneric (sqrtVal)"
            $ joinWith S.joinLeftGeneric sqrtVal
        , benchIOSrc1 "joinLeft "
            $ joinMapWith S.joinLeft halfVal
        , benchIOSrc1 "joinOuterGeneric (sqrtVal)"
            $ joinWith S.joinOuterGeneric sqrtVal
        , benchIOSrc1 "joinOuter"
            $ joinMapWith S.joinOuter halfVal
        , benchIOSrc1 "filterInStreamGenericBy (sqrtVal)"
            $ joinWith S.filterInStreamGenericBy sqrtVal
        , benchIOSrc1 "filterInStreamAscBy"
            $ joinMapWith (S.filterInStreamAscBy compare) halfVal
        ]
    ]

    where

    halfVal = value `div` 2
    sqrtVal = round $ sqrt (fromIntegral value :: Double)
-}

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

-- In addition to gauge options, the number of elements in the stream can be
-- passed using the --stream-size option.
--
{-# ANN benchmarks "HLint: ignore" #-}
benchmarks :: String -> Int -> [Benchmark]
benchmarks moduleName size =
        [ bgroup (o_1_space_prefix moduleName) $ Prelude.concat
            [
            -- multi-stream
              o_1_space_joining size
#ifdef USE_PRELUDE
            , o_1_space_concatFoldable size
#endif
            , o_1_space_concat size
            , o_1_space_applicative size
            , o_1_space_monad size
            ]
        , bgroup (o_n_space_prefix moduleName) $ Prelude.concat
            [
            -- multi-stream
              o_n_space_monad size
#ifdef USE_STREAMK
            , o_n_space_merge size
            , o_n_space_iterated size
            , o_n_space_applicative size
#endif
            ]
       {-
       , bgroup (o_n_heap_prefix moduleName) $
            -- multi-stream
            o_n_heap_buffering size
       -}
        ]
