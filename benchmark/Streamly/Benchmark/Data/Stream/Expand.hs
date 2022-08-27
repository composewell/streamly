-- |
-- Module      : Stream.Expand
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

module Stream.Expand (benchmarks) where

import Control.Monad.Trans.Class (lift)
import Streamly.Internal.Data.Stream (Stream)

import qualified Control.Applicative as AP

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection

import qualified Streamly.Internal.Data.Stream.StreamD as D
#endif

import qualified Stream.Common as Common
#ifdef USE_PRELUDE
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Streamly.Benchmark.Prelude
    ( sourceFoldMapM, sourceFoldMapWith, sourceFoldMapWithM
    , sourceFoldMapWithStream, concatFoldableWith, concatForFoldableWith)
#else
import qualified Streamly.Internal.Data.Stream as S
#endif
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Fold as Fold

import Gauge
import Stream.Common hiding (append2)
import Streamly.Benchmark.Common
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
iterateSingleton :: Monad m
    => (Int -> Stream m Int -> Stream m Int)
    -> Int
    -> Int
    -> Stream m Int
iterateSingleton g count n = iterateN g (return n) count

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
    drain $
        Common.append (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1))

{-# INLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    drain $
    Common.append
        (Common.append (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1)))
        (Common.append
              (sourceUnfoldrM count (n + 2))
              (sourceUnfoldrM count (n + 3)))

{-# INLINE append2 #-}
append2 :: Int -> Int -> IO ()
append2 count n =
    drain $
    Common.append2 (sourceUnfoldrM count n) (sourceUnfoldrM count (n + 1))

{-# INLINE append4 #-}
append4 :: Int -> Int -> IO ()
append4 count n =
    drain $
    Common.append2
        (Common.append2
              (sourceUnfoldrM count n)
              (sourceUnfoldrM count (n + 1)))
        (Common.append2
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
    drain $ S.concatMap
        (\_ -> sourceUnfoldrM inner n)
        (sourceUnfoldrM outer n)

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

-- concatMap unfoldr/unfoldr

{-# INLINE concatMapPure #-}
concatMapPure :: Int -> Int -> Int -> IO ()
concatMapPure outer inner n =
    drain $ S.concatMap
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
    drain $ S.concatMap (S.replicate inner) (sourceUnfoldrM outer n)

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapRepl
inspect $ 'concatMapRepl `hasNoType` ''SPEC
#endif

-- concatMapWith

{-# INLINE concatMapWithSerial #-}
concatMapWithSerial :: Int -> Int -> Int -> IO ()
concatMapWithSerial = concatStreamsWith Common.append

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithSerial
inspect $ 'concatMapWithSerial `hasNoType` ''SPEC
#endif

{-# INLINE concatMapWithAppend #-}
concatMapWithAppend :: Int -> Int -> Int -> IO ()
concatMapWithAppend = concatStreamsWith Common.append2

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'concatMapWithAppend
inspect $ 'concatMapWithAppend `hasNoType` ''SPEC
#endif

-- concatPairWith

{-# INLINE concatPairWithSerial #-}
concatPairWithSerial :: Int -> Int -> Int -> IO ()
concatPairWithSerial = concatPairsWith Common.append

{-# INLINE concatPairWithAppend #-}
concatPairWithAppend :: Int -> Int -> Int -> IO ()
concatPairWithAppend = concatPairsWith Common.append2

-- unfoldMany

-- unfoldMany replicate/unfoldrM

{-# INLINE unfoldManyRepl #-}
unfoldManyRepl :: Int -> Int -> Int -> IO ()
unfoldManyRepl outer inner n =
    drain
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
        , benchIOSrc "concatMapId (n of 1) (fromFoldable)"
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
        , benchIOSrc "concatMapWithId (n of 1) (fromFoldable)"
            (S.concatMapWith Common.append id . sourceConcatMapId value)

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
        [ benchIO "(*>) (sqrt n x sqrt n)" $ apDiscardFst value
        , benchIO "(<*) (sqrt n x sqrt n)" $ apDiscardSnd value
        , benchIO "(<*>) (sqrt n x sqrt n)" $ toNullAp value
        , benchIO "liftA2 (sqrt n x sqrt n)" $ apLiftA2 value
        ]
    ]

o_n_space_applicative :: Int -> [Benchmark]
o_n_space_applicative value =
    [ bgroup "Applicative"
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

-- This is a good benchmark but inefficient way to compute primes. As we see a
-- new prime we keep appending a division filter for all the future numbers.
{-# INLINE sieve #-}
sieve :: Monad m => Stream m Int -> Stream m Int
sieve s = do
    r <- lift $ S.uncons s
    case r of
        Just (prime, rest) ->
            prime `S.cons` sieve (S.filter (\n -> n `mod` prime /= 0) rest)
        Nothing -> S.nil

o_n_space_monad :: Int -> [Benchmark]
o_n_space_monad value =
    [ bgroup "Monad"
        [ benchIOSrc "(>>) (n times)" $
            iterateSingleton ((>>) . pure) value
        , benchIOSrc "(>>=) (n times)" $
            iterateSingleton (\x xs -> xs >>= \y -> return (x + y)) value
        , benchIO "(>>=) (sqrt n x sqrt n) (toList)" $
            toListM value
        , benchIO "(>>=) (sqrt n x sqrt n) (toListSome)" $
            toListSome value
        , benchIO "naive prime sieve (n/4)"
            (\n -> S.fold Fold.sum $ sieve $ S.enumerateFromTo 2 (value `div` 4 + n))
        ]
    ]

-------------------------------------------------------------------------------
-- Joining
-------------------------------------------------------------------------------

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
          benchIOSrc1 "joinInner (sqrtVal)"
            $ joinWith S.joinInner sqrtVal
        , benchIOSrc1 "joinInnerMap"
            $ joinMapWith S.joinInnerMap halfVal
        , benchIOSrc1 "joinLeft (sqrtVal)"
            $ joinWith S.joinLeft sqrtVal
        , benchIOSrc1 "joinLeftMap "
            $ joinMapWith S.joinLeftMap halfVal
        , benchIOSrc1 "joinOuter (sqrtVal)"
            $ joinWith S.joinOuter sqrtVal
        , benchIOSrc1 "joinOuterMap"
            $ joinMapWith S.joinOuterMap halfVal
        , benchIOSrc1 "intersectBy (sqrtVal)"
            $ joinWith S.intersectBy sqrtVal
        , benchIOSrc1 "intersectBySorted"
            $ joinMapWith (S.intersectBySorted compare) halfVal
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
              o_n_space_applicative size
            , o_n_space_monad size
            , o_n_space_concat size
            ]
       , bgroup (o_n_heap_prefix moduleName) $
            -- multi-stream
            o_n_heap_buffering size
        ]
