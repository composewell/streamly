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
import Streamly.Benchmark.Prelude
    ( sourceFoldMapM, sourceFoldMapWith, sourceFoldMapWithM
    , sourceFoldMapWithStream, concatFoldableWith, concatForFoldableWith)
#else
import qualified Streamly.Internal.Data.Stream as S
#endif

import Test.Tasty.Bench
import Stream.Common
import Streamly.Benchmark.Common
import Prelude hiding (concatMap)

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
        Common.append
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1))

{-# INLINE serial4 #-}
serial4 :: Int -> Int -> IO ()
serial4 count n =
    drain $
    Common.append
        (Common.append
            (sourceUnfoldrM count n)
            (sourceUnfoldrM count (n + 1)))
        (Common.append
              (sourceUnfoldrM count (n + 2))
              (sourceUnfoldrM count (n + 3)))

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
    drain $ S.concatMap
        (S.replicate inner) (sourceUnfoldrM outer n)

#ifdef INSPECTION
#if __GLASGOW_HASKELL__ >= 906
inspect $ hasNoTypeClassesExcept 'concatMapRepl [''Applicative]
#else
inspect $ hasNoTypeClasses 'concatMapRepl
#endif
inspect $ 'concatMapRepl `hasNoType` ''SPEC
#endif

-- unfoldMany

-- unfoldMany replicate/unfoldrM

{-# INLINE unfoldManyRepl #-}
unfoldManyRepl :: Int -> Int -> Int -> IO ()
unfoldManyRepl outer inner n =
    drain
         $ S.unfoldEach
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

        -- This is for comparison with foldMapWith
        , benchIOSrc "concatMapId (n of 1) (fromFoldable)"
            (S.concatMap id . sourceConcatMapId value)

        , benchIOSrc1 "concatMapM (n of 1)"
            (concatMapM value 1)
        , benchIOSrc1 "concatMapM (sqrt n of sqrt n)"
            (concatMapM sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapM (1 of n)"
            (concatMapM 1 value)

        -- concatMap vs unfoldMany
        , benchIOSrc1 "concatMapRepl (sqrt n of sqrt n)"
            (concatMapRepl sqrtVal sqrtVal)
        , benchIOSrc1 "unfoldManyRepl (sqrt n of sqrt n)"
            (unfoldManyRepl sqrtVal sqrtVal)
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
        , benchIO "toNullApPure" $ toNullApPure value
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
        , benchIO "toNullPure" $ toNullMPure value
        , benchIO "toNull3Pure" $ toNullM3Pure value
        , benchIO "filterAllInPure" $ filterAllInMPure value
        , benchIO "filterAllOutPure" $ filterAllOutMPure value
        ]
    ]

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
            ]
       {-
       , bgroup (o_n_heap_prefix moduleName) $
            -- multi-stream
            o_n_heap_buffering size
       -}
        ]
