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
import Streamly.Data.Stream (Stream)
import qualified Streamly.Internal.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream
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
unfoldManyRepl outer inner n = drain $
     S.unfoldEach (UF.lmap ((inner,) . return) UF.replicateM)
        $ sourceUnfoldrM outer n

#ifdef INSPECTION
inspect $ hasNoTypeClasses 'unfoldManyRepl
inspect $ 'unfoldManyRepl `hasNoType` ''D.ConcatMapUState
inspect $ 'unfoldManyRepl `hasNoType` ''SPEC
#endif

o_1_space_concat :: Int -> [Benchmark]
o_1_space_concat value = sqrtVal `seq`
    [ bgroup "concat"
        [ benchIOSrc1 "concatMapPure outer=Max inner=1"
            (concatMapPure value 1)
        , benchIOSrc1 "concatMapPure outer=inner=(sqrt Max)"
            (concatMapPure sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapPure outer=1 inner=Max"
            (concatMapPure 1 value)

        , benchIOSrc1 "concatMap outer=max inner=1"
            (concatMap value 1)
        , benchIOSrc1 "concatMap outer=inner=(sqrt Max)"
            (concatMap sqrtVal sqrtVal)
        , benchIOSrc1 "concatMap outer=1 inner=Max"
            (concatMap 1 value)

        -- This is for comparison with foldMapWith
        , benchIOSrc "concatMapId outer=max inner=1 (fromFoldable)"
            (S.concatMap id . sourceConcatMapId value)

        , benchIOSrc1 "concatMapM outer=max inner=1"
            (concatMapM value 1)
        , benchIOSrc1 "concatMapM outer=inner=(sqrt Max)"
            (concatMapM sqrtVal sqrtVal)
        , benchIOSrc1 "concatMapM outer=1 inner=Max"
            (concatMapM 1 value)

        -- concatMap vs unfoldEach
        , benchIOSrc1 "concatMap replicate outer=inner=(sqrt Max)"
            (concatMapRepl sqrtVal sqrtVal)
        , benchIOSrc1 "unfoldEach replicate outer=Max inner=1"
            (unfoldManyRepl value 1)
        , benchIOSrc1 "unfoldEach replicate outer=inner=(sqrt Max)"
            (unfoldManyRepl sqrtVal sqrtVal)
        , benchIOSrc1 "unfoldEach replicate outer=1 inner=Max"
            (unfoldManyRepl 1 value)
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
        [ benchIO "then2" $ monadThen value
        , benchIO "drain2" $ toNullM value
        , benchIO "drain3" $ toNullM3 value
        , benchIO "filterAllOut2" $ filterAllOutM value
        , benchIO "filterAllIn2" $ filterAllInM value
        , benchIO "filterSome2" $ filterSome value
        , benchIO "breakAfterSome2" $ breakAfterSome value
        , benchIO "pureDrain2" $ toNullMPure value
        , benchIO "pureDrain3" $ toNullM3Pure value
        , benchIO "pureFilterAllIn2" $ filterAllInMPure value
        , benchIO "pureFilterAllOut2" $ filterAllOutMPure value
        ]
    ]

o_n_space_monad :: Int -> [Benchmark]
o_n_space_monad value =
    [ bgroup "Monad"
        [ benchIO "toList2" $ toListM value
        , benchIO "toListSome2" $ toListSome value
        ]
    ]

{-# INLINE drainConcatFor1 #-}
drainConcatFor1 :: Monad m => Stream m Int -> m ()
drainConcatFor1 s = drain $ do
    Stream.concatFor s $ \x ->
            Stream.fromPure $ x + 1

{-# INLINE drainConcatFor #-}
drainConcatFor :: Monad m => Stream m Int -> m ()
drainConcatFor s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            Stream.fromPure $ x + y

{-# INLINE drainConcatForM #-}
drainConcatForM :: Monad m => Stream m Int -> m ()
drainConcatForM s = drain $ do
    Stream.concatForM s $ \x ->
        pure $ Stream.concatForM s $ \y ->
            pure $ Stream.fromPure $ x + y

{-# INLINE drainConcatFor3 #-}
drainConcatFor3 :: Monad m => Stream m Int -> m ()
drainConcatFor3 s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            Stream.concatFor s $ \z ->
                Stream.fromPure $ x + y + z

{-# INLINE drainConcatFor4 #-}
drainConcatFor4 :: Monad m => Stream m Int -> m ()
drainConcatFor4 s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            Stream.concatFor s $ \z ->
                Stream.concatFor s $ \w ->
                    Stream.fromPure $ x + y + z + w

{-# INLINE drainConcatFor5 #-}
drainConcatFor5 :: Monad m => Stream m Int -> m ()
drainConcatFor5 s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            Stream.concatFor s $ \z ->
                Stream.concatFor s $ \w ->
                    Stream.concatFor s $ \u ->
                        Stream.fromPure $ x + y + z + w + u

{-# INLINE drainConcatFor3M #-}
drainConcatFor3M :: Monad m => Stream m Int -> m ()
drainConcatFor3M s = drain $ do
    Stream.concatForM s $ \x ->
        pure $ Stream.concatForM s $ \y ->
            pure $ Stream.concatForM s $ \z ->
                pure $ Stream.fromPure $ x + y + z

{-# INLINE filterAllInConcatFor #-}
filterAllInConcatFor
    :: Monad m
    => Stream m Int -> m ()
filterAllInConcatFor s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            let s1 = x + y
             in if s1 > 0
                then Stream.fromPure s1
                else Stream.nil

{-# INLINE filterAllOutConcatFor #-}
filterAllOutConcatFor
    :: Monad m
    => Stream m Int -> m ()
filterAllOutConcatFor s = drain $ do
    Stream.concatFor s $ \x ->
        Stream.concatFor s $ \y ->
            let s1 = x + y
             in if s1 < 0
                then Stream.fromPure s1
                else Stream.nil

o_1_space_bind :: Int -> [Benchmark]
o_1_space_bind streamLen =
    [ bgroup "concatFor"
        [ benchFold "drain1" drainConcatFor1   (sourceUnfoldrM streamLen)
        , benchFold "drain2" drainConcatFor   (sourceUnfoldrM streamLen2)
        , benchFold "drain3" drainConcatFor3   (sourceUnfoldrM streamLen3)
        , benchFold "drain4" drainConcatFor4   (sourceUnfoldrM streamLen4)
        , benchFold "drain5" drainConcatFor5   (sourceUnfoldrM streamLen5)
        , benchFold "drainM2" drainConcatForM   (sourceUnfoldrM streamLen2)
        , benchFold "drainM3" drainConcatFor3M   (sourceUnfoldrM streamLen3)
        , benchFold "filterAllIn2"  filterAllInConcatFor  (sourceUnfoldrM streamLen2)
        , benchFold "filterAllOut2" filterAllOutConcatFor (sourceUnfoldrM streamLen2)
        ]
    ]

    where

    streamLen2 = round (fromIntegral streamLen**(1/2::Double)) -- double nested loop
    streamLen3 = round (fromIntegral streamLen**(1/3::Double)) -- triple nested loop
    streamLen4 = round (fromIntegral streamLen**(1/4::Double)) -- 4 times nested loop
    streamLen5 = round (fromIntegral streamLen**(1/5::Double)) -- 5 times nested loop

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
            , o_1_space_bind size
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
