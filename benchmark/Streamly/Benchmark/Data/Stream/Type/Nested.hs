-- |
-- Module      : Stream.Type.Nested
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

-- | Benchmarks for the 'Applicative' and 'Monad' instances of streams, i.e.
-- the nested looping (cross product) and @concatFor@ combinators.
module Stream.Type.Nested
    ( benchmarks
    ) where

import Streamly.Internal.Data.Stream (Stream)

import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Stream.Common hiding (benchIO)
import Stream.Type.Basic (benchIO, withStream)
import Streamly.Benchmark.Common
import Fusion.Plugin.Types
import qualified Streamly.Internal.Data.SVar.Type as SVar
import Prelude hiding (concatMap, mapM, zipWith)

mkCross :: Stream m a -> Stream.Nested m a
mkCross = Stream.Nested

unCross :: Stream.Nested m a -> Stream m a
unCross = Stream.unNested

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

{-# INLINE toNullApPure #-}
toNullApPure :: Int -> Int -> IO ()
toNullApPure linearCount start = drain $ unCross $
    (+) <$> mkCross (sourceUnfoldr nestedCount2 start)
        <*> mkCross (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullMPure #-}
toNullMPure :: Int -> Int -> IO ()
toNullMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM3Pure #-}
toNullM3Pure :: Int -> Int -> IO ()
toNullM3Pure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount3 start)
    y <- mkCross (sourceUnfoldr nestedCount3 start)
    z <- mkCross (sourceUnfoldr nestedCount3 start)
    return $ x + y + z

    where

    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE filterAllOutMPure #-}
filterAllOutMPure :: Int -> Int -> IO ()
filterAllOutMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    let s = x + y
    if s < 0
    then return s
    else mkCross Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE filterAllInMPure #-}
filterAllInMPure :: Int -> Int -> IO ()
filterAllInMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    let s = x + y
    if s > 0
    then return s
    else mkCross Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# ANN cross2 (PermitPatternMatches [''Int]) #-}
{-# ANN cross2 (PermitConstructions []) #-}
{-# ANN cross2 (PermitTypeClasses []) #-}
{-# NOINLINE cross2 #-}
cross2 :: Int -> Int -> IO ()
cross2 linearCount start = drain $
    Stream.crossWith (+)
        (sourceUnfoldr nestedCount2 start)
        (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# ANN crossApply (PermitPatternMatches [''Int]) #-}
{-# ANN crossApply (PermitConstructions [''Int]) #-}
{-# ANN crossApply (PermitTypeClasses []) #-}
{-# NOINLINE crossApply #-}
crossApply :: Int -> Int -> IO ()
crossApply linearCount start = drain $
    Stream.crossApply
        ((+) <$> sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# ANN crossApplyFst (PermitPatternMatches [''Int]) #-}
{-# ANN crossApplyFst (PermitConstructions []) #-}
{-# ANN crossApplyFst (PermitTypeClasses []) #-}
{-# NOINLINE crossApplyFst #-}
crossApplyFst :: Int -> Int -> IO ()
crossApplyFst linearCount start = drain $
    Stream.crossApplyFst
        (sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# ANN crossApplySnd (PermitPatternMatches [''Int]) #-}
{-# ANN crossApplySnd (PermitConstructions []) #-}
{-# ANN crossApplySnd (PermitTypeClasses []) #-}
{-# NOINLINE crossApplySnd #-}
crossApplySnd :: Int -> Int -> IO ()
crossApplySnd linearCount start = drain $
    Stream.crossApplySnd
        (sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

{-# ANN drainConcatFor1 (PermitPatternMatches [''Bool,''Int,''Stream.Step]) #-}
{-# ANN drainConcatFor1 (PermitConstructions [''Int,''Stream.Step,''SVar.State,''Maybe,''Bool]) #-}
{-# ANN drainConcatFor1 (PermitTypeClasses []) #-}
{-# NOINLINE drainConcatFor1 #-}
drainConcatFor1 :: Int -> Int -> IO ()
drainConcatFor1 count = withStream count $ \s ->
    drain $ Stream.concatFor s $ \x ->
        Stream.fromPure $ x + 1

{-# ANN drainConcatFor (PermitPatternMatches [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN drainConcatFor (PermitConstructions [''Int,''Stream.Step,''Either,''SVar.State,''Maybe,''(,),''Stream,''Bool]) #-}
{-# ANN drainConcatFor (PermitTypeClasses []) #-}
{-# NOINLINE drainConcatFor #-}
drainConcatFor :: Int -> Int -> IO ()
drainConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.fromPure $ x + y

{-# ANN drainConcatForM (PermitPatternMatches [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN drainConcatForM (PermitConstructions [''Int,''Stream.Step,''Either,''SVar.State,''Maybe,''(,),''Stream,''Bool]) #-}
{-# ANN drainConcatForM (PermitTypeClasses []) #-}
{-# NOINLINE drainConcatForM #-}
drainConcatForM :: Int -> Int -> IO ()
drainConcatForM count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.fromPure $ x + y

{-# ANN drainConcatFor3 (PermitPatternMatches [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN drainConcatFor3 (PermitConstructions [''Int,''Stream.Step,''Either,''Stream,''SVar.State,''Maybe,''(,),''Bool]) #-}
{-# ANN drainConcatFor3 (PermitTypeClasses []) #-}
{-# NOINLINE drainConcatFor3 #-}
drainConcatFor3 :: Int -> Int -> IO ()
drainConcatFor3 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.fromPure $ x + y + z

{-# ANN drainConcatFor4 (PermitPatternMatches [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN drainConcatFor4 (PermitConstructions [''Int,''Stream.Step,''Either,''Stream,''SVar.State,''Maybe,''(,),''Bool]) #-}
{-# ANN drainConcatFor4 (PermitTypeClasses []) #-}
{-# NOINLINE drainConcatFor4 #-}
drainConcatFor4 :: Int -> Int -> IO ()
drainConcatFor4 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.fromPure $ x + y + z + w

{-# ANN drainConcatFor5 (PermitPatternMatches [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN drainConcatFor5 (PermitConstructions [''Int,''Stream.Step,''Either,''Stream,''SVar.State,''Maybe,''(,),''Bool]) #-}
{-# ANN drainConcatFor5 (PermitTypeClasses []) #-}
{-# NOINLINE drainConcatFor5 #-}
drainConcatFor5 :: Int -> Int -> IO ()
drainConcatFor5 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.concatFor s $ \u ->
                            Stream.fromPure $ x + y + z + w + u

{-# ANN drainConcatFor3M (PermitPatternMatches [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN drainConcatFor3M (PermitConstructions [''Int,''Stream.Step,''Either,''Stream,''SVar.State,''Maybe,''(,),''Bool]) #-}
{-# ANN drainConcatFor3M (PermitTypeClasses []) #-}
{-# NOINLINE drainConcatFor3M #-}
drainConcatFor3M :: Int -> Int -> IO ()
drainConcatFor3M count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.concatForM s $ \z ->
                    pure $ Stream.fromPure $ x + y + z

{-# ANN filterAllInConcatFor (PermitPatternMatches [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN filterAllInConcatFor (PermitConstructions [''Stream,''Int,''Stream.Step,''Either,''SVar.State,''Maybe,''(,),''(),''Bool]) #-}
{-# ANN filterAllInConcatFor (PermitTypeClasses []) #-}
{-# NOINLINE filterAllInConcatFor #-}
filterAllInConcatFor :: Int -> Int -> IO ()
filterAllInConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                let s1 = x + y
                 in if s1 > 0
                    then Stream.fromPure s1
                    else Stream.nil

{-# ANN filterAllOutConcatFor (PermitPatternMatches [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN filterAllOutConcatFor (PermitConstructions [''Stream,''Int,''Stream.Step,''Either,''SVar.State,''Maybe,''(,),''(),''Bool]) #-}
{-# ANN filterAllOutConcatFor (PermitTypeClasses []) #-}
{-# NOINLINE filterAllOutConcatFor #-}
filterAllOutConcatFor :: Int -> Int -> IO ()
filterAllOutConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                let s1 = x + y
                 in if s1 < 0
                    then Stream.fromPure s1
                    else Stream.nil

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

{-# ANN benchmarks "HLint: ignore" #-}
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Applicative
    [ (SpaceO_1, benchIO "(*>)" (apDiscardFst size))
    , (SpaceO_1, benchIO "(<*)" (apDiscardSnd size))
    , (SpaceO_1, benchIO "(<*>)" (toNullAp size))
    , (SpaceO_1, benchIO "liftA2" (apLiftA2 size))
    , (SpaceO_1, benchIO "crossApply" $ crossApply size)
    , (SpaceO_1, benchIO "crossApplyFst" $ crossApplyFst size)
    , (SpaceO_1, benchIO "crossApplySnd" $ crossApplySnd size)
    , (SpaceO_1, benchIO "pureDrain2" (toNullApPure size))
    , (SpaceO_1, benchIO "pureCross2" $ cross2 size)

    -- Monad
    , (SpaceO_1, benchIO "then2M" (monadThen size))
    , (SpaceO_1, benchIO "drain2M" (toNullM size))
    , (SpaceO_1, benchIO "drain3M" (toNullM3 size))
    , (SpaceO_1, benchIO "filterAllOut2M" (filterAllOutM size))
    , (SpaceO_1, benchIO "filterAllIn2M" (filterAllInM size))
    , (SpaceO_1, benchIO "filterSome2M" (filterSome size))
    , (SpaceO_1, benchIO "breakAfterSome2M" (breakAfterSome size))
    , (SpaceO_1, benchIO "pureDrain2M" (toNullMPure size))
    , (SpaceO_1, benchIO "pureDrain3M" (toNullM3Pure size))
    , (SpaceO_1, benchIO "pureFilterAllIn2M" (filterAllInMPure size))
    , (SpaceO_1, benchIO "pureFilterAllOut2M" (filterAllOutMPure size))
    , (SpaceO_n, benchIO "toList2M" (toListM size))
    , (SpaceO_n, benchIO "toListSome2M" (toListSome size))

    -- concatFor (bind)
    , (SpaceO_1, benchIO "concatFor/drain1" $ drainConcatFor1 size)
    , (SpaceO_1, benchIO "concatFor/drain2" $ drainConcatFor sqrtVal)
    , (SpaceO_1, benchIO "concatFor/drain3" $ drainConcatFor3 cubertVal)
    , (SpaceO_1, benchIO "concatFor/drain4" $ drainConcatFor4 size4)
    , (SpaceO_1, benchIO "concatFor/drain5" $ drainConcatFor5 size5)
    , (SpaceO_1, benchIO "concatFor/drainM2" $ drainConcatForM sqrtVal)
    , (SpaceO_1, benchIO "concatFor/drainM3" $ drainConcatFor3M cubertVal)
    , (SpaceO_1, benchIO "concatFor/filterAllIn2" $ filterAllInConcatFor sqrtVal)
    , (SpaceO_1, benchIO "concatFor/filterAllOut2" $ filterAllOutConcatFor sqrtVal)
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double) -- double nested loop
    cubertVal = round (fromIntegral size**(1/3::Double)) -- triple nested loop
    size4 = round (fromIntegral size**(1/4::Double)) -- 4 times nested loop
    size5 = round (fromIntegral size**(1/5::Double)) -- 5 times nested loop
