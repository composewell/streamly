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

{-# ANN ap_ApplicativeInstance_Pure_x2 (PermitPatternMatches [''Int]) #-}
{-# ANN ap_ApplicativeInstance_Pure_x2 (PermitConstructions [''Int]) #-}
{-# ANN ap_ApplicativeInstance_Pure_x2 (PermitTypeClasses []) #-}
{-# NOINLINE ap_ApplicativeInstance_Pure_x2 #-}
ap_ApplicativeInstance_Pure_x2 :: Int -> Int -> IO ()
ap_ApplicativeInstance_Pure_x2 linearCount start = drain $ unCross $
    (+) <$> mkCross (sourceUnfoldr nestedCount2 start)
        <*> mkCross (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# ANN bind_MonadInstance_Pure_x2 (PermitPatternMatches
    [''Int,''Either,''Bool,''(,),''Stream,''Stream.Step]) #-}
{-# ANN bind_MonadInstance_Pure_x2 (PermitConstructions
    [''SVar.State,''Maybe,''Bool,''Stream.Step,''Either,''(,),''Int
    ,''Stream]) #-}
{-# ANN bind_MonadInstance_Pure_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_Pure_x2 #-}
bind_MonadInstance_Pure_x2 :: Int -> Int -> IO ()
bind_MonadInstance_Pure_x2 linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# ANN bind_MonadInstance_Pure_x3 (PermitPatternMatches
    [''Int,''Either,''Bool,''(,),''Stream,''Stream.Step]) #-}
{-# ANN bind_MonadInstance_Pure_x3 (PermitConstructions
    [''SVar.State,''Maybe,''Bool,''Either,''Stream.Step,''(,),''Int
    ,''Stream]) #-}
{-# ANN bind_MonadInstance_Pure_x3 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_Pure_x3 #-}
bind_MonadInstance_Pure_x3 :: Int -> Int -> IO ()
bind_MonadInstance_Pure_x3 linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount3 start)
    y <- mkCross (sourceUnfoldr nestedCount3 start)
    z <- mkCross (sourceUnfoldr nestedCount3 start)
    return $ x + y + z

    where

    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# ANN bind_MonadInstance_FilterAllOut_Pure_x2 (PermitPatternMatches
    [''Int,''Either,''Bool,''(,),''Stream,''Stream.Step]) #-}
{-# ANN bind_MonadInstance_FilterAllOut_Pure_x2 (PermitConstructions
    [''SVar.State,''Maybe,''Bool,''Stream.Step,''Stream,''(),''Either
    ,''(,),''Int]) #-}
{-# ANN bind_MonadInstance_FilterAllOut_Pure_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterAllOut_Pure_x2 #-}
bind_MonadInstance_FilterAllOut_Pure_x2 :: Int -> Int -> IO ()
bind_MonadInstance_FilterAllOut_Pure_x2 linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    let s = x + y
    if s < 0
    then return s
    else mkCross Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# ANN bind_MonadInstance_FilterAllIn_Pure_x2 (PermitPatternMatches
    [''Int,''Either,''Bool,''(,),''Stream,''Stream.Step]) #-}
{-# ANN bind_MonadInstance_FilterAllIn_Pure_x2 (PermitConstructions
    [''SVar.State,''Maybe,''Bool,''Stream.Step,''Stream,''(),''Either
    ,''(,),''Int]) #-}
{-# ANN bind_MonadInstance_FilterAllIn_Pure_x2 (PermitTypeClasses []) #-}
{-# NOINLINE bind_MonadInstance_FilterAllIn_Pure_x2 #-}
bind_MonadInstance_FilterAllIn_Pure_x2 :: Int -> Int -> IO ()
bind_MonadInstance_FilterAllIn_Pure_x2 linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    let s = x + y
    if s > 0
    then return s
    else mkCross Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# ANN crossWith (PermitPatternMatches [''Int]) #-}
{-# ANN crossWith (PermitConstructions []) #-}
{-# ANN crossWith (PermitTypeClasses []) #-}
{-# NOINLINE crossWith #-}
crossWith :: Int -> Int -> IO ()
crossWith linearCount start = drain $
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

{-# ANN concatFor_x1 (PermitPatternMatches [''Bool,''Int,''Stream.Step]) #-}
{-# ANN concatFor_x1 (PermitConstructions
    [''Int,''Stream.Step,''SVar.State,''Maybe,''Bool]) #-}
{-# ANN concatFor_x1 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_x1 #-}
concatFor_x1 :: Int -> Int -> IO ()
concatFor_x1 count = withStream count $ \s ->
    drain $ Stream.concatFor s $ \x ->
        Stream.fromPure $ x + 1

{-# ANN concatFor_x2 (PermitPatternMatches
    [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatFor_x2 (PermitConstructions
    [''Int,''Stream.Step,''Either,''SVar.State,''Maybe,''(,),''Stream
    ,''Bool]) #-}
{-# ANN concatFor_x2 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_x2 #-}
concatFor_x2 :: Int -> Int -> IO ()
concatFor_x2 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.fromPure $ x + y

{-# ANN concatForM_x2 (PermitPatternMatches
    [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatForM_x2 (PermitConstructions
    [''Int,''Stream.Step,''Either,''SVar.State,''Maybe,''(,),''Stream
    ,''Bool]) #-}
{-# ANN concatForM_x2 (PermitTypeClasses []) #-}
{-# NOINLINE concatForM_x2 #-}
concatForM_x2 :: Int -> Int -> IO ()
concatForM_x2 count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.fromPure $ x + y

{-# ANN concatFor_x3 (PermitPatternMatches
    [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatFor_x3 (PermitConstructions
    [''Int,''Stream.Step,''Either,''Stream,''SVar.State,''Maybe,''(,)
    ,''Bool]) #-}
{-# ANN concatFor_x3 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_x3 #-}
concatFor_x3 :: Int -> Int -> IO ()
concatFor_x3 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.fromPure $ x + y + z

{-# ANN concatFor_x4 (PermitPatternMatches
    [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatFor_x4 (PermitConstructions
    [''Int,''Stream.Step,''Either,''Stream,''SVar.State,''Maybe,''(,)
    ,''Bool]) #-}
{-# ANN concatFor_x4 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_x4 #-}
concatFor_x4 :: Int -> Int -> IO ()
concatFor_x4 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.fromPure $ x + y + z + w

{-# ANN concatFor_x5 (PermitPatternMatches
    [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatFor_x5 (PermitConstructions
    [''Int,''Stream.Step,''Either,''Stream,''SVar.State,''Maybe,''(,)
    ,''Bool]) #-}
{-# ANN concatFor_x5 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_x5 #-}
concatFor_x5 :: Int -> Int -> IO ()
concatFor_x5 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.concatFor s $ \u ->
                            Stream.fromPure $ x + y + z + w + u

{-# ANN concatForM_x3 (PermitPatternMatches
    [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatForM_x3 (PermitConstructions
    [''Int,''Stream.Step,''Either,''Stream,''SVar.State,''Maybe,''(,)
    ,''Bool]) #-}
{-# ANN concatForM_x3 (PermitTypeClasses []) #-}
{-# NOINLINE concatForM_x3 #-}
concatForM_x3 :: Int -> Int -> IO ()
concatForM_x3 count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.concatForM s $ \z ->
                    pure $ Stream.fromPure $ x + y + z

{-# ANN concatFor_FilterAllIn_x2 (PermitPatternMatches
    [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatFor_FilterAllIn_x2 (PermitConstructions
    [''Stream,''Int,''Stream.Step,''Either,''SVar.State,''Maybe,''(,),''()
    ,''Bool]) #-}
{-# ANN concatFor_FilterAllIn_x2 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_FilterAllIn_x2 #-}
concatFor_FilterAllIn_x2 :: Int -> Int -> IO ()
concatFor_FilterAllIn_x2 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                let s1 = x + y
                 in if s1 > 0
                    then Stream.fromPure s1
                    else Stream.nil

{-# ANN concatFor_FilterAllOut_x2 (PermitPatternMatches
    [''Either,''(,),''Bool,''Int,''Stream.Step,''Stream]) #-}
{-# ANN concatFor_FilterAllOut_x2 (PermitConstructions
    [''Stream,''Int,''Stream.Step,''Either,''SVar.State,''Maybe,''(,),''()
    ,''Bool]) #-}
{-# ANN concatFor_FilterAllOut_x2 (PermitTypeClasses []) #-}
{-# NOINLINE concatFor_FilterAllOut_x2 #-}
concatFor_FilterAllOut_x2 :: Int -> Int -> IO ()
concatFor_FilterAllOut_x2 count = withStream count $ \s ->
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
-- Benchmark naming: name each benchmark (and its IO action) after the exported
-- function it benchmarks, using combinator_dimension1_dimension2..., where the
-- dimensions are optional variants/type specializations (used esp. when more
-- than one specialization is benchmarked). Keep extra info in parenthetical
-- notes in the description; these also disambiguate benchmarks that reuse a
-- single IO action with different arguments. If the name has a trailing
-- underscore, add one more underscore.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    -- Applicative
    [ (SpaceO_1, benchIO "discardFst_ApplicativeInstance_x2 (*>)"
          $ discardFst_ApplicativeInstance_x2 size)
    , (SpaceO_1, benchIO "discardSnd_ApplicativeInstance_x2 (<*)"
          $ discardSnd_ApplicativeInstance_x2 size)
    , (SpaceO_1, benchIO "ap_ApplicativeInstance_x2 (<*>)"
          $ ap_ApplicativeInstance_x2 size)
    , (SpaceO_1, benchIO "liftA2_ApplicativeInstance_x2 (liftA2)"
          $ liftA2_ApplicativeInstance_x2 size)
    , (SpaceO_1, benchIO "crossApply" $ crossApply size)
    , (SpaceO_1, benchIO "crossApplyFst" $ crossApplyFst size)
    , (SpaceO_1, benchIO "crossApplySnd" $ crossApplySnd size)
    , (SpaceO_1, benchIO "ap_ApplicativeInstance_Pure_x2 (<*>)"
          $ ap_ApplicativeInstance_Pure_x2 size)
    , (SpaceO_1, benchIO "crossWith" $ crossWith size)

    -- Monad
    , (SpaceO_1, benchIO "then_MonadInstance_x2 (>>)"
          $ then_MonadInstance_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_x2 (>>=)"
          $ bind_MonadInstance_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_x3 (>>=)"
          $ bind_MonadInstance_x3 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllOut_x2 (>>=)"
          $ bind_MonadInstance_FilterAllOut_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllIn_x2 (>>=)"
          $ bind_MonadInstance_FilterAllIn_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_FilterSome_x2 (>>=)"
          $ bind_MonadInstance_FilterSome_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_BreakAfterSome_x2 (>>=)"
          $ bind_MonadInstance_BreakAfterSome_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_Pure_x2 (>>=)"
          $ bind_MonadInstance_Pure_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_Pure_x3 (>>=)"
          $ bind_MonadInstance_Pure_x3 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllIn_Pure_x2 (>>=)"
          $ bind_MonadInstance_FilterAllIn_Pure_x2 size)
    , (SpaceO_1, benchIO "bind_MonadInstance_FilterAllOut_Pure_x2 (>>=)"
          $ bind_MonadInstance_FilterAllOut_Pure_x2 size)
    , (SpaceO_n, benchIO "bind_MonadInstance_ToList_x2 (>>=)"
          $ bind_MonadInstance_ToList_x2 size)
    , (SpaceO_n, benchIO "bind_MonadInstance_ToListSome_x2 (>>=)"
          $ bind_MonadInstance_ToListSome_x2 size)

    -- concatFor (bind)
    , (SpaceO_1, benchIO "concatFor_x1" $ concatFor_x1 size)
    , (SpaceO_1, benchIO "concatFor_x2" $ concatFor_x2 sqrtVal)
    , (SpaceO_1, benchIO "concatFor_x3" $ concatFor_x3 cubertVal)
    , (SpaceO_1, benchIO "concatFor_x4" $ concatFor_x4 size4)
    , (SpaceO_1, benchIO "concatFor_x5" $ concatFor_x5 size5)
    , (SpaceO_1, benchIO "concatForM_x2" $ concatForM_x2 sqrtVal)
    , (SpaceO_1, benchIO "concatForM_x3" $ concatForM_x3 cubertVal)
    , (SpaceO_1, benchIO "concatFor_FilterAllIn_x2" $
          concatFor_FilterAllIn_x2 sqrtVal)
    , (SpaceO_1, benchIO "concatFor_FilterAllOut_x2" $
          concatFor_FilterAllOut_x2 sqrtVal)
    ]

    where

    sqrtVal = round $ sqrt (fromIntegral size :: Double) -- double nested loop
    cubertVal = round (fromIntegral size**(1/3::Double)) -- triple nested loop
    size4 = round (fromIntegral size**(1/4::Double)) -- 4 times nested loop
    size5 = round (fromIntegral size**(1/5::Double)) -- 5 times nested loop
