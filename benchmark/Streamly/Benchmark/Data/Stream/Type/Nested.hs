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
import Stream.Type.Basic (benchIO, withRandomIntIO, withStream)
import Streamly.Benchmark.Common
import Prelude hiding (concatMap, mapM, zipWith)

mkCross :: Stream m a -> Stream.Nested m a
mkCross = Stream.Nested

unCross :: Stream.Nested m a -> Stream m a
unCross = Stream.unNested

-------------------------------------------------------------------------------
-- Applicative
-------------------------------------------------------------------------------

{-# INLINE toNullApPure #-}
toNullApPure :: MonadAsync m => Int -> Int -> m ()
toNullApPure linearCount start = drain $ unCross $
    (+) <$> mkCross (sourceUnfoldr nestedCount2 start)
        <*> mkCross (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullMPure #-}
toNullMPure :: MonadAsync m => Int -> Int -> m ()
toNullMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    return $ x + y

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

{-# INLINE toNullM3Pure #-}
toNullM3Pure :: MonadAsync m => Int -> Int -> m ()
toNullM3Pure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount3 start)
    y <- mkCross (sourceUnfoldr nestedCount3 start)
    z <- mkCross (sourceUnfoldr nestedCount3 start)
    return $ x + y + z

    where

    nestedCount3 = round (fromIntegral linearCount**(1/3::Double))

{-# INLINE filterAllOutMPure #-}
filterAllOutMPure :: MonadAsync m => Int -> Int -> m ()
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
filterAllInMPure :: MonadAsync m => Int -> Int -> m ()
filterAllInMPure linearCount start = drain $ unCross $ do
    x <- mkCross (sourceUnfoldr nestedCount2 start)
    y <- mkCross (sourceUnfoldr nestedCount2 start)
    let s = x + y
    if s > 0
    then return s
    else mkCross Stream.nil

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

cross2 :: Int -> IO ()
cross2 linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossWith (+)
        (sourceUnfoldr nestedCount2 start)
        (sourceUnfoldr nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

crossApply :: Int -> IO ()
crossApply linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApply
        ((+) <$> sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

crossApplyFst :: Int -> IO ()
crossApplyFst linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApplyFst
        (sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

crossApplySnd :: Int -> IO ()
crossApplySnd linearCount = withRandomIntIO $ \start -> drain $
    Stream.crossApplySnd
        (sourceUnfoldrM nestedCount2 start)
        (sourceUnfoldrM nestedCount2 start)

    where

    nestedCount2 = round (fromIntegral linearCount**(1/2::Double))

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

drainConcatFor1 :: Int -> IO ()
drainConcatFor1 count = withStream count $ \s ->
    drain $ Stream.concatFor s $ \x ->
        Stream.fromPure $ x + 1

drainConcatFor :: Int -> IO ()
drainConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.fromPure $ x + y

drainConcatForM :: Int -> IO ()
drainConcatForM count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.fromPure $ x + y

drainConcatFor3 :: Int -> IO ()
drainConcatFor3 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.fromPure $ x + y + z

drainConcatFor4 :: Int -> IO ()
drainConcatFor4 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.fromPure $ x + y + z + w

drainConcatFor5 :: Int -> IO ()
drainConcatFor5 count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                Stream.concatFor s $ \z ->
                    Stream.concatFor s $ \w ->
                        Stream.concatFor s $ \u ->
                            Stream.fromPure $ x + y + z + w + u

drainConcatFor3M :: Int -> IO ()
drainConcatFor3M count = withStream count $ \s ->
    drain $ do
        Stream.concatForM s $ \x ->
            pure $ Stream.concatForM s $ \y ->
                pure $ Stream.concatForM s $ \z ->
                    pure $ Stream.fromPure $ x + y + z

filterAllInConcatFor :: Int -> IO ()
filterAllInConcatFor count = withStream count $ \s ->
    drain $ do
        Stream.concatFor s $ \x ->
            Stream.concatFor s $ \y ->
                let s1 = x + y
                 in if s1 > 0
                    then Stream.fromPure s1
                    else Stream.nil

filterAllOutConcatFor :: Int -> IO ()
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
    [ (SpaceO_1, benchIO "(*>)" $ withRandomIntIO (apDiscardFst size))
    , (SpaceO_1, benchIO "(<*)" $ withRandomIntIO (apDiscardSnd size))
    , (SpaceO_1, benchIO "(<*>)" $ withRandomIntIO (toNullAp size))
    , (SpaceO_1, benchIO "liftA2" $ withRandomIntIO (apLiftA2 size))
    , (SpaceO_1, benchIO "crossApply" $ crossApply size)
    , (SpaceO_1, benchIO "crossApplyFst" $ crossApplyFst size)
    , (SpaceO_1, benchIO "crossApplySnd" $ crossApplySnd size)
    , (SpaceO_1, benchIO "pureDrain2" $ withRandomIntIO (toNullApPure size))
    , (SpaceO_1, benchIO "pureCross2" $ cross2 size)

    -- Monad
    , (SpaceO_1, benchIO "then2M" $ withRandomIntIO (monadThen size))
    , (SpaceO_1, benchIO "drain2M" $ withRandomIntIO (toNullM size))
    , (SpaceO_1, benchIO "drain3M" $ withRandomIntIO (toNullM3 size))
    , (SpaceO_1, benchIO "filterAllOut2M" $ withRandomIntIO (filterAllOutM size))
    , (SpaceO_1, benchIO "filterAllIn2M" $ withRandomIntIO (filterAllInM size))
    , (SpaceO_1, benchIO "filterSome2M" $ withRandomIntIO (filterSome size))
    , (SpaceO_1, benchIO "breakAfterSome2M" $ withRandomIntIO (breakAfterSome size))
    , (SpaceO_1, benchIO "pureDrain2M" $ withRandomIntIO (toNullMPure size))
    , (SpaceO_1, benchIO "pureDrain3M" $ withRandomIntIO (toNullM3Pure size))
    , (SpaceO_1, benchIO "pureFilterAllIn2M" $ withRandomIntIO (filterAllInMPure size))
    , (SpaceO_1, benchIO "pureFilterAllOut2M" $ withRandomIntIO (filterAllOutMPure size))
    , (SpaceO_n, benchIO "toList2M" $ withRandomIntIO (toListM size))
    , (SpaceO_n, benchIO "toListSome2M" $ withRandomIntIO (toListSome size))

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
