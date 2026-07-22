-- |
-- Module      : Streamly.Benchmark.Data.ParserK
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

-- BENCH_CHUNKED             -> parse from Array stream
-- BENCH_CHUNKED_GENERIC     -> parse from Generic Array stream
-- BENCH_SINGULAR            -> parse from single element stream

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  (
    main
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO)
#ifdef BENCH_CHUNKED
import Streamly.Data.Array (Array, Unbox)
#endif
#ifdef BENCH_CHUNKED_GENERIC
import Streamly.Data.Array.Generic (Array)
#endif
import Streamly.Internal.Data.Fold (Fold(..))
import Streamly.Data.StreamK (StreamK)
import Streamly.Internal.Data.Parser
    (ParseError(..), Parser(..), Initial(..), Step(..), Final(..))
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Prelude hiding
    (any, all, take, sequence, sequence_, sequenceA, takeWhile)

import qualified Control.Applicative as AP
import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Data.Parser as PRD
import qualified Streamly.Internal.Data.ParserK as PR
import qualified Streamly.Internal.Data.StreamK as StreamK
import qualified Streamly.Internal.Data.SVar.Type as SVar
import GHC.Classes (IP)
import GHC.Stack (SrcLoc, CallStack)
#ifdef BENCH_CHUNKED
import qualified Streamly.Internal.Data.Array as Array
#elif defined(BENCH_CHUNKED_GENERIC)
import qualified Streamly.Internal.Data.Array.Generic as GenArr
#endif

import Test.Tasty.Bench
import Streamly.Benchmark.Common
import Fusion.Plugin.Types

-------------------------------------------------------------------------------
-- CPP Helpers
-------------------------------------------------------------------------------

#ifdef BENCH_CHUNKED

#define PARSE_OP Array.parse
#define FROM_PARSER Array.toParserK
#define INPUT (Array a)
#define PARSE_ELEM (Array Int)
#define CONSTRAINT_IO (MonadIO m, Unbox a)
#define CONSTRAINT (Monad m, Unbox a)
#define MODULE_NAME "Data.ParserK.Chunked"

#endif

#ifdef BENCH_CHUNKED_GENERIC

#define PARSE_OP GenArr.parse
#define FROM_PARSER GenArr.toParserK
#define INPUT (Array a)
#define PARSE_ELEM (Array Int)
#define CONSTRAINT_IO (MonadIO m)
#define CONSTRAINT (Monad m)
#define MODULE_NAME "Data.ParserK.Chunked.Generic"

#endif

#ifdef BENCH_SINGULAR

#define PARSE_OP StreamK.parse
#define FROM_PARSER PR.toParserK
#define INPUT a
#define PARSE_ELEM Int
#define CONSTRAINT_IO (MonadIO m)
#define CONSTRAINT (Monad m)
#define MODULE_NAME "Data.ParserK"

#endif

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: Monad m => Int -> Int -> Stream m Int
sourceUnfoldrM value n = Stream.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1, 1 :: Int) >>= f

{-# INLINE withStreamK #-}
withStreamK :: Int -> (StreamK IO PARSE_ELEM -> IO b) -> Int -> IO b
withStreamK value f =
    f . StreamK.fromStream
#ifdef BENCH_CHUNKED
          . Array.chunksOf 4000
#endif
#ifdef BENCH_CHUNKED_GENERIC
          . GenArr.chunksOf 4000
#endif
          . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# ANN drain (PermitPatternMatches []) #-}
{-# ANN drain (PermitConstructions [''()]) #-}
{-# ANN drain (PermitTypeClasses []) #-}
{-# NOINLINE drain #-}
drain :: Int -> Int -> IO ()
drain value = withStreamK value $ Stream.fold Fold.drain . StreamK.toStream

{-# ANN one (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''PR.ParseResult, ''Int
    , ''PR.Input, ''Maybe]) #-}
{-# ANN one (PermitConstructions
    [''SVar.State, ''Maybe, ''Bool, ''[], ''(,), ''Either, ''Int, ''SrcLoc
    , ''CallStack, ''PR.Step, ''PR.Input, ''PR.ParseResult]) #-}
{-# ANN one (PermitTypeClasses [''IP]) #-}
{-# NOINLINE one #-}
one :: Int -> Int -> IO (Either ParseError (Maybe Int))
one value = withStreamK value $ PARSE_OP p

    where

    p = do
        m <- FROM_PARSER (PRD.fromFold FL.one)
        case m of
          Just i -> if i >= value then pure m else p
          Nothing -> pure Nothing

{-# INLINE satisfy #-}
satisfy :: CONSTRAINT_IO => (a -> Bool) -> PR.ParserK INPUT m a
satisfy = FROM_PARSER . PRD.satisfy

{-# INLINE takeWhileParser #-}
takeWhileParser :: CONSTRAINT_IO => (a -> Bool) -> PR.ParserK INPUT m ()
takeWhileParser p = FROM_PARSER $ PRD.takeWhile p FL.drain

{-# ANN takeWhile (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''(), ''PR.Input]) #-}
{-# ANN takeWhile (PermitConstructions
    [''Int, ''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''SrcLoc
    , ''CallStack, ''Bool, ''PR.Step, ''(), ''PR.Input]) #-}
{-# ANN takeWhile (PermitTypeClasses [''IP]) #-}
{-# NOINLINE takeWhile #-}
takeWhile :: Int -> Int -> IO (Either ParseError ())
takeWhile value = withStreamK value $ PARSE_OP (takeWhileParser (<= value))

{-# ANN ap_ApplicativeInstance_x2 (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''(), ''PR.Input]) #-}
{-# ANN ap_ApplicativeInstance_x2 (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''(), ''PR.Step, ''PR.Input]) #-}
{-# ANN ap_ApplicativeInstance_x2 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE ap_ApplicativeInstance_x2 #-}
ap_ApplicativeInstance_x2 :: Int -> Int -> IO (Either ParseError ((), ()))
ap_ApplicativeInstance_x2 value =
    withStreamK value $ PARSE_OP
        ((,)
            <$> takeWhileParser (<= (value `div` 2))
            <*> takeWhileParser (<= value)
        )

{-# ANN ap_ApplicativeInstance_x8 (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''(), ''PR.Input]) #-}
{-# ANN ap_ApplicativeInstance_x8 (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''(), ''PR.Step, ''PR.Input]) #-}
{-# ANN ap_ApplicativeInstance_x8 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE ap_ApplicativeInstance_x8 #-}
ap_ApplicativeInstance_x8 :: Int -> Int -> IO (Either ParseError ())
ap_ApplicativeInstance_x8 value =
    withStreamK value $ PARSE_OP
        (      (\() () () () () () () () -> ())
            <$> takeWhileParser (<= ( value      `div` 8))
            <*> takeWhileParser (<= ((value * 2) `div` 8))
            <*> takeWhileParser (<= ((value * 3) `div` 8))
            <*> takeWhileParser (<= ((value * 4) `div` 8))
            <*> takeWhileParser (<= ((value * 5) `div` 8))
            <*> takeWhileParser (<= ((value * 6) `div` 8))
            <*> takeWhileParser (<= ((value * 7) `div` 8))
            <*> takeWhileParser (<= value)
        )

{-# ANN sequenceA (PermitPatternMatches
    [''Int, ''PR.Input, ''(), ''PR.ParseResult, ''[], ''PR.Step, ''SVar.State
    , ''(,), ''Either]) #-}
{-# ANN sequenceA (PermitConstructions
    [''PR.ParseResult, ''PR.Input, ''Int, ''PR.Step, ''(), ''[], ''(,)
    , ''Either, ''SVar.State, ''Maybe, ''SrcLoc, ''CallStack, ''Bool]) #-}
{-# ANN sequenceA (PermitTypeClasses [''IP]) #-}
{-# NOINLINE sequenceA #-}
sequenceA :: Int -> Int -> IO Int
sequenceA value = withStreamK value $ \xs -> do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequenceA list) xs
    return $ Prelude.length x

{-# ANN sequenceA_ (PermitPatternMatches
    [''Int, ''PR.Input, ''(), ''PR.ParseResult, ''[], ''PR.Step, ''SVar.State
    , ''(,)]) #-}
{-# ANN sequenceA_ (PermitConstructions
    [''PR.ParseResult, ''PR.Input, ''Int, ''PR.Step, ''(), ''[], ''(,)
    , ''Either, ''SVar.State, ''Maybe, ''SrcLoc, ''CallStack, ''Bool]) #-}
{-# ANN sequenceA_ (PermitTypeClasses [''IP]) #-}
{-# NOINLINE sequenceA_ #-}
sequenceA_ :: Int -> Int -> IO (Either ParseError ())
sequenceA_ value = withStreamK value $ \xs -> do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    PARSE_OP (F.sequenceA_ list) xs

{-# ANN sequence (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''PR.Input, ''()
    , ''PR.ParseResult, ''Either]) #-}
{-# ANN sequence (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''PR.ParseResult, ''PR.Input, ''PR.Step
    , ''()]) #-}
{-# ANN sequence (PermitTypeClasses [''IP]) #-}
{-# NOINLINE sequence #-}
sequence :: Int -> Int -> IO Int
sequence value = withStreamK value $ \xs -> do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequence list) xs
    return $ Prelude.length x

{-# ANN sequence_ (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''PR.Input, ''()
    , ''PR.ParseResult]) #-}
{-# ANN sequence_ (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''PR.ParseResult, ''PR.Input, ''PR.Step
    , ''()]) #-}
{-# ANN sequence_ (PermitTypeClasses [''IP]) #-}
{-# NOINLINE sequence_ #-}
sequence_ :: Int -> Int -> IO (Either ParseError ())
sequence_ value =
    withStreamK value $
        let parser = satisfy (> 0)
            list = Prelude.replicate value parser
         in PARSE_OP (F.sequence_ list)

{-# INLINE takeWhileFailD #-}
takeWhileFailD :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
takeWhileFailD predicate (Fold fstep finitial _ ffinal) =
    Parser step initial extract

    where

    initial = do
        res <- finitial
        return $ case res of
            Fold.Partial s -> IPartial s
            Fold.Done b -> IDone b

    step s a =
        if predicate a
        then do
            fres <- fstep s a
            return
                $ case fres of
                      Fold.Partial s1 -> SContinue 1 s1
                      Fold.Done b -> SDone 1 b
        else return $ SError "fail"

    extract s = fmap (FDone 0) (ffinal s)

{-# INLINE takeWhileFail #-}
takeWhileFail :: CONSTRAINT =>
    (a -> Bool) -> Fold m a b -> PR.ParserK INPUT m b
takeWhileFail p f = FROM_PARSER (takeWhileFailD p f)

{-# ANN alt_AlternativeInstance_x2 (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''(), ''PR.Input]) #-}
{-# ANN alt_AlternativeInstance_x2 (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''PR.Step, ''(), ''PR.Input]) #-}
{-# ANN alt_AlternativeInstance_x2 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE alt_AlternativeInstance_x2 #-}
alt_AlternativeInstance_x2 :: Int -> Int -> IO (Either ParseError ())
alt_AlternativeInstance_x2 value =
    withStreamK value $ PARSE_OP
        (   takeWhileFail (<= (value `div` 2)) Fold.drain
        <|> takeWhileParser (<= value)
        )

{-# ANN alt_AlternativeInstance_x8 (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''(), ''PR.Input]) #-}
{-# ANN alt_AlternativeInstance_x8 (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''PR.Step, ''(), ''PR.Input]) #-}
{-# ANN alt_AlternativeInstance_x8 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE alt_AlternativeInstance_x8 #-}
alt_AlternativeInstance_x8 :: Int -> Int -> IO (Either ParseError ())
alt_AlternativeInstance_x8 value =
    withStreamK value $ PARSE_OP
        (   takeWhileFail (<= ( value      `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 2) `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 3) `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 4) `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 5) `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 6) `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 7) `div` 8)) Fold.drain
        <|> takeWhileParser (<= value)
        )

{-# ANN alt_AlternativeInstance_x16 (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''(), ''PR.Input]) #-}
{-# ANN alt_AlternativeInstance_x16 (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''PR.Step, ''(), ''PR.Input]) #-}
{-# ANN alt_AlternativeInstance_x16 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE alt_AlternativeInstance_x16 #-}
alt_AlternativeInstance_x16 :: Int -> Int -> IO (Either ParseError ())
alt_AlternativeInstance_x16 value =
    withStreamK value $ PARSE_OP
        (   takeWhileFail (<= ( value      `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 2) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 3) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 4) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 5) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 6) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 7) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 8) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 9) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 10) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 11) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 12) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 13) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 14) `div` 16)) Fold.drain
        <|> takeWhileFail (<= ((value * 15) `div` 16)) Fold.drain
        <|> takeWhileParser (<= value)
        )

{-# ANN many_AlternativeInstance (PermitPatternMatches
    [''PR.Input, ''Int, ''PR.ParseResult, ''(), ''[], ''PR.Step, ''SVar.State
    , ''(,), ''Either]) #-}
{-# ANN many_AlternativeInstance (PermitConstructions
    [''Int, ''PR.ParseResult, ''[], ''PR.Input, ''PR.Step, ''(), ''(,)
    , ''Either, ''SVar.State, ''Maybe, ''SrcLoc, ''CallStack, ''Bool]) #-}
{-# ANN many_AlternativeInstance (PermitTypeClasses [''IP]) #-}
{-# NOINLINE many_AlternativeInstance #-}
many_AlternativeInstance :: Int -> Int -> IO Int
many_AlternativeInstance value = withStreamK value $ \xs -> do
    x <- PARSE_OP (AP.many (satisfy (> 0))) xs
    return $ Prelude.length x

{-# ANN some_AlternativeInstance (PermitPatternMatches
    [''PR.Input, ''Int, ''PR.ParseResult, ''(), ''[], ''PR.Step, ''SVar.State
    , ''(,), ''Either]) #-}
{-# ANN some_AlternativeInstance (PermitConstructions
    [''Int, ''PR.ParseResult, ''[], ''PR.Input, ''PR.Step, ''(), ''(,)
    , ''Either, ''SVar.State, ''Maybe, ''SrcLoc, ''CallStack, ''Bool]) #-}
{-# ANN some_AlternativeInstance (PermitTypeClasses [''IP]) #-}
{-# NOINLINE some_AlternativeInstance #-}
some_AlternativeInstance :: Int -> Int -> IO Int
some_AlternativeInstance value = withStreamK value $ \xs -> do
    x <- PARSE_OP (AP.some (satisfy (> 0))) xs
    return $ Prelude.length x

{-# ANN asum (PermitPatternMatches
    [''Int, ''(), ''PR.Input, ''PR.ParseResult, ''[], ''PR.Step, ''SVar.State
    , ''(,)]) #-}
{-# ANN asum (PermitConstructions
    [''PR.Step, ''(), ''PR.ParseResult, ''PR.Input, ''Int, ''[], ''(,)
    , ''Either, ''SVar.State, ''Maybe, ''SrcLoc, ''CallStack, ''Bool]) #-}
{-# ANN asum (PermitTypeClasses [''IP]) #-}
{-# NOINLINE asum #-}
asum :: Int -> Int -> IO (Either ParseError Int)
asum value =
    withStreamK value
        $ PARSE_OP
            (F.asum (replicate value (satisfy (< 0)))
                AP.<|> satisfy (> 0))

{-# ANN then_MonadInstance_x2 (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''(), ''PR.Input]) #-}
{-# ANN then_MonadInstance_x2 (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''PR.Step, ''(), ''PR.Input]) #-}
{-# ANN then_MonadInstance_x2 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE then_MonadInstance_x2 #-}
then_MonadInstance_x2 :: Int -> Int -> IO (Either ParseError ())
then_MonadInstance_x2 value =
    withStreamK value $ PARSE_OP $ do
        takeWhileParser (<= (value `div` 2))
        takeWhileParser (<= value)

{-# ANN then_MonadInstance_x4 (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''(), ''PR.Input]) #-}
{-# ANN then_MonadInstance_x4 (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''PR.Step, ''(), ''PR.Input]) #-}
{-# ANN then_MonadInstance_x4 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE then_MonadInstance_x4 #-}
then_MonadInstance_x4 :: Int -> Int -> IO (Either ParseError ())
then_MonadInstance_x4 value =
    withStreamK value $ PARSE_OP $ do
        takeWhileParser (<= ( value      `div` 4))
        takeWhileParser (<= ((value * 2) `div` 4))
        takeWhileParser (<= ((value * 3) `div` 4))
        takeWhileParser (<= value)

{-# ANN then_MonadInstance_x8 (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''(), ''PR.Input]) #-}
{-# ANN then_MonadInstance_x8 (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''PR.Step, ''(), ''PR.Input]) #-}
{-# ANN then_MonadInstance_x8 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE then_MonadInstance_x8 #-}
then_MonadInstance_x8 :: Int -> Int -> IO (Either ParseError ())
then_MonadInstance_x8 value =
    withStreamK value $ PARSE_OP $ do
        takeWhileParser (<= ( value      `div` 8))
        takeWhileParser (<= ((value * 2) `div` 8))
        takeWhileParser (<= ((value * 3) `div` 8))
        takeWhileParser (<= ((value * 4) `div` 8))
        takeWhileParser (<= ((value * 5) `div` 8))
        takeWhileParser (<= ((value * 6) `div` 8))
        takeWhileParser (<= ((value * 7) `div` 8))
        takeWhileParser (<= value)

{-# ANN then_MonadInstance_x16 (PermitPatternMatches
    [''[], ''PR.Step, ''SVar.State, ''(,), ''Int, ''(), ''PR.Input]) #-}
{-# ANN then_MonadInstance_x16 (PermitConstructions
    [''[], ''(,), ''Either, ''SVar.State, ''Maybe, ''Int, ''SrcLoc
    , ''CallStack, ''Bool, ''PR.Step, ''(), ''PR.Input]) #-}
{-# ANN then_MonadInstance_x16 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE then_MonadInstance_x16 #-}
then_MonadInstance_x16 :: Int -> Int -> IO (Either ParseError ())
then_MonadInstance_x16 value =
    withStreamK value $ PARSE_OP $ do
        takeWhileParser (<= ( value      `div` 16))
        takeWhileParser (<= ((value * 2) `div` 16))
        takeWhileParser (<= ((value * 3) `div` 16))
        takeWhileParser (<= ((value * 4) `div` 16))
        takeWhileParser (<= ((value * 5) `div` 16))
        takeWhileParser (<= ((value * 6) `div` 16))
        takeWhileParser (<= ((value * 7) `div` 16))
        takeWhileParser (<= ((value * 8) `div` 16))
        takeWhileParser (<= ((value * 9) `div` 16))
        takeWhileParser (<= ((value * 10) `div` 16))
        takeWhileParser (<= ((value * 11) `div` 16))
        takeWhileParser (<= ((value * 12) `div` 16))
        takeWhileParser (<= ((value * 13) `div` 16))
        takeWhileParser (<= ((value * 14) `div` 16))
        takeWhileParser (<= ((value * 15) `div` 16))
        takeWhileParser (<= value)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = MODULE_NAME

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

o_1_space_serial :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_serial value =
    [ (SpaceO_1, benchIO "drain" $ drain value)
    , (SpaceO_1, benchIO "takeWhile" $ takeWhile value)
    , (SpaceO_1, benchIO "ap_ApplicativeInstance_x2 (<*>)"
          $ ap_ApplicativeInstance_x2 value)
    , (SpaceO_1, benchIO "ap_ApplicativeInstance_x8 (<*>)"
          $ ap_ApplicativeInstance_x8 value)
    , (SpaceO_1, benchIO "alt_AlternativeInstance_x2 (<|>)"
          $ alt_AlternativeInstance_x2 value)
    , (SpaceO_1, benchIO "then_MonadInstance_x2" $ then_MonadInstance_x2 value)
    , (SpaceO_1, benchIO "then_MonadInstance_x4" $ then_MonadInstance_x4 value)
    ]

{-# ANN sepBy1 (PermitPatternMatches
    [''PR.Input, ''Int, ''PR.ParseResult, ''(), ''[], ''PR.Step, ''SVar.State
    , ''(,), ''Either]) #-}
{-# ANN sepBy1 (PermitConstructions
    [''PR.ParseResult, ''[], ''PR.Input, ''PR.Step, ''Int, ''(), ''(,)
    , ''Either, ''SVar.State, ''Maybe, ''SrcLoc, ''CallStack, ''Bool]) #-}
{-# ANN sepBy1 (PermitTypeClasses [''IP]) #-}
{-# NOINLINE sepBy1 #-}
sepBy1 :: Int -> Int -> IO Int
sepBy1 value = withStreamK value $ \xs -> do
    x <- PARSE_OP (parser (satisfy odd) (satisfy even)) xs
    return $ Prelude.length x

    where

    parser p sep = do
        x <- p
        fmap (x :) $ AP.many (sep >> p)

-- O(n) heap beacuse of accumulation of the list in strict IO monad?
o_n_heap_serial :: Int -> [(SpaceComplexity, Benchmark)]
o_n_heap_serial value =
    [
    -- accumulates the results in a list
    -- XXX why should this take O(n) heap, it discards the results?
      (HeapO_n, benchIO "sequence_" $ sequence_ value)
    , (HeapO_n, benchIO "sequenceA_" $ sequenceA_ value)
    , (HeapO_n, benchIO "sequence" $ sequence value)
    , (HeapO_n, benchIO "sequenceA" $ sequenceA value)
    , (HeapO_n, benchIO "many_AlternativeInstance"
          $ many_AlternativeInstance value)
    , (HeapO_n, benchIO "sepBy1 (odd & even, hand written)" $ sepBy1 value)
    , (HeapO_n, benchIO "some_AlternativeInstance"
          $ some_AlternativeInstance value)
    , (HeapO_n, benchIO "asum" $ asum value)

    -- XXX these take too much memory with --long, need to investigate
    , (HeapO_n, benchIO "alt_AlternativeInstance_x8 (<|>)"
          $ alt_AlternativeInstance_x8 value)
    , (HeapO_n, benchIO "alt_AlternativeInstance_x16 (<|>)"
          $ alt_AlternativeInstance_x16 value)
    , (HeapO_n, benchIO "then_MonadInstance_x8" $ then_MonadInstance_x8 value)
    , (HeapO_n, benchIO "then_MonadInstance_x16"
          $ then_MonadInstance_x16 value)
    ]

-- O(n) heap beacuse of accumulation of the list in strict IO monad?
o_1_space_recursive :: Int -> [(SpaceComplexity, Benchmark)]
o_1_space_recursive value =
    [ (SpaceO_1, benchIO "one (recursive)" $ one value)
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        let allBenches = o_1_space_serial value
                      ++ o_n_heap_serial value
                      ++ o_1_space_recursive value
            get x = map snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_heap = get HeapO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        ]
