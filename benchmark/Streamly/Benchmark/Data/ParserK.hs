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
import Data.Foldable (asum)
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
#ifdef BENCH_CHUNKED
import qualified Streamly.Internal.Data.Array as Array
#elif defined(BENCH_CHUNKED_GENERIC)
import qualified Streamly.Internal.Data.Array.Generic as GenArr
#endif

import Test.Tasty.Bench
import Streamly.Benchmark.Common

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
benchIO :: NFData b => String -> IO b -> Benchmark
benchIO name = bench name . nfIO

{-# INLINE withStreamK #-}
withStreamK :: Int -> (StreamK IO PARSE_ELEM -> IO b) -> IO b
withStreamK value f =
    randomRIO (1,1) >>=
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

{-# INLINE drain #-}
drain :: Int -> IO ()
drain value = withStreamK value $ Stream.fold Fold.drain . StreamK.toStream

{-# INLINE one #-}
one :: Int -> IO (Either ParseError (Maybe Int))
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

{-# INLINE takeWhile #-}
takeWhile :: CONSTRAINT_IO => (a -> Bool) -> PR.ParserK INPUT m ()
takeWhile p = FROM_PARSER $ PRD.takeWhile p FL.drain

{-# INLINE takeWhileK #-}
takeWhileK :: Int -> IO (Either ParseError ())
takeWhileK value = withStreamK value $ PARSE_OP (takeWhile (<= value))

{-# INLINE splitAp2 #-}
splitAp2 :: Int -> IO (Either ParseError ((), ()))
splitAp2 value =
    withStreamK value $ PARSE_OP
        ((,)
            <$> takeWhile (<= (value `div` 2))
            <*> takeWhile (<= value)
        )

{-# INLINE splitAp8 #-}
splitAp8 :: Int -> IO (Either ParseError ())
splitAp8 value =
    withStreamK value $ PARSE_OP
        (      (\() () () () () () () () -> ())
            <$> takeWhile (<= ( value      `div` 8))
            <*> takeWhile (<= ((value * 2) `div` 8))
            <*> takeWhile (<= ((value * 3) `div` 8))
            <*> takeWhile (<= ((value * 4) `div` 8))
            <*> takeWhile (<= ((value * 5) `div` 8))
            <*> takeWhile (<= ((value * 6) `div` 8))
            <*> takeWhile (<= ((value * 7) `div` 8))
            <*> takeWhile (<= value)
        )

{-# INLINE sequenceA #-}
sequenceA :: Int -> IO Int
sequenceA value = withStreamK value $ \xs -> do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequenceA list) xs
    return $ Prelude.length x

{-# INLINE sequenceA_ #-}
sequenceA_ :: Int -> IO (Either ParseError ())
sequenceA_ value = withStreamK value $ \xs -> do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    PARSE_OP (F.sequenceA_ list) xs

{-# INLINE sequence #-}
sequence :: Int -> IO Int
sequence value = withStreamK value $ \xs -> do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequence list) xs
    return $ Prelude.length x

{-# INLINE sequence_ #-}
sequence_ :: Int -> IO (Either ParseError ())
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

{-# INLINE alt2 #-}
alt2 :: Int -> IO (Either ParseError ())
alt2 value =
    withStreamK value $ PARSE_OP
        (   takeWhileFail (<= (value `div` 2)) Fold.drain
        <|> takeWhile (<= value)
        )

{-# INLINE alt8 #-}
alt8 :: Int -> IO (Either ParseError ())
alt8 value =
    withStreamK value $ PARSE_OP
        (   takeWhileFail (<= ( value      `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 2) `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 3) `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 4) `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 5) `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 6) `div` 8)) Fold.drain
        <|> takeWhileFail (<= ((value * 7) `div` 8)) Fold.drain
        <|> takeWhile (<= value)
        )

{-# INLINE alt16 #-}
alt16 :: Int -> IO (Either ParseError ())
alt16 value =
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
        <|> takeWhile (<= value)
        )

{-# INLINE manyAlt #-}
manyAlt :: Int -> IO Int
manyAlt value = withStreamK value $ \xs -> do
    x <- PARSE_OP (AP.many (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE someAlt #-}
someAlt :: Int -> IO Int
someAlt value = withStreamK value $ \xs -> do
    x <- PARSE_OP (AP.some (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE choice #-}
choice :: Int -> IO (Either ParseError Int)
choice value =
    withStreamK value $ PARSE_OP (asum (replicate value (satisfy (< 0)))
        AP.<|> satisfy (> 0))

{-# INLINE monad2 #-}
monad2 :: Int -> IO (Either ParseError ())
monad2 value =
    withStreamK value $ PARSE_OP $ do
        takeWhile (<= (value `div` 2))
        takeWhile (<= value)

{-# INLINE monad4 #-}
monad4 :: Int -> IO (Either ParseError ())
monad4 value =
    withStreamK value $ PARSE_OP $ do
        takeWhile (<= ( value      `div` 4))
        takeWhile (<= ((value * 2) `div` 4))
        takeWhile (<= ((value * 3) `div` 4))
        takeWhile (<= value)

{-# INLINE monad8 #-}
monad8 :: Int -> IO (Either ParseError ())
monad8 value =
    withStreamK value $ PARSE_OP $ do
        takeWhile (<= ( value      `div` 8))
        takeWhile (<= ((value * 2) `div` 8))
        takeWhile (<= ((value * 3) `div` 8))
        takeWhile (<= ((value * 4) `div` 8))
        takeWhile (<= ((value * 5) `div` 8))
        takeWhile (<= ((value * 6) `div` 8))
        takeWhile (<= ((value * 7) `div` 8))
        takeWhile (<= value)

{-# INLINE monad16 #-}
monad16 :: Int -> IO (Either ParseError ())
monad16 value =
    withStreamK value $ PARSE_OP $ do
        takeWhile (<= ( value      `div` 16))
        takeWhile (<= ((value * 2) `div` 16))
        takeWhile (<= ((value * 3) `div` 16))
        takeWhile (<= ((value * 4) `div` 16))
        takeWhile (<= ((value * 5) `div` 16))
        takeWhile (<= ((value * 6) `div` 16))
        takeWhile (<= ((value * 7) `div` 16))
        takeWhile (<= ((value * 8) `div` 16))
        takeWhile (<= ((value * 9) `div` 16))
        takeWhile (<= ((value * 10) `div` 16))
        takeWhile (<= ((value * 11) `div` 16))
        takeWhile (<= ((value * 12) `div` 16))
        takeWhile (<= ((value * 13) `div` 16))
        takeWhile (<= ((value * 14) `div` 16))
        takeWhile (<= ((value * 15) `div` 16))
        takeWhile (<= value)

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
    , (SpaceO_1, benchIO "takeWhile" $ takeWhileK value)
    , (SpaceO_1, benchIO "splitAp2" $ splitAp2 value)
    , (SpaceO_1, benchIO "splitAp8" $ splitAp8 value)
    , (SpaceO_1, benchIO "alt2" $ alt2 value)
    , (SpaceO_1, benchIO "monad2" $ monad2 value)
    , (SpaceO_1, benchIO "monad4" $ monad4 value)
    ]

{-# INLINE sepBy1 #-}
sepBy1 :: Int -> IO Int
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
    , (HeapO_n, benchIO "manyAlt" $ manyAlt value)
    , (HeapO_n, benchIO "sepBy1" $ sepBy1 value)
    , (HeapO_n, benchIO "someAlt" $ someAlt value)
    , (HeapO_n, benchIO "choice" $ choice value)

    -- XXX these take too much memory with --long, need to investigate
    , (HeapO_n, benchIO "alt8" $ alt8 value)
    , (HeapO_n, benchIO "alt16" $ alt16 value)
    , (HeapO_n, benchIO "monad8" $ monad8 value)
    , (HeapO_n, benchIO "monad16" $ monad16 value)
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
