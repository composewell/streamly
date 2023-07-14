-- |
-- Module      : Streamly.Benchmark.Data.ParserK
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

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
import Streamly.Data.Array (Array, Unbox)
import Streamly.Internal.Data.Fold.Type (Fold(..))
import Streamly.Data.StreamK (StreamK)
import Streamly.Internal.Data.Parser
    (ParseError(..), Parser(..), Initial(..), Step(..))
import Streamly.Internal.Data.Stream.StreamD (Stream)
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
import qualified Streamly.Data.ParserK as PR
import qualified Streamly.Internal.Data.Stream.StreamK as StreamK

import Gauge
import Streamly.Benchmark.Common

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

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: NFData b
    => Int -> String -> (StreamK IO (Array Int) -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1)
        >>= f
            . StreamK.fromStream
            . Stream.chunksOf 4000
            . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

#define PARSE_OP StreamK.parseChunks

{-# INLINE one #-}
one :: MonadIO m =>
    Int -> StreamK m (Array Int) -> m (Either ParseError (Maybe Int))
one value = StreamK.parseChunks p

    where

    p = do
        m <- PR.fromFold FL.one
        case m of
          Just i -> if i >= value then pure m else p
          Nothing -> pure Nothing

{-# INLINE satisfy #-}
satisfy :: (MonadIO m, Unbox a) => (a -> Bool) -> PR.ChunkParserK a m a
satisfy = PR.fromParser . PRD.satisfy

{-# INLINE takeWhile #-}
takeWhile :: (MonadIO m, Unbox a) => (a -> Bool) -> PR.ChunkParserK a m ()
takeWhile p = PR.fromParser $ PRD.takeWhile p FL.drain

{-# INLINE takeWhileK #-}
takeWhileK :: MonadIO m =>
    Int -> StreamK m (Array Int) -> m (Either ParseError ())
takeWhileK value = PARSE_OP (takeWhile (<= value))

{-# INLINE splitAp2 #-}
splitAp2 :: MonadIO m
    => Int -> StreamK m (Array Int) -> m (Either ParseError ((), ()))
splitAp2 value =
    PARSE_OP
        ((,)
            <$> takeWhile (<= (value `div` 2))
            <*> takeWhile (<= value)
        )

{-# INLINE splitAp8 #-}
splitAp8 :: MonadIO m
    => Int -> StreamK m (Array Int) -> m (Either ParseError ())
splitAp8 value =
    PARSE_OP
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
sequenceA :: MonadIO m => Int -> StreamK m (Array Int) -> m Int
sequenceA value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequenceA list) xs
    return $ Prelude.length x

{-# INLINE sequenceA_ #-}
sequenceA_ :: MonadIO m =>
    Int -> StreamK m (Array Int) -> m (Either ParseError ())
sequenceA_ value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    PARSE_OP (F.sequenceA_ list) xs

{-# INLINE sequence #-}
sequence :: MonadIO m => Int -> StreamK m (Array Int) -> m Int
sequence value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequence list) xs
    return $ Prelude.length x

{-# INLINE sequence_ #-}
sequence_ :: MonadIO m =>
    Int -> StreamK m (Array Int) -> m (Either ParseError ())
sequence_ value =
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
     in PARSE_OP (F.sequence_ list)

{-# INLINE takeWhileFailD #-}
takeWhileFailD :: Monad m => (a -> Bool) -> Fold m a b -> Parser a m b
takeWhileFailD predicate (Fold fstep finitial fextract) =
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
                      Fold.Partial s1 -> Partial 0 s1
                      Fold.Done b -> Done 0 b
        else return $ Error "fail"

    extract s = fmap (Done 0) (fextract s)

{-# INLINE takeWhileFail #-}
takeWhileFail :: (Monad m, Unbox a) =>
    (a -> Bool) -> Fold m a b -> PR.ChunkParserK a m b
takeWhileFail p f = PR.fromParser (takeWhileFailD p f)

{-# INLINE alt2 #-}
alt2 :: MonadIO m
    => Int -> StreamK m (Array Int) -> m (Either ParseError ())
alt2 value =
    PARSE_OP
        (   takeWhileFail (<= (value `div` 2)) Fold.drain
        <|> takeWhile (<= value)
        )

{-# INLINE alt8 #-}
alt8 :: MonadIO m
    => Int -> StreamK m (Array Int) -> m (Either ParseError ())
alt8 value =
    PARSE_OP
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
alt16 :: MonadIO m
    => Int -> StreamK m (Array Int) -> m (Either ParseError ())
alt16 value =
    PARSE_OP
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
manyAlt :: MonadIO m => StreamK m (Array Int) -> m Int
manyAlt xs = do
    x <- PARSE_OP (AP.many (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE someAlt #-}
someAlt :: MonadIO m => StreamK m (Array Int) -> m Int
someAlt xs = do
    x <- PARSE_OP (AP.some (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE choice #-}
choice :: MonadIO m =>
    Int -> StreamK m (Array Int) -> m (Either ParseError Int)
choice value =
    PARSE_OP (asum (replicate value (satisfy (< 0)))
        AP.<|> satisfy (> 0))

{-# INLINE monad2 #-}
monad2 :: MonadIO m
    => Int -> StreamK m (Array Int) -> m (Either ParseError ())
monad2 value =
    PARSE_OP $ do
        takeWhile (<= (value `div` 2))
        takeWhile (<= value)

{-# INLINE monad4 #-}
monad4 :: MonadIO m
    => Int -> StreamK m (Array Int) -> m (Either ParseError ())
monad4 value =
    PARSE_OP $ do
        takeWhile (<= ( value      `div` 4))
        takeWhile (<= ((value * 2) `div` 4))
        takeWhile (<= ((value * 3) `div` 4))
        takeWhile (<= value)

{-# INLINE monad8 #-}
monad8 :: MonadIO m
    => Int -> StreamK m (Array Int) -> m (Either ParseError ())
monad8 value =
    PARSE_OP $ do
        takeWhile (<= ( value      `div` 8))
        takeWhile (<= ((value * 2) `div` 8))
        takeWhile (<= ((value * 3) `div` 8))
        takeWhile (<= ((value * 4) `div` 8))
        takeWhile (<= ((value * 5) `div` 8))
        takeWhile (<= ((value * 6) `div` 8))
        takeWhile (<= ((value * 7) `div` 8))
        takeWhile (<= value)

{-# INLINE monad16 #-}
monad16 :: MonadIO m
    => Int -> StreamK m (Array Int) -> m (Either ParseError ())
monad16 value =
    PARSE_OP $ do
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
moduleName = "Data.Parser.ParserK"

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ benchIOSink value "drain" (Stream.fold Fold.drain . StreamK.toStream)
    , benchIOSink value "takeWhile" $ takeWhileK value
    , benchIOSink value "splitAp2" $ splitAp2 value
    , benchIOSink value "splitAp8" $ splitAp8 value
    , benchIOSink value "alt2" $ alt2 value
    , benchIOSink value "alt8" $ alt8 value
    , benchIOSink value "alt16" $ alt16 value
    , benchIOSink value "monad2" $ monad2 value
    , benchIOSink value "monad4" $ monad4 value
    , benchIOSink value "monad8" $ monad8 value
    , benchIOSink value "monad16" $ monad16 value
    ]

{-# INLINE sepBy1 #-}
sepBy1 :: MonadIO m => StreamK m (Array Int) -> m Int
sepBy1 xs = do
    x <- PARSE_OP (parser (satisfy odd) (satisfy even)) xs
    return $ Prelude.length x

    where

    parser p sep = do
        x <- p
        fmap (x :) $ AP.many (sep >> p)

-- O(n) heap beacuse of accumulation of the list in strict IO monad?
o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [
    -- accumulates the results in a list
    -- XXX why should this take O(n) heap, it discards the results?
      benchIOSink value "sequence_" $ sequence_ value
    , benchIOSink value "sequenceA_" $ sequenceA_ value
    , benchIOSink value "sequence" $ sequence value
    , benchIOSink value "sequenceA" $ sequenceA value
    , benchIOSink value "manyAlt" manyAlt
    , benchIOSink value "sepBy1" sepBy1
    , benchIOSink value "someAlt" someAlt
    , benchIOSink value "choice" $ choice value
    ]

-- O(n) heap beacuse of accumulation of the list in strict IO monad?
o_1_space_recursive :: Int -> [Benchmark]
o_1_space_recursive value =
    [ benchIOSink value "one (recursive)" $ one value
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOpts defaultStreamSize allBenchmarks

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_serial value)
        , bgroup (o_1_space_prefix moduleName) (o_1_space_recursive value)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
