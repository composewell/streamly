{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Benchmark.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Data.Functor (($>))
import Data.Monoid (Sum(..))
import GHC.Magic (inline)
import GHC.Magic (noinline)
import System.IO (Handle)
import System.Random (randomRIO)
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.Stream.StreamD (Stream)
import Prelude hiding
    (any, all, take, sequence, sequence_, sequenceA, takeWhile, dropWhile)

import qualified Control.Applicative as AP
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Producer.Source as Source
import qualified Streamly.Internal.Data.Stream.StreamD as Stream

import Gauge hiding (env)
import Streamly.Benchmark.Common
import Streamly.Benchmark.Common.Handle

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

-- | Generates something like this: { { \{ \{ } }.  The stream consists of
-- three parts, the first part is contains a sequence of `{`. The second part
-- contains a sequence pf escaped values `\{`. The third part contains a
-- sequence of `}`.
{-# INLINE sourceEscapedFrames #-}
sourceEscapedFrames ::
    Monad m
    => Int
    -> Int
    -> Stream m Char
sourceEscapedFrames value n = Stream.unfoldrM step n
    where

    bs = '\\'
    cbOpen = '{'
    cbClose = '}'
    value1 = value `div` 4

    step cnt
        | cnt > 4 * value1 = return Nothing
        | cnt <= value1 = return $ Just (cbOpen, cnt + 1)
        | cnt > 3 * value1 = return $ Just (cbClose, cnt + 1)
        | otherwise =
            return
                $ Just
                $ if (cnt - value1) `mod` 2 == 1
                  then (bs, cnt + 1)
                  else (cbOpen, cnt + 1)

{-# INLINE benchIOSrc #-}
benchIOSrc
    :: NFData b
    => (Int -> Int -> Stream IO a)
    -> Int
    -> String
    -> (Stream IO a -> IO b)
    -> Benchmark
benchIOSrc src value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . src value

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: NFData b
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE takeBetween #-}
takeBetween :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeBetween value = Stream.parse (PR.takeBetween 0 value Fold.drain)

{-# INLINE takeEQ #-}
takeEQ :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeEQ value = Stream.parse (PR.takeEQ value Fold.drain)

{-# INLINE dropWhile #-}
dropWhile :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
dropWhile value = Stream.parse (PR.dropWhile (<= value))

{-# INLINE takeStartBy #-}
takeStartBy :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
takeStartBy value stream = do
    let stream2 = value `Stream.cons` stream
    Stream.parse (PR.takeStartBy (== value) Fold.drain) stream2

takeFramedByEsc_ :: Monad m => Int -> Stream m Char -> m (Either ParseError ())
takeFramedByEsc_ _ = Stream.parse parser

    where

    isEsc = (== '\\')
    isBegin = (== '{')
    isEnd = (== '}')

    parser = PR.takeFramedByEsc_ isEsc isBegin isEnd Fold.drain

{-# INLINE listEqBy #-}
listEqBy :: Int -> Stream IO Int -> IO (Either ParseError [Int])
listEqBy len = Stream.parse (PR.listEqBy (==) [1 .. len])

{-# INLINE streamEqBy #-}
streamEqBy :: Int -> Stream IO Int -> IO (Either ParseError ())
streamEqBy len = Stream.parse (PR.streamEqBy (==) (Stream.enumerateFromTo 1 len))

{-# INLINE takeWhile #-}
takeWhile :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
takeWhile value = Stream.parse (PR.takeWhile (<= value) Fold.drain)

takeWhileP :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
takeWhileP value =
    Stream.parse (PR.takeWhileP (<= value) (PR.takeWhile (<= value - 1) Fold.drain))

{-# INLINE takeP #-}
takeP :: Monad m => Int -> Stream m a -> m (Either ParseError ())
takeP value = Stream.parse (PR.takeP value (PR.fromFold Fold.drain))

{-# INLINE groupBy #-}
groupBy :: Monad m => Stream m Int -> m (Either ParseError ())
groupBy = Stream.parse (PR.groupBy (<=) Fold.drain)

{-# INLINE groupByRolling #-}
groupByRolling :: Monad m => Stream m Int -> m (Either ParseError ())
groupByRolling = Stream.parse (PR.groupByRolling (<=) Fold.drain)

{-# INLINE wordBy #-}
wordBy :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
wordBy value = Stream.parse (PR.wordBy (>= value) Fold.drain)

{-# INLINE sepByWords #-}
sepByWords :: Monad m => Stream m Int -> m (Either ParseError ())
sepByWords = Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepBy (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

-- Returning a list to compare with the sepBy1 in ParserK
{-# INLINE sepBy1 #-}
sepBy1 :: Monad m => Stream m Int -> m (Either ParseError [Int])
sepBy1 xs = do
    Stream.parse (PR.sepBy1 (PR.satisfy odd) (PR.satisfy even) Fold.toList) xs

{-# INLINE sepByWords1 #-}
sepByWords1 :: Monad m => Stream m Int -> m (Either ParseError ())
sepByWords1 = Stream.parse (wrds even Fold.drain)
    where
    wrds p = PR.sepBy1 (PR.takeWhile (not . p) Fold.drain) (PR.dropWhile p)

{-# INLINE deintercalate #-}
deintercalate :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
deintercalate _ = Stream.parse (partition even)

    where

    partition p =
        PR.deintercalate
            (PR.takeWhile (not . p) Fold.sum) (PR.takeWhile p Fold.sum) Fold.drain

{-# INLINE manyWordByEven #-}
manyWordByEven :: Monad m => Stream m Int -> m (Either ParseError ())
manyWordByEven = Stream.parse (PR.many (PR.wordBy even Fold.drain) Fold.drain)

{-# INLINE many #-}
many :: Monad m => Stream m Int -> m (Either ParseError Int)
many = Stream.parse (PR.many (PR.satisfy (> 0)) Fold.length)

{-# INLINE manyAlt #-}
manyAlt :: Monad m => Stream m Int -> m Int
manyAlt xs = do
    x <- Stream.parse (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE some #-}
some :: Monad m => Stream m Int -> m (Either ParseError Int)
some = Stream.parse (PR.some (PR.satisfy (> 0)) Fold.length)

{-# INLINE someAlt #-}
someAlt :: Monad m => Stream m Int -> m Int
someAlt xs = do
    x <- Stream.parse (AP.some (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE manyTill #-}
manyTill :: Monad m => Int -> Stream m Int -> m (Either ParseError Int)
manyTill value =
    Stream.parse (PR.manyTill (PR.satisfy (> 0)) (PR.satisfy (== value)) Fold.length)

{-# INLINE splitAp #-}
splitAp :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ((), ()))
splitAp value =
    Stream.parse
        ((,)
            <$> PR.dropWhile (<= (value `div` 2))
            <*> PR.dropWhile (<= value)
        )

{-# INLINE splitApBefore #-}
splitApBefore :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
splitApBefore value =
    Stream.parse
        (  PR.dropWhile (<= (value `div` 2))
        *> PR.dropWhile (<= value)
        )

{-# INLINE splitApAfter #-}
splitApAfter :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
splitApAfter value =
    Stream.parse
        (  PR.dropWhile (<= (value `div` 2))
        <* PR.dropWhile (<= value)
        )

{-# INLINE splitWith #-}
splitWith :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ((), ()))
splitWith value =
    Stream.parse
        (PR.splitWith (,)
            (PR.dropWhile (<= (value `div` 2)))
            (PR.dropWhile (<= value))
        )

{-# INLINE split_ #-}
split_ :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
split_ value =
    Stream.parse
        (PR.split_
            (PR.dropWhile (<= (value `div` 2)))
            (PR.dropWhile (<= value))
        )

{-# INLINE takeEndBy_ #-}
takeEndBy_ :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
takeEndBy_ value = Stream.parse (PR.takeEndBy_ (>= value) (PR.fromFold Fold.drain))

{-
{-# INLINE teeAllAny #-}
teeAllAny :: Monad m
    => Int -> Stream m Int -> m ((), ())
teeAllAny value =
    Stream.parse
        (PR.teeWith (,)
            (PR.dropWhile (<= value))
            (PR.dropWhile (<= value))
        )

{-# INLINE teeFstAllAny #-}
teeFstAllAny :: Monad m
    => Int -> Stream m Int -> m ((), ())
teeFstAllAny value =
    Stream.parse
        (PR.teeWithFst (,)
            (PR.dropWhile (<= value))
            (PR.dropWhile (<= value))
        )

{-# INLINE shortestAllAny #-}
shortestAllAny :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
shortestAllAny value =
    Stream.parse
        (PR.shortest
            (PR.dropWhile (<= value))
            (PR.dropWhile (<= value))
        )

{-# INLINE longestAllAny #-}
longestAllAny :: Monad m
    => Int -> Stream m Int -> m (Either ParseError ())
longestAllAny value =
    Stream.parse
        (PR.longest
            (PR.dropWhile (<= value))
            (PR.dropWhile (<= value))
        )
-}

parseManyChunksOfSum :: Int -> Handle -> IO Int
parseManyChunksOfSum n inh =
    Stream.fold Fold.length
        $ Stream.parseMany
              (PR.fromFold $ Fold.take n Fold.sum)
              (Stream.unfold Handle.reader inh)

-------------------------------------------------------------------------------
-- Parsing with unfolds
-------------------------------------------------------------------------------

{-# INLINE parseManyUnfoldArrays #-}
parseManyUnfoldArrays :: Int -> [Array.Array Int] -> IO ()
parseManyUnfoldArrays count arrays = do
    let src = Source.source (Just (Producer.OuterLoop arrays))
    let parser = PR.fromFold (Fold.take count Fold.drain)
    let readSrc =
            Source.producer
                $ Producer.concat Producer.fromList Array.producer
    let streamParser =
            Producer.simplify (Source.parseMany parser readSrc)
    Stream.fold Fold.drain $ Stream.unfold streamParser src

-------------------------------------------------------------------------------
-- Parsers in which -fspec-constr-recursive=16 is problematic
-------------------------------------------------------------------------------

-- XXX -fspec-constr-recursive=16 makes GHC go beserk when compiling these.
-- We need to fix GHC so that we can have better control over that option or do
-- not have to rely on it.
--
{-# INLINE lookAhead #-}
lookAhead :: Monad m => Int -> Stream m Int -> m (Either ParseError ())
lookAhead value =
    Stream.parse (PR.lookAhead (PR.takeWhile (<= value) Fold.drain) $> ())

{-
{-# INLINE choice #-}
choice :: Monad m => Int -> Stream m Int -> m (Either ParseError Int)
choice value =
    Stream.parse
        (PR.choice (replicate value (PR.satisfy (< 0))) AP.<|> PR.satisfy (> 0))
-}

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE parseMany #-}
parseMany :: Monad m => Int -> Stream m Int -> m ()
parseMany n =
      Stream.fold Fold.drain
    . fmap getSum
    . Stream.catRights . Stream.parseMany (PR.fromFold $ Fold.take n Fold.mconcat)
    . fmap Sum

{-# INLINE parseIterate #-}
parseIterate :: Monad m => Int -> Stream m Int -> m ()
parseIterate n =
      Stream.fold Fold.drain
    . fmap getSum
    . Stream.catRights
    . Stream.parseIterate
        (PR.fromFold . Fold.take n . Fold.sconcat)
        (Sum 0)
    . fmap Sum

{-# INLINE concatSequence #-}
concatSequence :: Monad m => Stream m Int -> m (Either ParseError ())
concatSequence =
    Stream.parse $ PR.concatSequence (Stream.repeat PR.one) Fold.drain

{-# INLINE parseManyGroupBy #-}
parseManyGroupBy :: Monad m => (Int -> Int -> Bool) -> Stream m Int -> m ()
parseManyGroupBy cmp =
    Stream.fold Fold.drain . Stream.parseMany (PR.groupBy cmp Fold.drain)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser"

instance NFData ParseError where
    {-# INLINE rnf #-}
    rnf (ParseError x) = rnf x

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ benchIOSink value "takeBetween" $ takeBetween value
    , benchIOSink value "takeEQ" $ takeEQ value
    , benchIOSink value "takeWhile" $ takeWhile value
    , benchIOSink value "takeWhileP" $ takeWhileP value
    , benchIOSink value "takeP" $ takeP value
    , benchIOSink value "dropWhile" $ dropWhile value
    , benchIOSink value "takeStartBy" $ takeStartBy value
    , benchIOSrc sourceEscapedFrames value "takeFramedByEsc_"
        $ takeFramedByEsc_ value
    , benchIOSink value "groupBy" $ groupBy
    , benchIOSink value "groupByRolling" $ groupByRolling
    , benchIOSink value "wordBy" $ wordBy value
    , benchIOSink value "sepBy (words)" sepByWords
    , benchIOSink value "sepBy1" sepBy1
    , benchIOSink value "sepBy1 (words)" sepByWords1
    , benchIOSink value "deintercalate" $ deintercalate value
    , benchIOSink value "splitAp" $ splitAp value
    , benchIOSink value "splitApBefore" $ splitApBefore value
    , benchIOSink value "splitApAfter" $ splitApAfter value
    , benchIOSink value "splitWith" $ splitWith value
    , benchIOSink value "takeEndBy_" $ takeEndBy_ value
    , benchIOSink value "many" many
    , benchIOSink value "many (wordBy even)" $ manyWordByEven
    , benchIOSink value "some" some
    , benchIOSink value "manyTill" $ manyTill value
    {-
    , benchIOSink value "tee" $ teeAllAny value
    , benchIOSink value "teeFst" $ teeFstAllAny value
    , benchIOSink value "shortest" $ shortestAllAny value
    , benchIOSink value "longest" $ longestAllAny value
    -}
    , benchIOSink value "parseMany (take 1)" (parseMany 1)
    , benchIOSink value "parseMany (take all)" (parseMany value)
    , benchIOSink value "parseIterate (take 1)" (parseIterate 1)
    , benchIOSink value "parseIterate (take all)" (parseIterate value)
    , benchIOSink value "concatSequence" concatSequence
    , benchIOSink value "parseMany (groupBy (<))" (parseManyGroupBy (<))
    , benchIOSink value "parseMany (groupBy (==))" (parseManyGroupBy (==))
    , benchIOSink value "listEqBy" (listEqBy value)
    , benchIOSink value "streamEqBy" (streamEqBy value)
    ]

o_1_space_filesystem :: BenchEnv -> [Benchmark]
o_1_space_filesystem env =
    [ mkBench ("S.parseMany (Fold.take " ++ show (bigSize env) ++ " Fold.sum)") env
          $ \inh _ -> noinline parseManyChunksOfSum (bigSize env) inh
    , mkBench "S.parseMany (Fold.take 1 Fold.sum)" env
          $ \inh _ -> inline parseManyChunksOfSum 1 inh
    ]

o_1_space_serial_unfold :: Int -> [Array.Array Int] -> [Benchmark]
o_1_space_serial_unfold bound arrays =
    [ bench "parseMany/Unfold/1000 arrays/take all"
        $ nfIO $ parseManyUnfoldArrays bound arrays
    , bench "parseMany/Unfold/1000 arrays/take 1"
        $ nfIO $ parseManyUnfoldArrays 1 arrays
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [
    -- lookahead benchmark holds the entire input till end
      benchIOSink value "lookAhead" $ lookAhead value

    -- non-linear time complexity (parserD)
    , benchIOSink value "split_" $ split_ value
    -- XXX why O(n) heap?
    -- , benchIOSink value "choice" $ choice value

    -- These show non-linear time complexity.
    -- They accumulate the results in a list.
    , benchIOSink value "manyAlt" manyAlt
    , benchIOSink value "someAlt" someAlt
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    env <- mkHandleBenchEnv
    runWithCLIOptsEnv defaultStreamSize alloc (allBenchmarks env)

    where

    alloc value = Stream.fold Fold.toList $ Stream.arraysOf 100 $ sourceUnfoldrM value 0

    allBenchmarks env arrays value =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_serial value)
        , bgroup
              (o_1_space_prefix moduleName ++ "/filesystem")
              (o_1_space_filesystem env)
        , bgroup (o_1_space_prefix moduleName)
            (o_1_space_serial_unfold value arrays)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
