-- |
-- Module      : Streamly.Benchmark.Data.ParserD
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fspec-constr-recursive=4 #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Prelude hiding (any, all, take, sequence, sequenceA, sequence_, takeWhile, span)

import qualified Data.Traversable as TR
import qualified Data.Foldable as F
import qualified Control.Applicative as AP
import qualified Streamly.Internal.Data.Stream  as S
import qualified Streamly.Internal.Data.Array.Unboxed as Array
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser.ParserD as PR
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Producer.Source as Source
import qualified Streamly.Internal.Data.Stream.IsStream as IP
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Gauge
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (Monad m) => Int -> Int -> Stream m Int
sourceUnfoldrM value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (NFData b)
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-- Make the input unsorted.
{-# INLINE benchIOSinkRandom #-}
benchIOSinkRandom
    :: (NFData b)
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSinkRandom value name f =
    bench name $ nfIO $ randomRIO (1,1)
        >>= f
        . fmap (\x -> if even x then x + 2 else x)
        . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE drainWhile #-}
drainWhile :: MonadThrow m => (a -> Bool) -> PR.Parser m a ()
drainWhile p = PR.takeWhile p FL.drain

{-# INLINE takeStartBy #-}
takeStartBy :: MonadCatch m => Int -> Stream m Int -> m ()
takeStartBy value stream = do
    stream1 <-
        (return . fromMaybe (S.fromPure (value + 1))) . fmap S.fromStreamD
            =<< D.tail (S.toStreamD stream)
    let stream2 = value `S.cons` stream1
    IP.parseD (PR.takeStartBy (== value) FL.drain) stream2

{-# INLINE takeWhile #-}
takeWhile :: MonadThrow m => Int -> Stream m Int -> m ()
takeWhile value = IP.parseD (drainWhile (<= value))

{-# INLINE takeP #-}
takeP :: MonadThrow m => Int -> Stream m a -> m ()
takeP value = IP.parseD (PR.takeP value (PR.fromFold FL.drain))

{-# INLINE takeBetween #-}
takeBetween :: MonadCatch m => Int -> Stream m a -> m ()
takeBetween value =  IP.parseD (PR.takeBetween 0 value FL.drain)

{-# INLINE groupBy #-}
groupBy :: MonadThrow m => Stream m Int -> m ()
groupBy = IP.parseD (PR.groupBy (<=) FL.drain)

{-# INLINE groupByRolling #-}
groupByRolling :: MonadThrow m => Stream m Int -> m ()
groupByRolling = IP.parseD (PR.groupByRolling (<=) FL.drain)

{-# INLINE wordBy #-}
wordBy :: MonadThrow m => Int -> Stream m Int -> m ()
wordBy value = IP.parseD (PR.wordBy (>= value) FL.drain)

{-# INLINE manyWordByEven #-}
manyWordByEven :: MonadCatch m => Stream m Int -> m ()
manyWordByEven =
    IP.parseD (PR.many (PR.wordBy (Prelude.even) FL.drain) FL.drain)

{-# INLINE many #-}
many :: MonadCatch m => Stream m Int -> m Int
many = IP.parseD (PR.many (PR.satisfy (> 0)) FL.length)

{-# INLINE manyAlt #-}
manyAlt :: MonadCatch m => Stream m Int -> m Int
manyAlt xs = do
    x <- IP.parseD (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE some #-}
some :: MonadCatch m => Stream m Int -> m Int
some = IP.parseD (PR.some (PR.satisfy (> 0)) FL.length)

{-# INLINE someAlt #-}
someAlt :: MonadCatch m => Stream m Int -> m Int
someAlt xs = do
    x <- IP.parseD (AP.some (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-#INLINE takeEndBy_ #-}
takeEndBy_ :: MonadCatch m => Int -> Stream m Int -> m ()
takeEndBy_ value = IP.parseD (PR.takeEndBy_ (>= value) (PR.fromFold FL.drain))

{-# INLINE manyTill #-}
manyTill :: MonadCatch m => Int -> Stream m Int -> m Int
manyTill value =
    let p = PR.satisfy (> 0)
        pcond = PR.satisfy (== value)
    in IP.parseD (PR.manyTill FL.length p pcond)

{-# INLINE serialWith #-}
serialWith :: MonadThrow m
    => Int -> Stream m Int -> m ((), ())
serialWith value =
    IP.parseD
        ((,)
            <$> drainWhile (<= (value `div` 2))
            <*> drainWhile (<= value)
        )

{-# INLINE teeAllAny #-}
teeAllAny :: MonadThrow m
    => Int -> Stream m Int -> m ((), ())
teeAllAny value =
    IP.parseD
        (PR.teeWith (,)
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

{-# INLINE teeFstAllAny #-}
teeFstAllAny :: MonadThrow m
    => Int -> Stream m Int -> m ((), ())
teeFstAllAny value =
    IP.parseD
        (PR.teeWithFst (,)
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

{-# INLINE shortestAllAny #-}
shortestAllAny :: MonadThrow m
    => Int -> Stream m Int -> m ()
shortestAllAny value =
    IP.parseD
        (PR.shortest
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

{-# INLINE longestAllAny #-}
longestAllAny :: MonadCatch m
    => Int -> Stream m Int -> m ()
longestAllAny value =
    IP.parseD
        (PR.longest
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

{-# INLINE sequenceParser #-}
sequenceParser :: MonadCatch m => Stream m Int -> m ()
sequenceParser = IP.parseD (PR.sequence FL.drain (D.repeat (PR.satisfy $ const True)))

-------------------------------------------------------------------------------
-- Spanning
-------------------------------------------------------------------------------

{-# INLINE span #-}
span :: MonadThrow m => Int -> Stream m Int -> m ((), ())
span value = IP.parseD (PR.span (<= (value `div` 2)) FL.drain FL.drain)

{-# INLINE spanBy #-}
spanBy :: MonadThrow m => Int -> Stream m Int -> m ((), ())
spanBy value =
    IP.parseD (PR.spanBy (\_ i -> i <= (value `div` 2)) FL.drain FL.drain)

{-# INLINE spanByRolling #-}
spanByRolling :: MonadThrow m => Int -> Stream m Int -> m ((), ())
spanByRolling value =
    IP.parseD (PR.spanByRolling (\_ i -> i <= value `div` 2) FL.drain FL.drain)

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

{-# INLINE parseMany #-}
parseMany :: MonadThrow m => Int -> Stream m Int -> m ()
parseMany value = IP.drain . IP.parseManyD (drainWhile (<= value + 1))

{-# INLINE parseManyGroups #-}
parseManyGroups :: MonadThrow m => Bool -> Stream m Int -> m ()
parseManyGroups b = IP.drain . IP.parseManyD (PR.groupBy (\_ _ -> b) FL.drain)

{-# INLINE parseManyGroupsRolling #-}
parseManyGroupsRolling :: MonadThrow m => Bool -> Stream m Int -> m ()
parseManyGroupsRolling b =
    IP.drain . IP.parseManyD (PR.groupByRolling (\_ _ -> b) FL.drain)

{-# INLINE parseManyGroupsRollingEither #-}
parseManyGroupsRollingEither :: (MonadThrow m, MonadCatch m) =>
    (Int -> Int -> Bool) -> Stream m Int -> m ()
parseManyGroupsRollingEither cmp =
       IP.drain
    .  IP.parseManyD (PR.groupByRollingEither cmp FL.drain FL.drain)

-------------------------------------------------------------------------------
-- Parsing with unfolds
-------------------------------------------------------------------------------

{-# INLINE parseManyUnfoldArrays #-}
parseManyUnfoldArrays :: Int -> [Array.Array Int] -> IO ()
parseManyUnfoldArrays count arrays = do
    let src = Source.source (Just (Producer.OuterLoop arrays))
    let parser = PR.fromFold (FL.take count FL.drain)
    let readSrc =
            Source.producer
                $ Producer.concat Producer.fromList Array.producer
    let streamParser =
            Producer.simplify (Source.parseManyD parser readSrc)
    S.fold FL.drain $ S.unfold streamParser src

-------------------------------------------------------------------------------
-- Parsers in which -fspec-constr-recursive=16 is problematic
-------------------------------------------------------------------------------

-- XXX -fspec-constr-recursive=16 makes GHC go beserk when compiling these.
-- We need to fix GHC so that we can have better control over that option or do
-- not have to rely on it.
--
{-# INLINE lookAhead #-}
lookAhead :: MonadThrow m => Int -> Stream m Int -> m ()
lookAhead value =
    IP.parseD (PR.lookAhead (PR.takeWhile (<= value) FL.drain) $> ())

{-# INLINE sequenceA_ #-}
sequenceA_ :: MonadThrow m => Int -> Stream m Int -> m ()
sequenceA_ value =
    IP.parseD (F.sequenceA_ $ replicate value (PR.satisfy (> 0)))

-- quadratic complexity
{-# INLINE sequenceA #-}
sequenceA :: MonadThrow m => Int -> Stream m Int -> m Int
sequenceA value xs = do
    x <- IP.parseD (TR.sequenceA (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

-- quadratic complexity
{-# INLINE sequence #-}
sequence :: MonadThrow m => Int -> Stream m Int -> m Int
sequence value xs = do
    x <- IP.parseD (TR.sequence (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

{-# INLINE sequence_ #-}
sequence_ :: MonadCatch m => Int -> Stream m Int -> m ()
sequence_ value xs =
    IP.parseD (foldr f (return ()) (replicate value (PR.takeBetween 0 1 FL.drain))) xs

    where

    {-# INLINE f #-}
    f m k = m >>= (\_ -> k)

-- choice using the "Alternative" instance with direct style parser type has
-- quadratic performance complexity.
--
{-# INLINE choiceAsum #-}
choiceAsum :: MonadCatch m => Int -> Stream m Int -> m Int
choiceAsum value =
    IP.parseD (asum (replicate value (PR.satisfy (< 0)))
        AP.<|> PR.satisfy (> 0))

{-# INLINE choice #-}
choice :: MonadCatch m => Int -> Stream m Int -> m Int
choice value =
    IP.parseD
        (PR.choice (replicate value (PR.satisfy (< 0))) AP.<|> PR.satisfy (> 0))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser.ParserD"

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ benchIOSink value "takeWhile" $ takeWhile value
    , benchIOSink value "takeP" $ takeP value
    , benchIOSink value "takeBetween" $ takeBetween value
    , benchIOSink value "takeStartBy" $ takeStartBy value
    , benchIOSink value "groupBy" $ groupBy
    , benchIOSink value "groupByRolling" $ groupByRolling
    , benchIOSink value "wordBy" $ wordBy value
    , benchIOSink value "serialWith" $ serialWith value
    , benchIOSink value "many" many
    , benchIOSink value "many (wordBy even)" $ manyWordByEven
    , benchIOSink value "some" some
    , benchIOSink value "takeEndBy_" $ takeEndBy_ value
    , benchIOSink value "manyTill" $ manyTill value
    , benchIOSink value "tee (all,any)" $ teeAllAny value
    , benchIOSink value "teeFst (all,any)" $ teeFstAllAny value
    , benchIOSink value "shortest (all,any)" $ shortestAllAny value
    , benchIOSink value "longest (all,any)" $ longestAllAny value
    , benchIOSink value "sequenceParser" sequenceParser
    ]

o_1_space_serial_spanning :: Int -> [Benchmark]
o_1_space_serial_spanning value =
    [ benchIOSink value "span" $ span value
    , benchIOSink value "spanBy" $ spanBy value
    , benchIOSink value "spanByRolling" $ spanByRolling value
    ]

o_1_space_serial_nested :: Int -> [Benchmark]
o_1_space_serial_nested value =
    [ benchIOSink value "parseMany" $ parseMany value
    , benchIOSink value "parseMany groupBy (bound groups)"
          $ parseManyGroups False
    , benchIOSink value "parseMany groupRollingBy (bound groups)"
          $ parseManyGroupsRolling False
    , benchIOSink value "parseMany groupBy (1 group)" $ parseManyGroups True
    , benchIOSink value "parseMany groupRollingBy (1 group)"
          $ parseManyGroupsRolling True
    , benchIOSink value "parseMany groupRollingByEither (Left)"
          $ parseManyGroupsRollingEither (<)
    , benchIOSink value "parseMany groupRollingByEither (Right)"
          $ parseManyGroupsRollingEither (>)
    , benchIOSinkRandom value "parseMany groupRollingByEither (Alternating)"
          $ parseManyGroupsRollingEither (>)
    ]

o_1_space_serial_unfold :: Int -> [Array.Array Int] -> [Benchmark]
o_1_space_serial_unfold bound arrays =
    [ bench "parseMany/Unfold/1000 arrays/1 parse"
        $ nfIO $ parseManyUnfoldArrays bound arrays
    , bench "parseMany/Unfold/1000 arrays/100000 parses"
        $ nfIO $ parseManyUnfoldArrays 1 arrays
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [
    -- lookahead benchmark holds the entire input till end
      benchIOSink value "lookAhead" $ lookAhead value

    -- These show non-linear time complexity
    -- They accumulate the results in a list.
    , benchIOSink value "manyAlt" manyAlt
    , benchIOSink value "someAlt" someAlt
    ]

-- accumulate results in a list in IO
o_n_space_serial :: Int -> [Benchmark]
o_n_space_serial value =
    [ benchIOSink value "sequenceA/100" $ sequenceA (value `div` 100)
    , benchIOSink value "sequenceA_/100" $ sequenceA_ (value `div` 100)
    , benchIOSink value "sequence/100" $ sequence (value `div` 100)
    , benchIOSink value "sequence_/100" $ sequence_ (value `div` 100)
    , benchIOSink value "choice (asum)/100" $ choiceAsum (value `div` 100)
    , benchIOSink value "choice/100" $ choice (value `div` 100)
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = runWithCLIOptsEnv defaultStreamSize alloc allBenchmarks

    where

    alloc value = IP.toList $ IP.arraysOf 100 $ sourceUnfoldrM value 0

    allBenchmarks arraysSmall value =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_serial value)
        , bgroup (o_1_space_prefix moduleName) (o_1_space_serial_spanning value)
        , bgroup (o_1_space_prefix moduleName) (o_1_space_serial_nested value)
        , bgroup (o_1_space_prefix moduleName)
            (o_1_space_serial_unfold value arraysSmall)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        , bgroup (o_n_space_prefix moduleName) (o_n_space_serial value)
        ]
