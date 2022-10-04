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
import Control.Monad.Catch (MonadThrow)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Streamly.Internal.Data.Stream (Stream)
import System.Random (randomRIO)
import Prelude hiding (any, all, take, sequence, sequenceA, sequence_, takeWhile, span)

import qualified Data.Traversable as TR
import qualified Data.Foldable as F
import qualified Control.Applicative as AP
import qualified Streamly.Internal.Data.Array.Unboxed as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser.ParserD as PR
import qualified Streamly.Internal.Data.Producer as Producer
import qualified Streamly.Internal.Data.Producer.Source as Source
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Internal.Data.Stream.IsStream as IsStream (tail)
import qualified Streamly.Internal.Data.Stream.StreamD as D

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
    :: (NFData b)
    => Int -> String -> (Stream IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE eqBy #-}
eqBy :: Int -> Stream IO Int -> IO ()
eqBy len = Stream.parseD (PR.eqBy (==) [1 .. len])

{-# INLINE matchBy #-}
matchBy :: Int -> Stream IO Int -> IO ()
matchBy len = Stream.parseD (PR.matchBy (==) (D.enumerateFromToIntegral 1 len))

{-# INLINE drainWhile #-}
drainWhile :: MonadThrow m => (a -> Bool) -> PR.Parser m a ()
drainWhile p = PR.takeWhile p Fold.drain

{-# INLINE takeStartBy #-}
takeStartBy :: MonadThrow m => Int -> Stream m Int -> m ()
takeStartBy value stream = do
    stream1 <- return . fromMaybe (Stream.fromPure (value + 1)) =<< IsStream.tail stream
    let stream2 = value `Stream.cons` stream1
    Stream.parseD (PR.takeStartBy (== value) Fold.drain) stream2

{-# INLINE takeWhile #-}
takeWhile :: MonadThrow m => Int -> Stream m Int -> m ()
takeWhile value = Stream.parseD (drainWhile (<= value))

{-# INLINE takeP #-}
takeP :: MonadThrow m => Int -> Stream m a -> m ()
takeP value = Stream.parseD (PR.takeP value (PR.fromFold Fold.drain))

{-# INLINE takeBetween #-}
takeBetween :: MonadThrow m => Int -> Stream m a -> m ()
takeBetween value =  Stream.parseD (PR.takeBetween 0 value Fold.drain)

{-# INLINE groupBy #-}
groupBy :: MonadThrow m => Stream m Int -> m ()
groupBy = Stream.parseD (PR.groupBy (<=) Fold.drain)

{-# INLINE groupByRolling #-}
groupByRolling :: MonadThrow m => Stream m Int -> m ()
groupByRolling = Stream.parseD (PR.groupByRolling (<=) Fold.drain)

{-# INLINE wordBy #-}
wordBy :: MonadThrow m => Int -> Stream m Int -> m ()
wordBy value = Stream.parseD (PR.wordBy (>= value) Fold.drain)

{-# INLINE manyWordByEven #-}
manyWordByEven :: MonadThrow m => Stream m Int -> m ()
manyWordByEven =
    Stream.parseD (PR.many (PR.wordBy (Prelude.even) Fold.drain) Fold.drain)

{-# INLINE many #-}
many :: MonadThrow m => Stream m Int -> m Int
many = Stream.parseD (PR.many (PR.satisfy (> 0)) Fold.length)

{-# INLINE manyAlt #-}
manyAlt :: MonadThrow m => Stream m Int -> m Int
manyAlt xs = do
    x <- Stream.parseD (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE some #-}
some :: MonadThrow m => Stream m Int -> m Int
some = Stream.parseD (PR.some (PR.satisfy (> 0)) Fold.length)

{-# INLINE someAlt #-}
someAlt :: MonadThrow m => Stream m Int -> m Int
someAlt xs = do
    x <- Stream.parseD (AP.some (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-#INLINE takeEndBy_ #-}
takeEndBy_ :: MonadThrow m => Int -> Stream m Int -> m ()
takeEndBy_ value = Stream.parseD (PR.takeEndBy_ (>= value) (PR.fromFold Fold.drain))

{-# INLINE manyTill #-}
manyTill :: MonadThrow m => Int -> Stream m Int -> m Int
manyTill value =
    let p = PR.satisfy (> 0)
        pcond = PR.satisfy (== value)
    in Stream.parseD (PR.manyTill Fold.length p pcond)

{-# INLINE serialWith #-}
serialWith :: MonadThrow m
    => Int -> Stream m Int -> m ((), ())
serialWith value =
    Stream.parseD
        ((,)
            <$> drainWhile (<= (value `div` 2))
            <*> drainWhile (<= value)
        )

{-
{-# INLINE teeAllAny #-}
teeAllAny :: MonadThrow m
    => Int -> Stream m Int -> m ((), ())
teeAllAny value =
    Stream.parseD
        (PR.teeWith (,)
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

{-# INLINE teeFstAllAny #-}
teeFstAllAny :: MonadThrow m
    => Int -> Stream m Int -> m ((), ())
teeFstAllAny value =
    Stream.parseD
        (PR.teeWithFst (,)
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

{-# INLINE shortestAllAny #-}
shortestAllAny :: MonadThrow m
    => Int -> Stream m Int -> m ()
shortestAllAny value =
    Stream.parseD
        (PR.shortest
            (drainWhile (<= value))
            (drainWhile (<= value))
        )

{-# INLINE longestAllAny #-}
longestAllAny :: MonadThrow m
    => Int -> Stream m Int -> m ()
longestAllAny value =
    Stream.parseD
        (PR.longest
            (drainWhile (<= value))
            (drainWhile (<= value))
        )
-}

{-# INLINE sequenceParser #-}
sequenceParser :: MonadThrow m => Stream m Int -> m ()
sequenceParser = Stream.parseD (PR.sequence Fold.drain (D.repeat (PR.satisfy $ const True)))

-------------------------------------------------------------------------------
-- Spanning
-------------------------------------------------------------------------------

{-# INLINE span #-}
span :: MonadThrow m => Int -> Stream m Int -> m ((), ())
span value = Stream.parseD (PR.span (<= (value `div` 2)) Fold.drain Fold.drain)

{-# INLINE spanBy #-}
spanBy :: MonadThrow m => Int -> Stream m Int -> m ((), ())
spanBy value =
    Stream.parseD (PR.spanBy (\_ i -> i <= (value `div` 2)) Fold.drain Fold.drain)

{-# INLINE spanByRolling #-}
spanByRolling :: MonadThrow m => Int -> Stream m Int -> m ((), ())
spanByRolling value =
    Stream.parseD (PR.spanByRolling (\_ i -> i <= value `div` 2) Fold.drain Fold.drain)

-------------------------------------------------------------------------------
-- Nested
-------------------------------------------------------------------------------

{-# INLINE parseMany #-}
parseMany :: MonadThrow m => Int -> Stream m Int -> m ()
parseMany value = Stream.fold Fold.drain . Stream.parseManyD (drainWhile (<= value + 1))

{-# INLINE parseManyGroups #-}
parseManyGroups :: MonadThrow m => Bool -> Stream m Int -> m ()
parseManyGroups b = Stream.fold Fold.drain . Stream.parseManyD (PR.groupBy (\_ _ -> b) Fold.drain)

{-# INLINE parseManyGroupsRolling #-}
parseManyGroupsRolling :: MonadThrow m => Bool -> Stream m Int -> m ()
parseManyGroupsRolling b =
    Stream.fold Fold.drain . Stream.parseManyD (PR.groupByRolling (\_ _ -> b) Fold.drain)

{-# INLINE parseManyGroupsRollingEither #-}
parseManyGroupsRollingEither :: MonadThrow m =>
    (Int -> Int -> Bool) -> Int -> m ()
parseManyGroupsRollingEither cmp value = do
    sourceUnfoldrM value 1
        & Stream.parseManyD (PR.groupByRollingEither cmp Fold.drain Fold.drain)
        & Stream.fold Fold.drain

{-# INLINE parseManyGroupsRollingEitherAlt #-}
parseManyGroupsRollingEitherAlt :: MonadThrow m =>
    (Int -> Int -> Bool) -> Int -> m ()
parseManyGroupsRollingEitherAlt cmp value = do
    sourceUnfoldrM value 1
        -- Make the input unsorted.
        & fmap (\x -> if even x then x + 2 else x)
        & Stream.parseManyD (PR.groupByRollingEither cmp Fold.drain Fold.drain)
        & Stream.fold Fold.drain

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
            Producer.simplify (Source.parseManyD parser readSrc)
    Stream.fold Fold.drain $ Stream.unfold streamParser src

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
    Stream.parseD (PR.lookAhead (PR.takeWhile (<= value) Fold.drain) $> ())

-- XXX The timing of this increased 3x after the stepify extract changes.
{-# INLINE sequenceA_ #-}
sequenceA_ :: MonadThrow m => Int -> Stream m Int -> m ()
sequenceA_ value =
    Stream.parseD (F.sequenceA_ $ replicate value (PR.satisfy (> 0)))

-- quadratic complexity
{-# INLINE sequenceA #-}
sequenceA :: MonadThrow m => Int -> Stream m Int -> m Int
sequenceA value xs = do
    x <- Stream.parseD (TR.sequenceA (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

-- quadratic complexity
{-# INLINE sequence #-}
sequence :: MonadThrow m => Int -> Stream m Int -> m Int
sequence value xs = do
    x <- Stream.parseD (TR.sequence (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

{-# INLINE sequence_ #-}
sequence_ :: MonadThrow m => Int -> Stream m Int -> m ()
sequence_ value xs =
    Stream.parseD (foldr f (return ()) (replicate value (PR.takeBetween 0 1 Fold.drain))) xs

    where

    {-# INLINE f #-}
    f m k = m >>= (\_ -> k)

-- choice using the "Alternative" instance with direct style parser type has
-- quadratic performance complexity.
--
{-# INLINE choiceAsum #-}
choiceAsum :: MonadThrow m => Int -> Stream m Int -> m Int
choiceAsum value =
    Stream.parseD (asum (replicate value (PR.satisfy (< 0)))
        AP.<|> PR.satisfy (> 0))

{-
{-# INLINE choice #-}
choice :: MonadThrow m => Int -> Stream m Int -> m Int
choice value =
    Stream.parseD
        (PR.choice (replicate value (PR.satisfy (< 0))) AP.<|> PR.satisfy (> 0))
-}

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
    {-
    , benchIOSink value "tee (all,any)" $ teeAllAny value
    , benchIOSink value "teeFst (all,any)" $ teeFstAllAny value
    , benchIOSink value "shortest (all,any)" $ shortestAllAny value
    , benchIOSink value "longest (all,any)" $ longestAllAny value
    -}
    , benchIOSink value "sequenceParser" sequenceParser
    , benchIOSink value "eqBy" (eqBy value)
    , benchIOSink value "matchBy" (matchBy value)
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
    , bench "parseMany groupRollingByEither (Left)"
        $ nfIO $ parseManyGroupsRollingEitherLeft
    , bench "parseMany groupRollingByEither (Right)"
        $ nfIO $ parseManyGroupsRollingEitherRight
    , bench "parseMany groupRollingByEither (Alternating)"
        $ nfIO $ parseManyGroupsRollingEitherAlt1
    ]

    where

    {-# NOINLINE parseManyGroupsRollingEitherLeft #-}
    parseManyGroupsRollingEitherLeft = parseManyGroupsRollingEither (<) value

    {-# NOINLINE parseManyGroupsRollingEitherRight #-}
    parseManyGroupsRollingEitherRight = parseManyGroupsRollingEither (>) value

    {-# NOINLINE parseManyGroupsRollingEitherAlt1 #-}
    parseManyGroupsRollingEitherAlt1 =
        parseManyGroupsRollingEitherAlt (>) value

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
    -- , benchIOSink value "choice/100" $ choice (value `div` 100)
    ]

-------------------------------------------------------------------------------
-- Driver
-------------------------------------------------------------------------------

main :: IO ()
main = do
    x <- randomRIO (1,1)
    runWithCLIOptsEnv (defaultStreamSize + x) alloc allBenchmarks

    where

    alloc value = Stream.fold Fold.toList $ Stream.arraysOf 100 $ sourceUnfoldrM value 0

    allBenchmarks arraysSmall value =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_serial value)
        , bgroup (o_1_space_prefix moduleName) (o_1_space_serial_spanning value)
        , bgroup (o_1_space_prefix moduleName) (o_1_space_serial_nested value)
        , bgroup (o_1_space_prefix moduleName)
            (o_1_space_serial_unfold value arraysSmall)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        , bgroup (o_n_space_prefix moduleName) (o_n_space_serial value)
        ]
