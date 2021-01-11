-- |
-- Module      : Streamly.Benchmark.Data.Parser
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadCatch)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.Monoid (Sum(..))
import System.Random (randomRIO)
import Prelude
       hiding (any, all, take, sequence, sequence_, sequenceA, takeWhile)

import qualified Data.Traversable as TR
import qualified Data.Foldable as F
import qualified Control.Applicative as AP
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as IP

import Gauge
import Streamly.Prelude (SerialT)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (S.IsStream t, S.MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (S.IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE takeBetween #-}
takeBetween :: MonadCatch m => Int -> SerialT m a -> m ()
takeBetween value =  IP.parse (PR.takeBetween 0 value FL.drain)

{-# INLINE takeEQ #-}
takeEQ :: MonadCatch m => Int -> SerialT m a -> m ()
takeEQ value = IP.parse (PR.takeEQ value FL.drain)

{-# INLINE drainWhile #-}
drainWhile :: MonadCatch m => Int -> SerialT m Int -> m ()
drainWhile value = IP.parse (PR.drainWhile (<= value))

{-# INLINE takeWhile #-}
takeWhile :: MonadCatch m => Int -> SerialT m Int -> m ()
takeWhile value = IP.parse (PR.takeWhile (<= value) FL.drain)

{-# INLINE groupBy #-}
groupBy :: MonadCatch m => SerialT m Int -> m ()
groupBy = IP.parse (PR.groupBy (<=) FL.drain)

{-# INLINE wordBy #-}
wordBy :: MonadCatch m => Int -> SerialT m Int -> m ()
wordBy value = IP.parse (PR.wordBy (>= value) FL.drain)

{-# INLINE manyWordByEven #-}
manyWordByEven :: MonadCatch m => SerialT m Int -> m ()
manyWordByEven = IP.parse (PR.many FL.drain (PR.wordBy (Prelude.even) FL.drain))

{-# INLINE many #-}
many :: MonadCatch m => SerialT m Int -> m Int
many = IP.parse (PR.many FL.length (PR.satisfy (> 0)))

{-# INLINE manyAlt #-}
manyAlt :: MonadCatch m => SerialT m Int -> m Int
manyAlt xs = do
    x <- IP.parse (AP.many (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE some #-}
some :: MonadCatch m => SerialT m Int -> m Int
some = IP.parse (PR.some FL.length (PR.satisfy (> 0)))

{-# INLINE someAlt #-}
someAlt :: MonadCatch m => SerialT m Int -> m Int
someAlt xs = do
    x <- IP.parse (AP.some (PR.satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE manyTill #-}
manyTill :: MonadCatch m => Int -> SerialT m Int -> m Int
manyTill value =
    IP.parse (PR.manyTill FL.length (PR.satisfy (> 0)) (PR.satisfy (== value)))

{-# INLINE splitAp #-}
splitAp :: MonadCatch m
    => Int -> SerialT m Int -> m ((), ())
splitAp value =
    IP.parse
        ((,)
            <$> PR.drainWhile (<= (value `div` 2))
            <*> PR.drainWhile (<= value)
        )

{-# INLINE splitApBefore #-}
splitApBefore :: MonadCatch m
    => Int -> SerialT m Int -> m ()
splitApBefore value =
    IP.parse
        (  PR.drainWhile (<= (value `div` 2))
        *> PR.drainWhile (<= value)
        )

{-# INLINE splitApAfter #-}
splitApAfter :: MonadCatch m
    => Int -> SerialT m Int -> m ()
splitApAfter value =
    IP.parse
        (  PR.drainWhile (<= (value `div` 2))
        <* PR.drainWhile (<= value)
        )

{-# INLINE splitWith #-}
splitWith :: MonadCatch m
    => Int -> SerialT m Int -> m ((), ())
splitWith value =
    IP.parse
        (PR.splitWith (,)
            (PR.drainWhile (<= (value `div` 2)))
            (PR.drainWhile (<= value))
        )

{-# INLINE split_ #-}
split_ :: MonadCatch m
    => Int -> SerialT m Int -> m ()
split_ value =
    IP.parse
        (PR.split_
            (PR.drainWhile (<= (value `div` 2)))
            (PR.drainWhile (<= value))
        )

{-# INLINE sliceSepBy #-}
sliceSepBy :: MonadCatch m
    => Int -> SerialT m Int -> m()
sliceSepBy value = IP.parse (PR.sliceSepBy (>= value) (PR.many FL.drain
                    (PR.satisfy (const True))))

{-# INLINE teeAllAny #-}
teeAllAny :: MonadCatch m
    => Int -> SerialT m Int -> m ((), ())
teeAllAny value =
    IP.parse
        (PR.teeWith (,)
            (PR.drainWhile (<= value))
            (PR.drainWhile (<= value))
        )

{-# INLINE teeFstAllAny #-}
teeFstAllAny :: MonadCatch m
    => Int -> SerialT m Int -> m ((), ())
teeFstAllAny value =
    IP.parse
        (PR.teeWithFst (,)
            (PR.drainWhile (<= value))
            (PR.drainWhile (<= value))
        )

{-# INLINE shortestAllAny #-}
shortestAllAny :: MonadCatch m
    => Int -> SerialT m Int -> m ()
shortestAllAny value =
    IP.parse
        (PR.shortest
            (PR.drainWhile (<= value))
            (PR.drainWhile (<= value))
        )

{-# INLINE longestAllAny #-}
longestAllAny :: MonadCatch m
    => Int -> SerialT m Int -> m ()
longestAllAny value =
    IP.parse
        (PR.longest
            (PR.drainWhile (<= value))
            (PR.drainWhile (<= value))
        )

-------------------------------------------------------------------------------
-- Parsers in which -fspec-constr-recursive=16 is problematic
-------------------------------------------------------------------------------

-- XXX -fspec-constr-recursive=16 makes GHC go beserk when compiling these.
-- We need to fix GHC so that we can have better control over that option or do
-- not have to rely on it.
--
{-# INLINE lookAhead #-}
lookAhead :: MonadCatch m => Int -> SerialT m Int -> m ()
lookAhead value =
    IP.parse (PR.lookAhead (PR.takeWhile (<= value) FL.drain) $> ())

{-# INLINE sequenceA #-}
sequenceA :: MonadCatch m => Int -> SerialT m Int -> m Int
sequenceA value xs = do
    x <- IP.parse (TR.sequenceA (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

{-# INLINE sequenceA_ #-}
sequenceA_ :: MonadCatch m => Int -> SerialT m Int -> m ()
sequenceA_ value =
    IP.parse (F.sequenceA_ $ replicate value (PR.satisfy (> 0)))

{-# INLINE sequence #-}
sequence :: MonadCatch m => Int -> SerialT m Int -> m Int
sequence value xs = do
    x <- IP.parse (TR.sequence (replicate value (PR.satisfy (> 0)))) xs
    return $ length x

{-# INLINE sequence_ #-}
sequence_ :: MonadCatch m => Int -> SerialT m Int -> m ()
sequence_ value =
    IP.parse (F.sequence_ $ replicate value (PR.satisfy (> 0)))

{-# INLINE choice #-}
choice :: MonadCatch m => Int -> SerialT m Int -> m Int
choice value =
    IP.parse (asum (replicate value (PR.satisfy (< 0)))
        AP.<|> PR.satisfy (> 0))

-------------------------------------------------------------------------------
-- Stream transformation
-------------------------------------------------------------------------------

{-# INLINE parseMany #-}
parseMany :: MonadCatch m => SerialT m Int -> m ()
parseMany =
      S.drain
    . S.map getSum
    . IP.parseMany (PR.fromFold $ FL.ltake 2 FL.mconcat)
    . S.map Sum

{-# INLINE parseIterate #-}
parseIterate :: MonadCatch m => SerialT m Int -> m ()
parseIterate =
      S.drain
    . S.map getSum
    . IP.parseIterate (PR.fromFold . FL.ltake 2 . FL.sconcat) (Sum 0)
    . S.map Sum

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser"

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ benchIOSink value "takeBetween" $ takeBetween value
    , benchIOSink value "takeEQ" $ takeEQ value
    , benchIOSink value "takeWhile" $ takeWhile value
    , benchIOSink value "drainWhile" $ drainWhile value
    , benchIOSink value "groupBy" $ groupBy
    , benchIOSink value "wordBy" $ wordBy value
    , benchIOSink value "splitAp" $ splitAp value
    , benchIOSink value "splitApBefore" $ splitApBefore value
    , benchIOSink value "splitApAfter" $ splitApAfter value
    , benchIOSink value "splitWith" $ splitWith value
    , benchIOSink value "sliceSepBy" $ sliceSepBy value
    , benchIOSink value "many" many
    , benchIOSink value "many (wordBy even)" $ manyWordByEven
    , benchIOSink value "some" some
    , benchIOSink value "manyTill" $ manyTill value
    , benchIOSink value "tee" $ teeAllAny value
    , benchIOSink value "teeFst" $ teeFstAllAny value
    , benchIOSink value "shortest" $ shortestAllAny value
    , benchIOSink value "longest" $ longestAllAny value
    , benchIOSink value "parseMany" parseMany
    , benchIOSink value "parseIterate" parseIterate
    ]

o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [
    -- lookahead benchmark holds the entire input till end
      benchIOSink value "lookAhead" $ lookAhead value

    -- accumulates the results in a list
    , benchIOSink value "sequence" $ sequence value
    , benchIOSink value "sequenceA" $ sequenceA value

    -- XXX why should this take O(n) heap, it discards the results?
    , benchIOSink value "sequence_" $ sequence_ value
    , benchIOSink value "sequenceA_" $ sequenceA_ value
    -- non-linear time complexity (parserD)
    , benchIOSink value "split_" $ split_ value
    -- XXX why O(n) heap?
    , benchIOSink value "choice" $ choice value

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
    (value, cfg, benches) <- parseCLIOpts defaultStreamSize
    value `seq` runMode (mode cfg) cfg benches (allBenchmarks value)

    where

    allBenchmarks value =
        [ bgroup (o_1_space_prefix moduleName) (o_1_space_serial value)
        , bgroup (o_n_heap_prefix moduleName) (o_n_heap_serial value)
        ]
