-- |
-- Module      : Streamly.Benchmark.Data.ParserK
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Main
  (
    main
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (MonadCatch)
import Data.Foldable (asum)
import System.Random (randomRIO)
import Prelude hiding (any, all, take, sequence, sequenceA, takeWhile)

import qualified Control.Applicative as AP
import qualified Data.Foldable as F
import qualified Data.Traversable as TR
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser.ParserK.Types as PR
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Data.Stream.IsStream as IP

import Gauge
import Streamly.Prelude (SerialT, MonadAsync, IsStream)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

{-# INLINE sourceUnfoldrM #-}
sourceUnfoldrM :: (IsStream t, MonadAsync m) => Int -> Int -> t m Int
sourceUnfoldrM value n = S.unfoldrM step n
    where
    step cnt =
        if cnt > n + value
        then return Nothing
        else return (Just (cnt, cnt + 1))

-- | Takes a fold method, and uses it with a default source.
{-# INLINE benchIOSink #-}
benchIOSink
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

#ifdef FROM_PARSERK
#define PARSE_OP (IP.parseD . PR.fromParserK)
#else
#define PARSE_OP IP.parseK
#endif

{-# INLINE satisfy #-}
satisfy :: MonadCatch m => (a -> Bool) -> PR.Parser m a a
satisfy = PR.toParserK . PRD.satisfy

{-# INLINE any #-}
any :: MonadCatch m => (a -> Bool) -> PR.Parser m a Bool
any = PR.toParserK . PRD.any

{-# INLINE anyK #-}
anyK :: (MonadCatch m, Ord a) => a -> SerialT m a -> m Bool
anyK value = PARSE_OP (any (> value))

{-# INLINE all #-}
all :: MonadCatch m => (a -> Bool) -> PR.Parser m a Bool
all = PR.toParserK . PRD.all

{-# INLINE allK #-}
allK :: (MonadCatch m, Ord a) => a -> SerialT m a -> m Bool
allK value = PARSE_OP (all (<= value))

{-# INLINE take #-}
take :: MonadCatch m => Int -> PR.Parser m a ()
take value = PR.toParserK $ PRD.take value FL.drain

{-# INLINE takeK #-}
takeK :: MonadCatch m => Int -> SerialT m a -> m ()
takeK value = PARSE_OP (take value)

{-# INLINE takeWhile #-}
takeWhile :: MonadCatch m => (a -> Bool) -> PR.Parser m a ()
takeWhile p = PR.toParserK $ PRD.takeWhile p FL.drain

{-# INLINE takeWhileK #-}
takeWhileK :: MonadCatch m => Int -> SerialT m Int -> m ()
takeWhileK value = PARSE_OP (takeWhile (<= value))

{-# INLINE splitApp #-}
splitApp :: MonadCatch m
    => Int -> SerialT m Int -> m (Bool, Bool)
splitApp value =
    PARSE_OP ((,) <$> any (>= (value `div` 2)) <*> any (> value))

{-# INLINE sequenceA #-}
sequenceA :: MonadCatch m => Int -> SerialT m Int -> m Int
sequenceA value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequenceA list) xs
    return $ Prelude.length x

{-# INLINE sequenceA_ #-}
sequenceA_ :: MonadCatch m => Int -> SerialT m Int -> m ()
sequenceA_ value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    PARSE_OP (F.sequenceA_ list) xs

{-# INLINE sequence #-}
sequence :: MonadCatch m => Int -> SerialT m Int -> m Int
sequence value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- PARSE_OP (TR.sequence list) xs
    return $ Prelude.length x

{-# INLINE manyAlt #-}
manyAlt :: MonadCatch m => SerialT m Int -> m Int
manyAlt xs = do
    x <- PARSE_OP (AP.many (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE someAlt #-}
someAlt :: MonadCatch m => SerialT m Int -> m Int
someAlt xs = do
    x <- PARSE_OP (AP.some (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE choice #-}
choice :: MonadCatch m => Int -> SerialT m Int -> m Int
choice value =
    PARSE_OP (asum (replicate value (satisfy (< 0)))
        AP.<|> satisfy (> 0))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Parser.ParserK"

o_1_space_serial :: Int -> [Benchmark]
o_1_space_serial value =
    [ benchIOSink value "any" $ anyK value
    , benchIOSink value "all" $ allK value
    , benchIOSink value "take" $ takeK value
    , benchIOSink value "takeWhile" $ takeWhileK value
    , benchIOSink value "splitApp" $ splitApp value
    ]

-- O(n) heap beacuse of accumulation of the list in strict IO monad?
o_n_heap_serial :: Int -> [Benchmark]
o_n_heap_serial value =
    [ benchIOSink value "sequenceA" $ sequenceA value
    , benchIOSink value "sequenceA_" $ sequenceA_ value
    , benchIOSink value "sequence" $ sequence value
    , benchIOSink value "manyAlt" manyAlt
    , benchIOSink value "someAlt" someAlt
    , benchIOSink value "choice" $ choice value
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
        [ bgroup (o_1_space_prefix moduleName) $ concat
            [ o_1_space_serial value
            ]
        , bgroup (o_n_heap_prefix moduleName) $ concat
            [ o_n_heap_serial value
            ]
        ]
