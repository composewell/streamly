-- |
-- Module      : Streamly.Benchmark.Data.ParserK
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : MIT
-- Maintainer  : streamly@composewell.com

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

import qualified Data.Traversable as TR
import qualified Control.Applicative as AP
import qualified Streamly as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Parser.ParserK.Types as PR
import qualified Streamly.Internal.Data.Parser.ParserD as PRD
import qualified Streamly.Internal.Prelude as IP

import Gauge
import Streamly hiding (runStream)
import Streamly.Benchmark.Common

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- XXX these can be moved to the common module

-- We need a monadic bind here to make sure that the function f does not get
-- completely optimized out by the compiler in some cases.

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
    :: (IsStream t, NFData b)
    => Int -> String -> (t IO Int -> IO b) -> Benchmark
benchIOSink value name f =
    bench name $ nfIO $ randomRIO (1,1) >>= f . sourceUnfoldrM value

-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

{-# INLINE satisfy #-}
satisfy :: MonadCatch m => (a -> Bool) -> PR.Parser m a a
satisfy = PRD.toParserK . PRD.satisfy

{-# INLINE any #-}
any :: MonadCatch m => (a -> Bool) -> PR.Parser m a Bool
any = PRD.toParserK . PRD.any

{-# INLINE anyK #-}
anyK :: (MonadCatch m, Ord a) => a -> SerialT m a -> m Bool
anyK value = IP.parseK (any (> value))

{-# INLINE splitApp #-}
splitApp :: MonadCatch m
    => Int -> SerialT m Int -> m (Bool, Bool)
splitApp value =
    IP.parseK ((,) <$> any (>= (value `div` 2)) <*> any (> value))

{-# INLINE sequenceA #-}
sequenceA :: MonadCatch m => Int -> SerialT m Int -> m Int
sequenceA value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- IP.parseK (TR.sequenceA list) xs
    return $ Prelude.length x

{-# INLINE sequence #-}
sequence :: MonadCatch m => Int -> SerialT m Int -> m Int
sequence value xs = do
    let parser = satisfy (> 0)
        list = Prelude.replicate value parser
    x <- IP.parseK (TR.sequence list) xs
    return $ Prelude.length x

{-# INLINE manyAlt #-}
manyAlt :: MonadCatch m => SerialT m Int -> m Int
manyAlt xs = do
    x <- IP.parseK (AP.many (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE someAlt #-}
someAlt :: MonadCatch m => SerialT m Int -> m Int
someAlt xs = do
    x <- IP.parseK (AP.some (satisfy (> 0))) xs
    return $ Prelude.length x

{-# INLINE choice #-}
choice :: MonadCatch m => Int -> SerialT m Int -> m Int
choice value = do
    IP.parseK (asum (replicate value (satisfy (< 0)))
        AP.<|> satisfy (> 0))

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

o_1_space_serial_parse :: Int -> [Benchmark]
o_1_space_serial_parse value =
    [ benchIOSink value "any" $ anyK value
    , benchIOSink value "splitApp" $ splitApp value
    ]

-- O(n) heap beacuse of accumulation of the list in strict IO monad?
o_n_heap_serial_parse :: Int -> [Benchmark]
o_n_heap_serial_parse value =
    [ benchIOSink value "sequenceA" $ sequenceA value
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
        [ bgroup "o-1-space"
            [ bgroup "parserK" $ concat
                [ o_1_space_serial_parse value
                ]
            ]
        , bgroup "o-n-heap"
            [ bgroup "parserK" $ concat
                [ o_n_heap_serial_parse value
                ]
            ]
        ]
