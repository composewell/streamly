-- |
-- Module      : Streamly.Benchmark.Data.Parser
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
import System.Random (randomRIO)
import Prelude hiding (any, all, take)

import qualified Streamly as S hiding (runStream)
import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Parser as PR
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

{-# INLINE any #-}
any :: (Monad m, Ord a) => a -> SerialT m a -> m (Either String Bool)
any value = IP.parse (PR.any (> value))

{-# INLINE all #-}
all :: (Monad m, Ord a) => a -> SerialT m a -> m (Either String Bool)
all value = IP.parse (PR.all (<= value))

{-# INLINE zipAllAny #-}
zipAllAny :: (Monad m, Ord a)
    => a -> SerialT m a -> m (Either String (Bool, Bool))
zipAllAny value = IP.parse ((,) <$> PR.all (<= value) <*> PR.any (> value))

{-# INLINE take #-}
take :: Monad m => Int -> SerialT m a -> m (Either String ())
take value = IP.parse (FL.take value FL.drain)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

o_1_space_serial_parse :: Int -> [Benchmark]
o_1_space_serial_parse value =
    [ benchIOSink value "any" $ any value
    , benchIOSink value "all" $ all value
    , benchIOSink value "(all,any)" $ zipAllAny value
    , benchIOSink value "take" $ take value
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
        [ bgroup "o1"
            [ bgroup "parser" $ concat
                [
                  o_1_space_serial_parse value
                ]
            ]
        ]
