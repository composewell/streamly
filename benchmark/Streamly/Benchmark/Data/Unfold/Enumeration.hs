-- |
-- Module      : Unfold.Enumeration
-- Copyright   : (c) 2018 Composewell
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}

module Unfold.Enumeration (benchmarks) where

import Control.DeepSeq (NFData(..))
import Streamly.Internal.Data.Unfold (Unfold)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF

import Fusion.Plugin.Types
import Test.Tasty.Bench hiding (env)
import Prelude hiding
    (take, filter, zipWith, map, mapM, takeWhile, scanl, repeat, dropWhile)
import Streamly.Benchmark.Common

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

{-# INLINE drainGeneration #-}
drainGeneration :: Monad m => Unfold m a b -> a -> m ()
drainGeneration = UF.fold FL.drain

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

{-# ANN enumerateFromNum (PermitPatternMatches [''Int]) #-}
{-# ANN enumerateFromNum (PermitConstructions []) #-}
{-# ANN enumerateFromNum (PermitTypeClasses []) #-}
{-# NOINLINE enumerateFromNum #-}
enumerateFromNum :: Int -> Int -> IO ()
enumerateFromNum size = drainGeneration (UF.take size UF.enumerateFromNum)

{-# ANN enumerateFromThenNum (PermitPatternMatches [''Int]) #-}
{-# ANN enumerateFromThenNum (PermitConstructions []) #-}
{-# ANN enumerateFromThenNum (PermitTypeClasses []) #-}
{-# NOINLINE enumerateFromThenNum #-}
enumerateFromThenNum :: Int -> Int -> IO ()
enumerateFromThenNum size start =
    drainGeneration (UF.take size UF.enumerateFromThenNum) (start, 1)

{-# ANN enumerateFromToNum (PermitPatternMatches [''Int]) #-}
{-# ANN enumerateFromToNum (PermitConstructions []) #-}
{-# ANN enumerateFromToNum (PermitTypeClasses []) #-}
{-# NOINLINE enumerateFromToNum #-}
enumerateFromToNum :: Int -> Int -> IO ()
enumerateFromToNum size start =
    drainGeneration
    ( UF.supplySecond
      (size + start)
      UF.enumerateFromToNum
    ) start

{-# ANN enumerateFromToRealFloat (PermitPatternMatches []) #-}
{-# ANN enumerateFromToRealFloat (PermitConstructions []) #-}
{-# ANN enumerateFromToRealFloat (PermitTypeClasses []) #-}
{-# NOINLINE enumerateFromToRealFloat #-}
enumerateFromToRealFloat :: Int -> Int -> IO ()
enumerateFromToRealFloat size start =
    let intToDouble x = fromInteger (fromIntegral x) :: Double
     in drainGeneration
            ( UF.supplySecond
              (intToDouble $ start + size)
              UF.enumerateFromToRealFloat
            )
            (intToDouble start)

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

-- Benchmark naming: name each benchmark (and its IO action) after the exported
-- function it benchmarks, using combinator_dimension1_dimension2..., where the
-- dimensions are optional variants/type specializations (used esp. when more
-- than one specialization is benchmarked). Keep extra info in parenthetical
-- notes in the description; these also disambiguate benchmarks that reuse a
-- single IO action with different arguments. If the name has a trailing
-- underscore, add one more underscore.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    [ (SpaceO_1, benchIO "enumerateFromNum" $ enumerateFromNum size)
    , (SpaceO_1, benchIO "enumerateFromThenNum" $ enumerateFromThenNum size)
    , (SpaceO_1, benchIO "enumerateFromToNum" $ enumerateFromToNum size)
    , (SpaceO_1, benchIO "enumerateFromToRealFloat" $
          enumerateFromToRealFloat size)
    ]
