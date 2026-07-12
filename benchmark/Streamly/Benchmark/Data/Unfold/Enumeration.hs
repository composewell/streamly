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
import Prelude hiding (take, filter, zipWith, map, mapM, takeWhile, scanl, repeat, dropWhile)
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

{-# ANN enumerateFromThenIntegral (PermitPatternMatches [''Int]) #-}
{-# ANN enumerateFromThenIntegral (PermitConstructions []) #-}
{-# ANN enumerateFromThenIntegral (PermitTypeClasses []) #-}
{-# NOINLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: Int -> Int -> IO ()
enumerateFromThenIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromThenNum) (start, 1)

{-# ANN enumerateFromToIntegral (PermitPatternMatches [''Int]) #-}
{-# ANN enumerateFromToIntegral (PermitConstructions []) #-}
{-# ANN enumerateFromToIntegral (PermitTypeClasses []) #-}
{-# NOINLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: Int -> Int -> IO ()
enumerateFromToIntegral size start =
    drainGeneration
    ( UF.supplySecond
      (size + start)
      UF.enumerateFromToNum
    ) start

{-# ANN enumerateFromIntegral (PermitPatternMatches [''Int]) #-}
{-# ANN enumerateFromIntegral (PermitConstructions []) #-}
{-# ANN enumerateFromIntegral (PermitTypeClasses []) #-}
{-# NOINLINE enumerateFromIntegral #-}
enumerateFromIntegral :: Int -> Int -> IO ()
enumerateFromIntegral size =
    drainGeneration (UF.take size UF.enumerateFromNum)

{-# ANN enumerateFromStepNum (PermitPatternMatches [''Int]) #-}
{-# ANN enumerateFromStepNum (PermitConstructions []) #-}
{-# ANN enumerateFromStepNum (PermitTypeClasses []) #-}
{-# NOINLINE enumerateFromStepNum #-}
enumerateFromStepNum :: Int -> Int -> IO ()
enumerateFromStepNum size start =
    drainGeneration (UF.take size UF.enumerateFromThenNum) (start, 1)

{-# ANN enumerateFromNum (PermitPatternMatches [''Int]) #-}
{-# ANN enumerateFromNum (PermitConstructions []) #-}
{-# ANN enumerateFromNum (PermitTypeClasses []) #-}
{-# NOINLINE enumerateFromNum #-}
enumerateFromNum :: Int -> Int -> IO ()
enumerateFromNum size = drainGeneration (UF.take size UF.enumerateFromNum)

{-# ANN enumerateFromToFractional (PermitPatternMatches []) #-}
{-# ANN enumerateFromToFractional (PermitConstructions []) #-}
{-# ANN enumerateFromToFractional (PermitTypeClasses []) #-}
{-# NOINLINE enumerateFromToFractional #-}
enumerateFromToFractional :: Int -> Int -> IO ()
enumerateFromToFractional size start =
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

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    [ (SpaceO_1, benchIO "enumerateFromThenIntegral" $ enumerateFromThenIntegral size)
    , (SpaceO_1, benchIO "enumerateFromToIntegral" $ enumerateFromToIntegral size)
    , (SpaceO_1, benchIO "enumerateFromIntegral" $ enumerateFromIntegral size)
    , (SpaceO_1, benchIO "enumerateFromStepNum" $ enumerateFromStepNum size)
    , (SpaceO_1, benchIO "enumerateFromNum" $ enumerateFromNum size)
    , (SpaceO_1, benchIO "enumerateFromToFractional" $ enumerateFromToFractional size)
    ]
