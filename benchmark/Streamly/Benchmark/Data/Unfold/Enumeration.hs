-- |
-- Module      : Unfold.Enumeration
-- Copyright   : (c) 2018 Composewell
-- License     : MIT
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#undef FUSION_CHECK
#ifdef FUSION_CHECK
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
#endif

#ifdef __HADDOCK_VERSION__
#undef INSPECTION
#endif

#ifdef INSPECTION
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
#endif

module Unfold.Enumeration (benchmarks) where

import Control.DeepSeq (NFData(..))
import Streamly.Internal.Data.Unfold (Unfold)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF


import Test.Tasty.Bench hiding (env)
import Prelude hiding (take, filter, zipWith, map, mapM, takeWhile, scanl, repeat, dropWhile)
import Streamly.Benchmark.Common

#ifdef INSPECTION
import GHC.Types (SPEC(..))
import Test.Inspection
import qualified Streamly.Internal.Data.Stream as S
#endif

{-# INLINE benchIO #-}
benchIO :: (NFData b) => String -> (Int -> IO b) -> Benchmark
benchIO name f = bench name $ nfIO $ randomRIO (1,1) >>= f

{-# INLINE drainGeneration #-}
drainGeneration :: Monad m => Unfold m a b -> a -> m ()
drainGeneration unf seed = UF.fold FL.drain unf seed

-------------------------------------------------------------------------------
-- Stream generation
-------------------------------------------------------------------------------

{-# INLINE enumerateFromThenIntegral #-}
enumerateFromThenIntegral :: Int -> Int -> IO ()
enumerateFromThenIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromThenIntegral) (start, 1)

#ifdef INSPECTION
inspect $ 'enumerateFromThenIntegral `hasNoType` ''S.Step
inspect $ 'enumerateFromThenIntegral `hasNoType` ''FL.Step
inspect $ 'enumerateFromThenIntegral `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromToIntegral #-}
enumerateFromToIntegral :: Int -> Int -> IO ()
enumerateFromToIntegral size start =
    drainGeneration
    ( UF.supplySecond
      (size + start)
      UF.enumerateFromToIntegral
    ) start

#ifdef INSPECTION
inspect $ 'enumerateFromToIntegral `hasNoType` ''S.Step
inspect $ 'enumerateFromToIntegral `hasNoType` ''FL.Step
inspect $ 'enumerateFromToIntegral `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromIntegral #-}
enumerateFromIntegral :: Int -> Int -> IO ()
enumerateFromIntegral size start =
    drainGeneration (UF.take size UF.enumerateFromIntegral) start

#ifdef INSPECTION
inspect $ 'enumerateFromIntegral `hasNoType` ''S.Step
inspect $ 'enumerateFromIntegral `hasNoType` ''FL.Step
inspect $ 'enumerateFromIntegral `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromStepNum #-}
enumerateFromStepNum :: Int -> Int -> IO ()
enumerateFromStepNum size start =
    drainGeneration (UF.take size (UF.enumerateFromThenNum)) (start, 1)

#ifdef INSPECTION
inspect $ 'enumerateFromStepNum `hasNoType` ''S.Step
inspect $ 'enumerateFromStepNum `hasNoType` ''FL.Step
inspect $ 'enumerateFromStepNum `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromNum #-}
enumerateFromNum :: Int -> Int -> IO ()
enumerateFromNum size start = drainGeneration (UF.take size UF.enumerateFromNum) start

#ifdef INSPECTION
inspect $ 'enumerateFromNum `hasNoType` ''S.Step
inspect $ 'enumerateFromNum `hasNoType` ''FL.Step
inspect $ 'enumerateFromNum `hasNoType` ''SPEC
#endif

{-# INLINE enumerateFromToFractional #-}
enumerateFromToFractional :: Int -> Int -> IO ()
enumerateFromToFractional size start =
    let intToDouble x = (fromInteger (fromIntegral x)) :: Double
     in drainGeneration
            ( UF.supplySecond
              (intToDouble $ start + size)
              UF.enumerateFromToFractional
            )
            (intToDouble start)

#ifdef INSPECTION
inspect $ 'enumerateFromToFractional `hasNoType` ''S.Step
inspect $ 'enumerateFromToFractional `hasNoType` ''FL.Step
inspect $ 'enumerateFromToFractional `hasNoType` ''SPEC
#endif

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
