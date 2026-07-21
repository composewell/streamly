{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}

#include "Streamly/Benchmark/Data/Array/CommonImports.hs"

import Control.DeepSeq (NFData(..))
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Stream.Common as P

import qualified Streamly.Internal.Data.Array.Generic as A

-- Imported for the types named in the fusion-plugin annotations in the
-- included Array/TypeCommon.hs and Array/Common.hs.
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.MutArray.Generic as MutArray
import GHC.Classes (IP)
import GHC.Exts (SPEC)
import GHC.Stack (CallStack, SrcLoc)

-- Select the array type in the common includes below
type Arr = A.Array

-- Selects the generic-array variant of the fusion-plugin annotations in the
-- shared includes below.
#define ARRAY_GENERIC
#include "Streamly/Benchmark/Data/Array/TypeCommon.hs"

#include "Streamly/Benchmark/Data/Array/Common.hs"

instance NFData a => NFData (A.Array a) where
    {-# INLINE rnf #-}
    rnf = A.foldl' (\_ x -> rnf x) ()

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# ANN fromListN (PermitPatternMatches [''Int]) #-}
{-# ANN fromListN (PermitConstructions [''[], ''Int]) #-}
{-# ANN fromListN (PermitTypeClasses []) #-}
{-# NOINLINE fromListN #-}
fromListN :: Int -> Int -> IO (Arr Int)
fromListN value n =
    P.return $ A.fromListN value [n..n + value]

#ifdef DEVBUILD
{-
{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Arr Int -> Int
foldableFoldl' = F.foldl' (+) 0

{-# INLINE foldableSum #-}
foldableSum :: Arr Int -> Int
foldableSum = P.sum
-}
#endif

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array.Generic"

defStreamSize :: Int
defStreamSize = defaultStreamSize

-- Note: Name each benchmark (and its IO action) after the exported function it
-- benchmarks, using the format functionName_dimension1_dimension2..., where
-- the dimensions are optional variants/type specializations. Keep extra info
-- in parenthetical notes in the description.
benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    typeCommonBenchmarks size
    ++ commonBenchmarks size
    -- Before adding any benchmarks here check if they can be added to
    -- typeCommonBenchmarks (Array.Type source module common with
    -- Array.Generic) or commonBenchmarks (Array module common with
    -- Array.Generic) above.

main :: IO ()
main = runWithCLIOpts defStreamSize allBenchmarks

    where

    allBenchmarks size =
        let allBenches = benchmarks size
            get x = fmap snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_heap = get HeapO_n
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
