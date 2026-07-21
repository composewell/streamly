{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}

#include "Streamly/Benchmark/Data/Array/CommonImports.hs"


#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif

import qualified GHC.Exts as GHC

import qualified Array.Stream as ArrayStream

-- import qualified Streamly.Data.Array as A
import qualified Streamly.Internal.Data.Array as A

-- Imported for the types named in the fusion-plugin annotations below and in
-- the included Array/Common.hs.
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.MutArray as MutArray
import qualified Streamly.Internal.Data.RingArray as RingArray
import qualified Streamly.Internal.Data.SVar.Type as SVar
import GHC.Classes (IP)
import GHC.Exts (SPEC)
import GHC.Stack (CallStack, SrcLoc)

import Array.Type
    (typeCommonBenchmarks, benchIO, withArray, withStream)

#if __GLASGOW_HASKELL__ >= 810
type Arr :: Type -> Type
#endif
type Arr = A.Array

-- Selects the unboxed-array variant of the fusion-plugin annotations in the
-- shared include below.
#define ARRAY_UNBOXED
#include "Streamly/Benchmark/Data/Array/Common.hs"

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# ANN fromList_IsList (PermitPatternMatches
    [ ''MutArray.MutArray, ''[], ''Int, ''SVar.State, ''A.Array, ''IO
    ]) #-}
{-# ANN fromList_IsList (PermitConstructions
    [ ''MutArray.MutArray, ''SVar.State, ''Maybe, ''Bool, ''Int, ''[]
    ]) #-}
{-# ANN fromList_IsList (PermitTypeClasses []) #-}
{-# NOINLINE fromList_IsList #-}
fromList_IsList :: Int -> Int -> IO (Arr Int)
fromList_IsList value n = return $! GHC.fromList [n..n+value]

{-# ANN fromString_IsString (PermitPatternMatches
    [ ''MutArray.MutArray, ''[], ''Char, ''Int, ''SVar.State, ''A.Array
    , ''IO
    ]) #-}
{-# ANN fromString_IsString (PermitConstructions
    [ ''MutArray.MutArray, ''SVar.State, ''Maybe, ''Bool, ''Int, ''[]
    , ''Char
    ]) #-}
{-# ANN fromString_IsString (PermitTypeClasses []) #-}
{-# NOINLINE fromString_IsString #-}
fromString_IsString :: Int -> Int -> IO (Arr P.Char)
fromString_IsString value n =
    return $! GHC.fromString (P.replicate (n + value) 'a')

{-# ANN toList_length_IsList (PermitPatternMatches [''Int, ''IO]) #-}
{-# ANN toList_length_IsList (PermitConstructions []) #-}
{-# ANN toList_length_IsList (PermitTypeClasses []) #-}
{-# NOINLINE toList_length_IsList #-}
toList_length_IsList :: Int -> Int -> IO Int
toList_length_IsList value =
    withArray value $ \arr -> return $! P.length (GHC.toList arr)

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array"

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
    ++
    -- Before adding any benchmarks here check if they can be added to
    -- typeCommonBenchmarks (Array.Type source module common with
    -- Array.Generic) or commonBenchmarks (Array module common with
    -- Array.Generic) above.
      [ (SpaceO_1, benchIO "fromList_IsList" $ fromList_IsList size)
      , (SpaceO_1, benchIO "fromString_IsString" $ fromString_IsString size)
      , (SpaceO_1, benchIO "toList_length_IsList"
            $ toList_length_IsList size)
      ]

main :: IO ()
main = runWithCLIOptsEnv defStreamSize ArrayStream.alloc allBenchmarks

    where

    allBenchmarks arrays size =
        let allBenches = benchmarks size ++ ArrayStream.benchmarks arrays size
            get x = fmap snd $ filter ((==) x . fst) allBenches
            o_1_space = get SpaceO_1
            o_n_heap = get HeapO_n
            o_n_space = get SpaceO_n
        in
        [ bgroup (o_1_space_prefix moduleName) o_1_space
        , bgroup (o_n_heap_prefix moduleName) o_n_heap
        , bgroup (o_n_space_prefix moduleName) o_n_space
        ]
