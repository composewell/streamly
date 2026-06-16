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

import Array.Type
    (typeCommonBenchmarks, benchIO, withRandomIntIO, withArray, withStream)

#if __GLASGOW_HASKELL__ >= 810
type Arr :: Type -> Type
#endif
type Arr = A.Array

#include "Streamly/Benchmark/Data/Array/Common.hs"

-------------------------------------------------------------------------------
-- Bench Ops
-------------------------------------------------------------------------------

{-# INLINE sourceIsList #-}
sourceIsList :: Int -> IO (Arr Int)
sourceIsList value = withRandomIntIO $ \n -> return $! GHC.fromList [n..n+value]

{-# INLINE sourceIsString #-}
sourceIsString :: Int -> IO (Arr P.Char)
sourceIsString value = withRandomIntIO $ \n ->
    return $! GHC.fromString (P.replicate (n + value) 'a')

{-# INLINE toListLength #-}
toListLength :: Int -> IO Int
toListLength value = withArray value $ \arr -> return $! length (GHC.toList arr)

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array"

defStreamSize :: Int
defStreamSize = defaultStreamSize

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
    typeCommonBenchmarks size
    ++ commonBenchmarks size
    ++
    -- Before adding any benchmarks here check if they can be added to
    -- typeCommonBenchmarks (Array.Type source module common with
    -- Array.Generic) or commonBenchmarks (Array module common with
    -- Array.Generic) above.
      [ (SpaceO_1, benchIO "writeN . IsList.fromList" $ sourceIsList size)
      , (SpaceO_1, benchIO "writeN . IsString.fromString" $ sourceIsString size)
      , (SpaceO_1, benchIO "length . IsList.toList" $ toListLength size)
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
