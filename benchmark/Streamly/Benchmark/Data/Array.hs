{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE CPP #-}

#include "Streamly/Benchmark/Data/Array/CommonImports.hs"


#if __GLASGOW_HASKELL__ >= 810
import Data.Kind (Type)
#endif

import qualified Streamly.Internal.Data.Array as IA
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

{-# INLINE parseInstance #-}
parseInstance :: P.String -> Arr Int
parseInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "parseInstance: no parse"

{-# INLINE readInstance #-}
readInstance :: Int -> IO (Arr Int)
readInstance value = withRandomIntIO $ \n ->
    let testStr = "fromList " ++ show [n..n+value]
    in return $! parseInstance testStr

{-# INLINE sourceIsList #-}
sourceIsList :: Int -> IO (Arr Int)
sourceIsList value = withRandomIntIO $ \n -> return $! GHC.fromList [n..n+value]

{-# INLINE sourceIsString #-}
sourceIsString :: Int -> IO (Arr P.Char)
sourceIsString value = withRandomIntIO $ \n ->
    return $! GHC.fromString (P.replicate (n + value) 'a')

{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: Int -> IO (Arr Int)
sourceIntFromToFromStream value = withRandomIntIO $ \n ->
    S.fold A.create $ S.enumerateFromTo n (n + value)

{-# INLINE toListLength #-}
toListLength :: Int -> IO Int
toListLength value = withArray value $ \arr -> return $! length (GHC.toList arr)

{-# INLINE createOfLast1 #-}
createOfLast1 :: Int -> IO (Arr Int)
createOfLast1 value = withStream value (S.fold (IA.createOfLast 1))

{-# INLINE createOfLast10 #-}
createOfLast10 :: Int -> IO (Arr Int)
createOfLast10 value = withStream value (S.fold (IA.createOfLast 10))

{-# INLINE createOfLastMax #-}
createOfLastMax :: Int -> IO (Arr Int)
createOfLastMax value = withStream value (S.fold (IA.createOfLast (value + 1)))

-------------------------------------------------------------------------------
-- Bench groups
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Data.Array"

defStreamSize :: Int
defStreamSize = defaultStreamSize

benchmarks :: Int -> [(SpaceComplexity, Benchmark)]
benchmarks size =
      [ (SpaceO_1, benchIO "write . intFromTo" $ sourceIntFromToFromStream size)
      , (SpaceO_1, benchIO "read" $ readInstance size)
      , (SpaceO_1, benchIO "writeN . IsList.fromList" $ sourceIsList size)
      , (SpaceO_1, benchIO "writeN . IsString.fromString" $ sourceIsString size)

      , (SpaceO_1, benchIO "length . IsList.toList" $ toListLength size)
      , (SpaceO_1, benchIO "createOfLast.1" $ createOfLast1 size)
      , (SpaceO_1, benchIO "createOfLast.10" $ createOfLast10 size)

      , (HeapO_n, benchIO "createOfLast.Max" $ createOfLastMax size)
      ]
    ++ typeCommonBenchmarks size
    ++ commonBenchmarks size

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
