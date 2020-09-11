-- |
-- Module      : Streamly.Benchmark.Data.ArrayOps
-- Copyright   : (c) 2020 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- CPP:
-- MEMORY_ARRAY
-- DATA_ARRAY
-- DATA_PRIM_ARRAY
-- DATA_SMALLARRAY

module Streamly.Benchmark.Data.ArrayOps where

import Control.Monad.IO.Class (MonadIO)
import Prelude (Bool, Int, Maybe(..), ($), (+), (.), (==), (>), undefined)

import qualified Prelude as P
import qualified Streamly.Prelude as S

#ifndef DATA_PRIM_ARRAY
#ifdef DEVBUILD
import qualified Data.Foldable as F
#endif
#endif

#ifdef DATA_SMALLARRAY
import qualified Streamly.Internal.Data.SmallArray as A
type Stream = A.SmallArray
#elif defined(MEMORY_ARRAY)
import qualified GHC.Exts as GHC
import qualified Streamly.Data.Array.Storable.Foreign as A
import qualified Streamly.Internal.Data.Array.Storable.Foreign as A
type Stream = A.Array
#elif defined(DATA_PRIM_ARRAY)
import qualified Streamly.Internal.Data.Prim.Array as A
type Stream = A.PrimArray
#else
import qualified Streamly.Internal.Data.Array as A
type Stream = A.Array
#endif

-------------------------------------------------------------------------------
-- Benchmark ops
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Stream generation and elimination
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- CPP Common to:
-- MEMORY_ARRAY
-- DATA_ARRAY
-- DATA_PRIM_ARRAY
-- DATA_SMALLARRAY
-------------------------------------------------------------------------------

{-# INLINE sourceUnfoldr #-}
sourceUnfoldr :: MonadIO m => Int -> Int -> m (Stream Int)
sourceUnfoldr value n = S.fold (A.writeN value) $ S.unfoldr step n
    where
    step cnt =
        if cnt > n + value
        then Nothing
        else (Just (cnt, cnt + 1))

{-# INLINE sourceIntFromTo #-}
sourceIntFromTo :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromTo value n = S.fold (A.writeN value) $ S.enumerateFromTo n (n + value)

{-# INLINE sourceFromList #-}
sourceFromList :: MonadIO m => Int -> Int -> m (Stream Int)
sourceFromList value n = S.fold (A.writeN value) $ S.fromList [n..n+value]

-- Different defination of sourceIntFromToFromList for DATA_SMALLARRAY
-- CPP:
{-# INLINE sourceIntFromToFromList #-}
sourceIntFromToFromList :: MonadIO m => Int -> Int -> m (Stream Int)
#ifndef DATA_SMALLARRAY
sourceIntFromToFromList value n = P.return $ A.fromList $ [n..n + value]
#else
sourceIntFromToFromList value n = P.return $ A.fromListN value $ [n..n + value]
#endif

-------------------------------------------------------------------------------
-- CPP Common to:
-- MEMORY_ARRAY
-- DATA_ARRAY
-- DATA_PRIM_ARRAY
-------------------------------------------------------------------------------

-- CPP:
#ifndef DATA_SMALLARRAY
{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromToFromStream value n = S.fold A.write $ S.enumerateFromTo n (n + value)
#endif

-------------------------------------------------------------------------------
-- CPP Common to:
-- MEMORY_ARRAY
-------------------------------------------------------------------------------

-- CPP:
#ifdef MEMORY_ARRAY
{-# INLINE sourceIsList #-}
sourceIsList :: Int -> Int -> Stream Int
sourceIsList value n = GHC.fromList [n..n+value]

{-# INLINE sourceIsString #-}
sourceIsString :: Int -> Int -> Stream P.Char
sourceIsString value n = GHC.fromString (P.replicate (n + value) 'a')
#endif

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- CPP Common to:
-- MEMORY_ARRAY
-- DATA_ARRAY
-- DATA_PRIM_ARRAY
-- DATA_SMALLARRAY
-------------------------------------------------------------------------------

{-# INLINE composeN #-}
composeN :: P.Monad m
    => Int -> (Stream Int -> m (Stream Int)) -> Stream Int -> m (Stream Int)
composeN n f x =
    case n of
        1 -> f x
        2 -> f x P.>>= f
        3 -> f x P.>>= f P.>>= f
        4 -> f x P.>>= f P.>>= f P.>>= f
        _ -> undefined

{-# INLINE scanl' #-}
{-# INLINE scanl1' #-}
{-# INLINE map #-}

scanl' , scanl1', map
    :: MonadIO m => Int -> Int -> Stream Int -> m (Stream Int)


{-# INLINE onArray #-}
onArray
    :: MonadIO m => Int -> (S.SerialT m Int -> S.SerialT m Int)
    -> Stream Int
    -> m (Stream Int)
onArray value f arr = S.fold (A.writeN value) $ f $ (S.unfold A.read arr)

scanl'  value n = composeN n $ onArray value $ S.scanl' (+) 0
scanl1' value n = composeN n $ onArray value $ S.scanl1' (+)
map     value n = composeN n $ onArray value $ S.map (+1)
-- map           n = composeN n $ A.map (+1)

{-# INLINE eqInstance #-}
eqInstance :: Stream Int -> Bool
eqInstance src = src == src

{-# INLINE eqInstanceNotEq #-}
eqInstanceNotEq :: Stream Int -> Bool
eqInstanceNotEq src = src P./= src

{-# INLINE ordInstance #-}
ordInstance :: Stream Int -> Bool
ordInstance src = src P.< src

{-# INLINE ordInstanceMin #-}
ordInstanceMin :: Stream Int -> Stream Int
ordInstanceMin src = P.min src src

{-# INLINE showInstance #-}
showInstance :: Stream Int -> P.String
showInstance src = P.show src

{-# INLINE pureFoldl' #-}
pureFoldl' :: MonadIO m => Stream Int -> m Int
pureFoldl' = S.foldl' (+) 0 . S.unfold A.read

-------------------------------------------------------------------------------
-- CPP Common to:
-- MEMORY_ARRAY
-- DATA_ARRAY
-- DATA_SMALLARRAY
-------------------------------------------------------------------------------

-- CPP:
#ifndef DATA_PRIM_ARRAY
{-# INLINE readInstance #-}
readInstance :: P.String -> Stream Int
readInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

#ifdef DEVBUILD
{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Stream Int -> Int
foldableFoldl' = F.foldl' (+) 0

{-# INLINE foldableSum #-}
foldableSum :: Stream Int -> Int
foldableSum = P.sum
#endif
#endif

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- CPP Common to:
-- MEMORY_ARRAY
-- DATA_ARRAY
-- DATA_PRIM_ARRAY
-- DATA_SMALLARRAY
-------------------------------------------------------------------------------

{-# INLINE unfoldReadDrain #-}
unfoldReadDrain :: MonadIO m => Stream Int -> m ()
unfoldReadDrain = S.drain . S.unfold A.read

{-# INLINE toStreamRevDrain #-}
toStreamRevDrain :: MonadIO m => Stream Int -> m ()
toStreamRevDrain = S.drain . A.toStreamRev
