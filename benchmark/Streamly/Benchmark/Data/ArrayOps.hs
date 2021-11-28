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
-- DATA_ARRAY_PRIM
-- DATA_ARRAY_PRIM_PINNED

module Streamly.Benchmark.Data.ArrayOps where

import Control.Monad.IO.Class (MonadIO)
import Prelude (Bool, Int, Maybe(..), ($), (+), (.), (==), (>), undefined)

import qualified Prelude as P
import qualified Streamly.Prelude as S

#if !defined(DATA_ARRAY_PRIM) && !defined(DATA_ARRAY_PRIM_PINNED)
#ifdef DEVBUILD
{-
import qualified Data.Foldable as F
-}
#endif
#endif

#if defined(MEMORY_ARRAY)
import qualified GHC.Exts as GHC
import qualified Streamly.Data.Array.Foreign as A
import qualified Streamly.Internal.Data.Array.Foreign as A
type Stream = A.Array
#elif defined(DATA_ARRAY_PRIM)
import qualified Streamly.Internal.Data.Array.Prim as A
type Stream = A.Array
#elif defined(DATA_ARRAY_PRIM_PINNED)
import qualified Streamly.Internal.Data.Array.Prim.Pinned as A
type Stream = A.Array
#elif defined(DATA_ARRAY)
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
-- DATA_ARRAY_PRIM
-- DATA_ARRAY_PRIM_PINNED
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

-- CPP:
{-# INLINE sourceIntFromToFromList #-}
sourceIntFromToFromList :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromToFromList value n = P.return $ A.fromList $ [n..n + value]

-------------------------------------------------------------------------------
-- CPP Common to:
-- MEMORY_ARRAY
-- DATA_ARRAY
-- DATA_ARRAY_PRIM
-- DATA_ARRAY_PRIM_PINNED
-------------------------------------------------------------------------------

-- CPP:
{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromToFromStream value n = S.fold A.write $ S.enumerateFromTo n (n + value)

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
-- DATA_ARRAY_PRIM
-- DATA_ARRAY_PRIM_PINNED
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
-------------------------------------------------------------------------------

-- CPP:
#if !defined(DATA_ARRAY_PRIM) && !defined(DATA_ARRAY_PRIM_PINNED)
{-# INLINE readInstance #-}
readInstance :: P.String -> Stream Int
readInstance str =
    let r = P.reads str
    in case r of
        [(x,"")] -> x
        _ -> P.error "readInstance: no parse"

#ifdef DEVBUILD
{-
{-# INLINE foldableFoldl' #-}
foldableFoldl' :: Stream Int -> Int
foldableFoldl' = F.foldl' (+) 0

{-# INLINE foldableSum #-}
foldableSum :: Stream Int -> Int
foldableSum = P.sum
-}
#endif
#endif

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- CPP Common to:
-- MEMORY_ARRAY
-- DATA_ARRAY
-- DATA_ARRAY_PRIM
-- DATA_ARRAY_PRIM_PINNED
-------------------------------------------------------------------------------

{-# INLINE unfoldReadDrain #-}
unfoldReadDrain :: MonadIO m => Stream Int -> m ()
unfoldReadDrain = S.drain . S.unfold A.read

{-# INLINE toStreamRevDrain #-}
toStreamRevDrain :: MonadIO m => Stream Int -> m ()
toStreamRevDrain = S.drain . A.toStreamRev
