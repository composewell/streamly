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
-- DATA_ARRAY

module Streamly.Benchmark.Data.ArrayOps where

import Control.Monad.IO.Class (MonadIO)
import Prelude (Bool, Int, Maybe(..), ($), (+), (.), (==), (>), undefined)

import qualified Prelude as P
import qualified Streamly.Prelude as S

#ifdef DEVBUILD
{-
import qualified Data.Foldable as F
-}
#endif

#if defined(DATA_ARRAY)
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
-- DATA_ARRAY
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
-- DATA_ARRAY
-------------------------------------------------------------------------------

-- CPP:
{-# INLINE sourceIntFromToFromStream #-}
sourceIntFromToFromStream :: MonadIO m => Int -> Int -> m (Stream Int)
sourceIntFromToFromStream value n = S.fold A.write $ S.enumerateFromTo n (n + value)

-------------------------------------------------------------------------------
-- Transformation
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- CPP Common to:
-- DATA_ARRAY
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
-- DATA_ARRAY
-------------------------------------------------------------------------------

-- CPP:
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

-------------------------------------------------------------------------------
-- Elimination
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- CPP Common to:
-- DATA_ARRAY
-------------------------------------------------------------------------------

{-# INLINE unfoldReadDrain #-}
unfoldReadDrain :: MonadIO m => Stream Int -> m ()
unfoldReadDrain = S.drain . S.unfold A.read

{-# INLINE toStreamRevDrain #-}
toStreamRevDrain :: MonadIO m => Stream Int -> m ()
toStreamRevDrain = S.drain . A.toStreamRev
