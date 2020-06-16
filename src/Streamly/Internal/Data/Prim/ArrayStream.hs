{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Prim.ArrayStream
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to efficiently manipulate streams of arrays.
--
module Streamly.Internal.Data.Prim.ArrayStream
    (
    -- * Creation
      arraysOf

    -- * Flattening to elements
    , concat
    , concatRev
    , interpose
    , interposeSuffix
    , intercalateSuffix

    -- * Transformation
    , splitOn
    , splitOnSuffix
    , compact -- compact

    -- * Elimination
    , toArray
    )
where

import Streamly.Internal.Data.Prim.Array.Types (Array(..), length)
import qualified Streamly.Internal.Data.Prim.Array as A
import qualified Streamly.Internal.Data.Prim.Array.Types as A
import qualified Streamly.Internal.Data.Prim.Mutable.Array.Types as MA

#include "prim-array-stream.hs"

{-
-- THIS ISNT POSSIBLE AS OUR ARRAYS ARE ALWAYS COMPLETE
-- XXX Both of these implementations of splicing seem to perform equally well.
-- We need to perform benchmarks over a range of sizes though.

-- CAUTION! length must more than equal to lengths of all the arrays in the
-- stream.
{-# INLINE spliceArraysLenUnsafe #-}
spliceArraysLenUnsafe :: (PrimMonad m, Prim a)
    => Int -> SerialT m (Array a) -> m (Array a)
spliceArraysLenUnsafe len buffered = do
    arr <- MA.newArray len
    end <- S.foldlM' writeArr (aEnd arr) buffered
    return $ arr {aEnd = end}

    where

    writeArr dst Array{..} =
        liftIO $ withForeignPtr aStart $ \src -> do
                        let count = aEnd `minusPtr` src
                        A.memcpy (castPtr dst) (castPtr src) count
                        return $ dst `plusPtr` count

{-# INLINE _spliceArraysBuffered #-}
_spliceArraysBuffered :: (PrimMonad m, Prim a)
    => SerialT m (Array a) -> m (Array a)
_spliceArraysBuffered s = do
    buffered <- P.foldr S.cons S.nil s
    len <- S.sum (S.map length buffered)
    spliceArraysLenUnsafe len s
-}
