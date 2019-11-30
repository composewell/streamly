{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Data.Array
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Data.Array
    ( Array

    -- * Construction

    -- Stream Folds
    , fromStreamN

    -- MonadicAPIs
    , A.writeN

    -- * Elimination

    , toStream
    , toStreamRev
    , read

    -- * Random Access
    , length

    -- * Folding Arrays
    , streamFold
    , fold
    )
where

import Prelude hiding (length, read)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import GHC.Base (Int(..))
import qualified GHC.Exts as Exts

import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream)
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Unfold.Types (Unfold(..))
import Streamly.Internal.Data.Array (Array(..))

import qualified Streamly.Internal.Data.Array as A
import qualified Streamly.Streams.Prelude as P
import qualified Streamly.Streams.StreamD as D

{-# INLINE fromStreamN #-}
fromStreamN :: MonadIO m => Int -> SerialT m a -> m (Array a)
fromStreamN n m = do
    when (n < 0) $ error "fromStreamN: negative write count specified"
    A.fromStreamDN n $ D.toStreamD m

{-# INLINE_EARLY toStream #-}
toStream :: (Monad m, IsStream t) => Array a -> t m a
toStream = D.fromStreamD . A.toStreamD

{-# INLINE_EARLY toStreamRev #-}
toStreamRev :: (Monad m, IsStream t) => Array a -> t m a
toStreamRev = D.fromStreamD . A.toStreamDRev

{-# INLINE length #-}
length :: Array a -> Int
length Array {..} = aLen

{-# INLINE fold #-}
fold :: forall m a b . Monad m => Fold m a b -> Array a -> m b
fold f arr = P.runFold f (toStream arr :: SerialT m a)

{-# INLINE streamFold #-}
streamFold :: Monad m => (SerialT m a -> m b) -> Array a -> m b
streamFold f arr = f (toStream arr)

{-# INLINE_NORMAL read #-}
read :: Monad m => Unfold m (Array a) a
read = Unfold step inject
  where
    inject arr = return (arr, 0)
    step (Array {..}, i)
        | i == aLen = return D.Stop
    step (arr, (I# i)) =
        return $
        case Exts.indexArray# (aA arr) i of
            (# x #) -> D.Yield x (arr, I# i + 1)
