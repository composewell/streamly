-- |
-- Module      : Streamly.Data.Stream.Zip
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Data.Stream.Zip
    (
      ZipStream (..)
    , ZipSerialM
    , ZipSerial
    , zipWithM
    , zipWith
    )
where

import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Stream.Zip

import Prelude hiding (zipWith)

{-# INLINE zipWithM #-}
zipWithM :: Monad m =>
    (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipWithM = zipWithMD

{-# INLINE zipWith #-}
zipWith :: Monad m
    => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith = zipWithD
