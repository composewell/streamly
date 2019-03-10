-- |
-- Module      : Streamly.Transform
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Make copies of a stream and transform each copy using a different transform
-- and merge/zip the resulting streams. We should be able to apply different
-- folds to each of the resulting stream. We should be able to merge the
-- resulting stream in all the different ways as we can merge the producer
-- streams.
--
-- This essentially gives us a tool to split followed by a series of stateful
-- transforms followed by merge/foldmerge streams. With just folds we can
-- transform the splits before folding but we cannot continue a composition of
-- stateful transforms.
--
-- The way Foldl gives us a tool to apply a fold to a split, the same way a
-- Scanl gives us a tool to apply a scan to a split before we merge it back.
-- This is more general than a Foldl and subsumes Foldl but is more complex to
-- use than a Foldl, just like a Foldl is more complex to use than a Sink.
--
--
-- @
--
--                    |-------Scanl x a--------|
--                    |                        |
-- -----stream m x----|-------Scanl x b--------|----stream m f \<$> a \<*> b \<*> c
--                    |                        |
--                    |-------Scanl x c--------|
-- @
-- @
--
--                    |-------Scanl x a--------|
--                    |                        |
-- -----stream m x----|-------Scanl x a--------|----stream m a
--                    |                        |
--                    |-------Scanl x a--------|
-- @


module Streamly.Scanl
    (
    )
where

import Streamly.Foldl.Types (Foldl(..))
import Streamly.Transform.Types ()
import Streamly.Streams.Serial (SerialT)

import qualified Streamly.Streams.Prelude as P

------------------------------------------------------------------------------
-- Scanning monadic streams
------------------------------------------------------------------------------

-- We can have scan versions of many of the fold APIs.
-- unzipM :: Monad m => (a -> m (b,c)) -> Scan m b x -> Scan m c y -> t m (x,y)
