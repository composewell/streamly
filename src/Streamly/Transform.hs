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
-- and merge/zip the resulting streams.
--
-- @
--
--                    |-------Transform x a--------|
--                    |                            |
-- -----stream m x----|-------Transform x b--------|----stream m f \<$> a \<*> b \<*> c
--                    |                            |
--                    |-------Transform x c--------|
-- @
-- @
--
--                    |-------Transform x a--------|
--                    |                            |
-- -----stream m x----|-------Transform x a--------|----stream m a
--                    |                            |
--                    |-------Transform x a--------|
-- @


module Streamly.Transform
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
