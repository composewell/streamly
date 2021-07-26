{-# LANGUAGE UnboxedTuples #-}

-- |
-- Module      : Streamly.Internal.System.IO
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.System.IO
    ( defaultChunkSize
    , mkChunkSize
    , mkChunkSizeKB
    , unsafeInlineIO
    )

where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Foreign.Storable (Storable(sizeOf))
import GHC.Base (realWorld#)
import GHC.IO (IO(IO))

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

{-# INLINE unsafeInlineIO #-}
unsafeInlineIO :: IO a -> a
unsafeInlineIO (IO m) = case m realWorld# of (# _, r #) -> r

-- | GHC memory management allocation header overhead
allocOverhead :: Int
allocOverhead = 2 * sizeOf (undefined :: Int)

mkChunkSize :: Int -> Int
mkChunkSize n = let size = n - allocOverhead in max size 0

mkChunkSizeKB :: Int -> Int
mkChunkSizeKB n = mkChunkSize (n * k)
   where k = 1024

-- | Default maximum buffer size in bytes, for reading from and writing to IO
-- devices, the value is 32KB minus GHC allocation overhead, which is a few
-- bytes, so that the actual allocation is 32KB.
defaultChunkSize :: Int
defaultChunkSize = mkChunkSizeKB 32
