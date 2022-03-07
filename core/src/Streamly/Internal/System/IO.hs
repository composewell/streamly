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
    , arrayPayloadSize
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

-- | Returns the heap allocation overhead for allocating a byte array. Each
-- heap object contains a one word header. Byte arrays contain the size of the
-- array after the header.
--
-- See https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#arrays
--
byteArrayOverhead :: Int
byteArrayOverhead = 2 * sizeOf (undefined :: Word)

-- | When we allocate a byte array of size @k@ the allocator actually allocates
-- memory of size @k + byteArrayOverhead@. @arrayPayloadSize n@ returns the
-- size of the array in bytes that would result in an allocation of @n@ bytes.
--
arrayPayloadSize :: Int -> Int
arrayPayloadSize n = let size = n - byteArrayOverhead in max size 0

-- | Default maximum buffer size in bytes, for reading from and writing to IO
-- devices, the value is 32KB minus GHC allocation overhead, which is a few
-- bytes, so that the actual allocation is 32KB.
defaultChunkSize :: Int
defaultChunkSize = arrayPayloadSize (32 * 1024)
