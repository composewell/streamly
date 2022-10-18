-- |
-- Module      : Streamly.Console.Stdio
-- Copyright   : (c) 2021 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Combinators to work with standard input, output and error streams.
--
-- See also: "Streamly.Internal.Console.Stdio"

module Streamly.Console.Stdio
    (
    -- * Unfolds (stdin)
      reader
    , chunkReader

    -- * Write (stdout)
    , write
    , writeChunks

    -- * Write (stderr)
    , writeErr
    , writeErrChunks

    -- * Deprecated
    , read
    , readChunks
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Streamly.Internal.Data.Array.Unboxed.Type (Array)
import Streamly.Internal.Data.Unfold (Unfold)

import Streamly.Internal.Console.Stdio hiding (read, readChunks)
import Prelude hiding (read)

-- Same as 'reader'
--
-- @since 0.8.0
{-# DEPRECATED read "Please use 'reader' instead" #-}
{-# INLINE read #-}
read :: MonadIO m => Unfold m () Word8
read = reader

-- Same as 'chunkReader'
--
-- @since 0.8.0
{-# DEPRECATED readChunks "Please use 'chunkReader' instead" #-}
{-# INLINE readChunks #-}
readChunks :: MonadIO m => Unfold m () (Array Word8)
readChunks = chunkReader
