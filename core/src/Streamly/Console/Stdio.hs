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
    )
where

import Streamly.Internal.Console.Stdio
