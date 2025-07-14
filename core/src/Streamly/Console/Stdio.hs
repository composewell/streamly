-- |
-- Module      : Streamly.Console.Stdio
-- Copyright   : (c) 2021 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- Combinators to work with standard input, output, and error streams. This
-- module supports reading and writing binary data or UTF-8 encoded text only.
-- However, it is possible to use specific encoders and decoders to implement
-- other encodings.
--
-- These streaming APIs use the stdin and stdout handles for reading from and
-- writing to the console. The reads and writes are buffered, meaning each
-- stream has its own buffer. Be cautious when switching between these APIs and
-- handle-based APIs (e.g., readChars, getLine), between different stream APIs
-- (e.g., readChars, readChunks), or even between different calls to the same
-- API (e.g., readChars, readChars). If you switch from one stream to another,
-- you should drain the first stream completely if you care about preserving
-- any buffered data.
--
-- See also: "Streamly.Internal.Console.Stdio"

-- XXX put examples of repeatM getLine from stream module
-- XXX put examples of using parseBreak or foldBreak.

module Streamly.Console.Stdio
    (
    -- * Streams (stdin)
      read
    , readChars
    , readChunks

    -- * Streams (srdout)
    , putChunks

    -- * Unfolds (stdin)
    , reader
    , chunkReader

    -- * Write (stdout)
    , write
    , writeChunks

    -- * Write (stderr)
    , writeErr
    , writeErrChunks
    )
where

import Prelude hiding (read)
import Streamly.Internal.Console.Stdio
