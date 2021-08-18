-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- | This module contains different stream types and combinators to
-- interconvert between them.

module Streamly.Internal.Data.Stream.IsStream.Types
    (
    -- * Stream Types
    -- ** Serial Streams
      SerialT
    , Serial
    , WSerialT
    , WSerial

    -- ** Speculative Streams
    , AheadT
    , Ahead

    -- ** Asynchronous Streams
    , AsyncT
    , Async
    , WAsyncT
    , WAsync

    -- ** Parallel Streams
    -- | Ahead, Async and WAsync schedule actions concurrently on demand.
    -- Unlike those 'Parallel' streams schedule all actions concurrently
    -- upfront.
    , ParallelT
    , Parallel
    , mkAsync

    -- ** Zipping Streams
    , ZipSerialM
    , ZipSerial
    , ZipAsyncM
    , ZipAsync

    -- * Stream Type Adapters
    , IsStream ()

    , fromSerial
    , fromWSerial
    , fromAsync
    , fromAhead
    , fromWAsync
    , fromParallel
    , fromZipSerial
    , fromZipAsync
    , adapt

    -- * Type Synonyms
    , MonadAsync
    )
where

import Streamly.Internal.Data.Stream.Ahead (AheadT, Ahead, fromAhead)
import Streamly.Internal.Data.Stream.Async
       ( AsyncT, Async, WAsyncT, WAsync, mkAsync, fromAsync
       , fromWAsync)
import Streamly.Internal.Data.Stream.Parallel (ParallelT, Parallel, fromParallel)
import Streamly.Internal.Data.Stream.Serial
       ( SerialT, WSerialT, Serial, WSerial, fromSerial
       , fromWSerial)
import Streamly.Internal.Data.Stream.StreamK (IsStream(), adapt)
import Streamly.Internal.Data.Stream.Zip
       (ZipSerialM, ZipSerial, ZipAsyncM, ZipAsync, fromZipSerial, fromZipAsync)
import Streamly.Internal.Control.Concurrent (MonadAsync)
