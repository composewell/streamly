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

    , serially
    , wSerially
    , asyncly
    , aheadly
    , wAsyncly
    , parallely
    , zipSerially
    , zipAsyncly
    , adapt

    -- * Type Synonyms
    , MonadAsync
    )
where

import Streamly.Internal.Data.Stream.Ahead (AheadT, Ahead, aheadly)
import Streamly.Internal.Data.Stream.Async
       ( AsyncT, Async, WAsyncT, WAsync, mkAsync, asyncly
       , wAsyncly)
import Streamly.Internal.Data.Stream.Parallel (ParallelT, Parallel, parallely)
import Streamly.Internal.Data.Stream.Serial
       ( SerialT, WSerialT, Serial, WSerial, serially
       , wSerially)
import Streamly.Internal.Data.Stream.StreamK (IsStream(), adapt)
import Streamly.Internal.Data.Stream.Zip
       (ZipSerialM, ZipSerial, ZipAsyncM, ZipAsync, zipSerially, zipAsyncly)
import Streamly.Internal.Data.SVar (MonadAsync)
