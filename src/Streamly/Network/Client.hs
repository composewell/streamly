-- |
-- Module      : Streamly.Network.Client
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Combinators to build network clients.
--
-- > import qualified Streamly.Network.Socket as Client
--

module Streamly.Network.Client
    (
    -- * Interact
    -- | Socket based reads and writes.
      withConnection

    -- * Source
    , read
    -- , readUtf8
    -- , readLines
    -- , readFrames
    -- , readByChunks

    -- -- * Array Read
    -- , readArrayUpto
    -- , readArrayOf

    -- , readArraysUpto
    -- , readArraysOf
    -- , readArrays

    -- * Sink
    , write
    -- , writeUtf8
    -- , writeUtf8ByLines
    -- , writeByFrames
    -- , writeInChunksOf

    -- -- * Array Write
    -- , writeArray
    , writeArrays
    )
where

import Streamly.Internal.Network.Client
import Prelude hiding (read)
