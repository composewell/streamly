{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnboxedTuples #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Network.Client
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
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
    -- , writeByChunks

    -- -- * Array Write
    -- , writeArray
    , writeArrays
    )
where

import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Word (Word8)
import Network.Socket
       (Socket, PortNumber, Family(..), SockAddr(..), SocketType(..),
        socket, connect, defaultProtocol)
import Prelude hiding (read)

import qualified Network.Socket as Net

import Streamly (MonadAsync)
import Streamly.Mem.Array.Types (Array(..))
import Streamly.Streams.Serial (SerialT)
import Streamly.Streams.StreamK.Type (IsStream)

import qualified Streamly.Prelude as S
import qualified Streamly.Network.Socket as SK

-------------------------------------------------------------------------------
-- Connect
-------------------------------------------------------------------------------

openConnection :: (Word8, Word8, Word8, Word8) -> PortNumber -> IO Socket
openConnection addr port = do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock $ SockAddrInet port (Net.tupleToHostAddress addr)
    return sock

-- | @'withConnection' addr port act@ opens a connection to the specified IPv4
-- host address and port and passes the resulting socket handle to the
-- computation @act@.  The handle will be closed on exit from 'withConnection',
-- whether by normal termination or by raising an exception.  If closing the
-- handle raises an exception, then this exception will be raised by
-- 'withConnection' rather than any exception raised by 'act'.
--
-- @since 0.7.0
withConnection :: (IsStream t, MonadCatch m, MonadIO m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> (Socket -> t m a) -> t m a
withConnection addr port =
    S.bracket (liftIO $ openConnection addr port) (liftIO . Net.close)

-------------------------------------------------------------------------------
-- Read Addr to Stream
-------------------------------------------------------------------------------

-- | Read a stream from the supplied IPv4 host address and port number.
--
-- @since 0.7.0
read :: (IsStream t, MonadCatch m, MonadIO m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> t m Word8
read addr port = withConnection addr port SK.read

-------------------------------------------------------------------------------
-- Writing
-------------------------------------------------------------------------------

-- | Write a stream to the supplied IPv4 host address and port number.
--
-- @since 0.7.0
write :: (MonadCatch m, MonadIO m)
    => (Word8, Word8, Word8, Word8) -> PortNumber -> SerialT m Word8 -> m ()
write addr port xs =
    S.drain $ withConnection addr port (\sk -> S.yieldM $ SK.write sk xs)

-- | Write a stream of arrays to the supplied IPv4 host address and port
-- number.
--
-- @since 0.7.0
writeArrays
    :: (MonadCatch m, MonadAsync m)
    => (Word8, Word8, Word8, Word8)
    -> PortNumber
    -> SerialT m (Array Word8)
    -> m ()
writeArrays addr port xs =
    S.drain $ withConnection addr port (\sk -> S.yieldM $ SK.writeArrays sk xs)
