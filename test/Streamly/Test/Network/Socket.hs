{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Test.Network.Socket
-- Copyright   : (c) 2020 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Main (main) where

import Control.Concurrent (threadDelay, killThread, forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Word (Word8)
import Network.Socket (Socket, PortNumber)
import Streamly.Internal.Control.Monad (discard)
import Streamly.Internal.System.IO (defaultChunkSize)
import Streamly.Internal.Data.Stream (Stream)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Internal.Network.Socket as Socket
import qualified Streamly.Internal.Unicode.Stream as Unicode

import Test.Hspec
import Test.Hspec.QuickCheck

------------------------------------------------------------------------------
-- Command Handlers
------------------------------------------------------------------------------
testData :: String
testData = "Test data 1234567891012131415!@#$%^&*()`~ABCD"

testDataSource :: String
testDataSource = concat $ replicate 1000 testData

------------------------------------------------------------------------------
-- Parse and handle commands on a socket
------------------------------------------------------------------------------

handlerChunksWithBuffer :: Socket -> IO ()
handlerChunksWithBuffer sk =
          Stream.unfold Socket.chunkReaderWith (100, sk)
        & Stream.fold (Socket.writeChunks sk)
        & discard

handlerChunks :: Socket -> IO ()
handlerChunks sk =
          Stream.unfold Socket.chunkReader sk
        & Stream.fold (Socket.writeChunks sk)
        & discard

handlerwithbuffer :: Socket -> IO ()
handlerwithbuffer sk =
          Stream.unfold Socket.readerWith (100, sk)
        & Stream.fold (Socket.writeWith 100 sk)
        & discard

handlerRW :: Socket -> IO ()
handlerRW sk =
          Stream.unfold Socket.reader sk
        & Stream.fold (Socket.write sk)
        & discard

------------------------------------------------------------------------------
-- Accept connections and handle connected sockets
------------------------------------------------------------------------------

-- Ideally we should choose an available port automatically.  However, to test
-- the APIs that do not allow choosing a port automatically we still need to
-- use a hardcoded port or search an available port on the system.
--
-- This is unreliable and the test may fail on systems where this port is not
-- available. We choose a higher port number so that the likelihood of it being
-- available is more.
--
-- Also, we cannot run the test after running it once until the timeout.
basePort :: PortNumber
basePort = 64000

server :: PortNumber -> MVar () -> (Socket -> IO ()) -> IO ()
server port sem handler = do
    putMVar sem ()
    Stream.unfold TCP.acceptorOnPort port
        & Stream.mapM (Socket.forSocketM handler)
        & Stream.fold Fold.drain

remoteAddr :: (Word8,Word8,Word8,Word8)
remoteAddr = (127, 0, 0, 1)

sender :: PortNumber -> MVar () -> Stream IO Char
sender port sem = Stream.before action stream

    where

    action = liftIO (takeMVar sem >> threadDelay 1000000) -- wait for server

    stream =
        Stream.replicate 1000 testData                     -- Stream IO String
            & Stream.concatMap Stream.fromList             -- Stream IO Char
            & Unicode.encodeLatin1                         -- Stream IO Word8
            & TCP.pipeBytes remoteAddr port                -- Stream IO Word8
            & Unicode.decodeLatin1                         -- Stream IO Char

execute :: PortNumber -> Int -> (Socket -> IO ()) -> IO (Stream IO Char)
execute port size handler = do
    sem <- newEmptyMVar
    tid <- forkIO $ server port sem handler
    let lst = sender port sem
                & Stream.take size
                & Stream.finally (killThread tid)
    return lst

validateWith :: Property
validateWith = monadicIO $ do
    res <- run $ do
        ls2 <- execute basePort 45000 handlerwithbuffer
        Stream.eqBy (==) (Stream.fromList testDataSource) ls2
    assert res

validateRW :: Property
validateRW = monadicIO $ do
    res <- run $ do
        ls2 <- execute (basePort + 1) defaultChunkSize handlerRW
        let dataChunk = take defaultChunkSize testDataSource
        Stream.eqBy (==) (Stream.fromList dataChunk) ls2
    assert res

validateChunks :: Property
validateChunks = monadicIO $ do
    res <- run $ do
        ls2 <- execute (basePort + 2) 45000 handlerChunks
        Stream.eqBy (==) (Stream.fromList testDataSource) ls2
    assert res

validateChunksWith :: Property
validateChunksWith = monadicIO $ do
    res <- run $ do
        ls2 <- execute (basePort + 3) 45000 handlerChunksWithBuffer
        Stream.eqBy (==) (Stream.fromList testDataSource) ls2
    assert res

moduleName :: String
moduleName = "Network.Socket"

main :: IO ()
main = hspec $ do
    modifyMaxSuccess (const 1) $ do
      describe moduleName $ do
        describe "Read/Write" $ do
            prop "read/write" validateRW
            prop "readWith/writeWith" validateWith
            prop "readChunks/writeChunks" validateChunks
            prop "readChunksWith" validateChunksWith
