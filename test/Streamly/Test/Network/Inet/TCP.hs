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
import Streamly.Prelude (SerialT)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as Array
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Internal.Network.Socket as Socket
import qualified Streamly.Internal.Unicode.Stream as Unicode
import qualified Streamly.Prelude as Stream

import Test.Hspec
import Test.Hspec.QuickCheck

------------------------------------------------------------------------------
-- Command Handlers
------------------------------------------------------------------------------
testData :: String
testData = "Test data 1234567891012131415!@#$%^&*()`~ABCD"

testDataSource :: String
testDataSource = (concat $ replicate 1000 testData)

------------------------------------------------------------------------------
-- Read and Write data on a socket
------------------------------------------------------------------------------
handlerRW :: Socket -> IO ()
handlerRW sk =
          Stream.unfold Socket.read sk
        & Stream.fold (Socket.write sk)
        & discard

------------------------------------------------------------------------------
-- Accept connections and handle connected sockets
------------------------------------------------------------------------------
server
    :: Unfold.Unfold IO PortNumber Socket
    -> PortNumber
    -> MVar ()
    -> (Socket -> IO ())
    -> IO ()
server addr port sem handler = do
    putMVar sem ()
    (Stream.serially $ Stream.unfold addr port)
        & (Stream.asyncly . Stream.mapM (Socket.handleWithM handler))
        & Stream.drain

remoteAddr :: (Word8,Word8,Word8,Word8)
remoteAddr = (127, 0, 0, 1)

sender :: PortNumber -> MVar () -> SerialT IO Char
sender port sem = do
    _ <- liftIO $ takeMVar sem
    liftIO $ threadDelay 1000000                       -- wait for server
    Stream.replicate 1000 testData                   -- SerialT IO String
        & Stream.concatMap Stream.fromList              -- SerialT IO Char
        & Unicode.encodeLatin1                         -- SerialT IO Word8
        & TCP.transformBytesWith remoteAddr port       -- SerialT IO Word8
        & Unicode.decodeLatin1                         -- SerialT IO Char

execute
    :: Unfold.Unfold IO PortNumber Socket
    -> PortNumber
    -> Int
    -> (Socket -> IO ())
    -> IO (SerialT IO Char)
execute addr port size handler = do
    sem <- newEmptyMVar
    tid <- forkIO $ server addr port sem handler
    let lst = sender port sem
                & Stream.take size
                & Stream.finally (killThread tid)
    return $ lst

validateOnPort :: Property
validateOnPort = monadicIO $ do
    res <- run $ do
        ls2 <- execute TCP.acceptOnPort 8007 Array.defaultChunkSize handlerRW
        let dataChunk = take Array.defaultChunkSize testDataSource
        Stream.eqBy (==) (Stream.fromList dataChunk) ls2
    assert res

validateOnPortLocal :: Property
validateOnPortLocal = monadicIO $ do
    res <- run $ do
        ls2 <-
            execute
            TCP.acceptOnPortLocal
            8008
            Array.defaultChunkSize
            handlerRW
        let dataChunk = take Array.defaultChunkSize testDataSource
        Stream.eqBy (==) (Stream.fromList dataChunk) ls2
    assert res

main :: IO ()
main = hspec $ do
    modifyMaxSuccess (const 1) $ do
        describe "TCP Tests: " $ do
            prop "acceptOnPort" validateOnPort
            prop "acceptOnPortLocal" validateOnPortLocal
        --  prop "acceptOnAddr/connect" Tested as part of above test cases
