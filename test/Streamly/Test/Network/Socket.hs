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

import qualified Streamly.Internal.Data.Array.Storable.Foreign.Types as A
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Internal.Network.Socket as SK
import qualified Streamly.Internal.Unicode.Stream as U
import qualified Streamly.Prelude as S

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
          S.unfold SK.readChunksWithBufferOf (100, sk)
        & S.fold (SK.writeChunks sk)
        & discard

handlerChunks :: Socket -> IO ()
handlerChunks sk =
          S.unfold SK.readChunks sk
        & S.fold (SK.writeChunks sk)
        & discard

handlerwithbuffer :: Socket -> IO ()
handlerwithbuffer sk =
          S.unfold SK.readWithBufferOf (100, sk)
        & S.fold (SK.writeWithBufferOf 100 sk)
        & discard

handlerRW :: Socket -> IO ()
handlerRW sk =
          S.unfold SK.read sk
        & S.fold (SK.write sk)
        & discard
------------------------------------------------------------------------------
-- Accept connecttions and handle connected sockets
------------------------------------------------------------------------------

server :: PortNumber -> MVar () -> (Socket -> IO ()) -> IO ()
server port sem handler = do
    putMVar sem ()
    (S.serially $ S.unfold TCP.acceptOnPort port)
        & (S.asyncly . S.mapM (SK.handleWithM handler))
        & S.drain

remoteAddr :: (Word8,Word8,Word8,Word8)
remoteAddr = (127, 0, 0, 1)

sender :: PortNumber -> MVar () -> SerialT IO Char
sender port sem = do
    _ <- liftIO $ takeMVar sem
    liftIO $ threadDelay 1000000                       -- wait for server
    S.replicate 1 testDataSource                        -- SerialT IO String
        & U.unwords UF.fromList                        -- SerialT IO Char
        & U.encodeLatin1                               -- SerialT IO Word8
        & TCP.transformBytesWith remoteAddr port       -- SerialT IO Word8
        & U.decodeLatin1                               -- SerialT IO Char

execute :: PortNumber -> Int -> (Socket -> IO ()) -> IO [Char]
execute port size handler = do
    sem <- newEmptyMVar
    tid <- forkIO $ server port sem handler
    lst <- sender port sem
        & S.take size
        & S.toList
    killThread tid
    return $ lst

validateWithBufferOf :: Property
validateWithBufferOf = monadicIO $ do
    ls2 <- run $ execute 8000 45000 handlerwithbuffer
    assert (testDataSource == ls2)

validateRW :: Property
validateRW = monadicIO $ do
    ls2 <- run $ execute 8001 A.defaultChunkSize handlerRW
    assert (take A.defaultChunkSize testDataSource == ls2)

validateChunks :: Property
validateChunks = monadicIO $ do
    ls2 <- run $ execute 8002 45000 handlerChunks
    assert (testDataSource == ls2)

validateChunksWithBufferOf :: Property
validateChunksWithBufferOf = monadicIO $ do
    ls2 <- run $ execute 8003 45000 handlerChunksWithBuffer
    assert (testDataSource == ls2)

main :: IO ()
main = hspec $ do
    modifyMaxSuccess (const 1) $ do
        describe "Socket Tests: " $ do
            prop "read/write" validateRW
            prop "readWithBufferOf/writeWithBufferOf" validateWithBufferOf
            prop "readChunksWithBufferOf" validateChunksWithBufferOf
            prop "readChunks/writeChunks" validateChunks
