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

testDataLarge :: String
testDataLarge = (concat $ replicate 1000 testData)

------------------------------------------------------------------------------
-- Read and Write data on a socket
------------------------------------------------------------------------------
handlerRW :: Socket -> IO ()
handlerRW sk =
          S.unfold SK.read sk
        & S.fold (SK.write sk)
        & discard

------------------------------------------------------------------------------
-- Accept connecttions and handle connected sockets
------------------------------------------------------------------------------
server
    :: UF.Unfold IO PortNumber Socket
    -> PortNumber
    -> MVar ()
    -> (Socket -> IO ())
    -> IO ()
server addr port sem handler = do
    putMVar sem ()
    (S.serially $ S.unfold addr port)
        & (S.asyncly . S.mapM (SK.handleWithM handler))
        & S.drain

remoteAddr :: (Word8,Word8,Word8,Word8)
remoteAddr = (127, 0, 0, 1)

sender :: PortNumber -> MVar () -> SerialT IO Char
sender port sem = do
    _ <- liftIO $ takeMVar sem
    liftIO $ threadDelay 1000000                       -- wait for server
    S.replicate 1 testDataLarge                        -- SerialT IO String
        & U.unwords UF.fromList                        -- SerialT IO Char
        & U.encodeLatin1                               -- SerialT IO Word8
        & TCP.transformBytesWith remoteAddr port       -- SerialT IO Word8
        & U.decodeLatin1                               -- SerialT IO Char

execute
    :: UF.Unfold IO PortNumber Socket
    -> PortNumber
    -> Int
    -> (Socket -> IO ())
    -> IO [Char]
execute addr port size handler = do
    sem <- newEmptyMVar
    tid <- forkIO $ server addr port sem handler
    lst <- sender port sem
        & S.take size
        & S.toList
    killThread tid
    return $ lst

validateOnPort :: Property
validateOnPort = monadicIO $ do
    ls2 <- run $ execute TCP.acceptOnPort 8000 A.defaultChunkSize handlerRW
    assert (take A.defaultChunkSize testDataLarge == ls2)

validateOnPortLocal :: Property
validateOnPortLocal = monadicIO $ do
    ls2 <- 
          run 
        $ execute TCP.acceptOnPortLocal 8001 A.defaultChunkSize handlerRW
    assert (take A.defaultChunkSize testDataLarge == ls2)

main :: IO ()
main = hspec $ do
    modifyMaxSuccess (const 1) $ do
        describe "TCP Tests: " $ do
            prop "acceptOnPort" validateOnPort
            prop "acceptOnPortLocal" validateOnPortLocal
        --  prop "acceptOnAddr/connect" Tested as part of above test cases
