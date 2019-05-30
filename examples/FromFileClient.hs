-- A TCP client that does the following:
-- * Reads multiple filenames passed on the command line
-- * Opens as many concurrent connections to the server
-- * Sends all the files concurrently to the server

import Network.Socket
import System.IO (openFile, IOMode(..), hClose)
import System.Environment (getArgs)

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.FileSystem.File as File
import qualified Streamly.Network.Socket as NS

main :: IO ()
main = getArgs >>= S.drain . parallely . S.mapM sendFile . S.fromList

    where

    sendFile file = do
        h <- openFile file ReadMode
        openConnection >>= \sk -> do
            S.drain $ NS.writeArrays sk $ File.readArrays h
            close sk
        hClose h

    openConnection = do
        sock <- socket AF_INET Stream defaultProtocol
        connect sock $ SockAddrInet 8090 (tupleToHostAddress (127,0,0,1))
        return sock
