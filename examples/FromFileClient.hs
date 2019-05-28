-- A TCP client that reads from stdin and sends to a server

import Network.Socket
import System.IO (stdin)

import qualified Streamly.Prelude as S
import qualified Streamly.FileSystem.File as File
import qualified Streamly.Network.Socket as NS

main :: IO ()
main = do
    openConnection >>= \sk ->
        S.drain $ NS.writeArrays sk $ File.readArrays stdin

    where

    openConnection = do
        sock <- socket AF_INET Stream defaultProtocol
        connect sock $ SockAddrInet 8090 (tupleToHostAddress (127,0,0,1))
        return sock
