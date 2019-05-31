-- A TCP client that does the following:
-- * Reads multiple filenames passed on the command line
-- * Opens as many concurrent connections to the server
-- * Sends all the files concurrently to the server

import System.Environment (getArgs)

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.FileSystem.File as File
import qualified Streamly.Network.Client as Client

main :: IO ()
main =
    let sendFile = Client.writeArrays (127,0,0,1) 8090 . File.readArrays
    in getArgs >>= S.drain . parallely . S.mapM sendFile . S.fromList
