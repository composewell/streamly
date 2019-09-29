-- A TCP client that does the following:
-- * Reads multiple filenames passed on the command line
-- * Opens as many concurrent connections to the server
-- * Sends all the files concurrently to the server

import System.Environment (getArgs)

import Streamly
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.Network.Client as Client

import System.IO (withFile, IOMode(..))

main :: IO ()
main =
    let sendFile file =
            withFile file ReadMode $ \src ->
                  S.fold (Client.writeArrays (127, 0, 0, 1) 8090)
                $ IFH.readArrays src
     in getArgs >>= S.drain . parallely . S.mapM sendFile . S.fromList
