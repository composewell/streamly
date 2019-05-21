import System.IO (stdout)
import Data.Char (ord)

import Streamly
import qualified Streamly.FileSystem.File as FH
import qualified Streamly.Fold as FL
import qualified Streamly.Mem.Array as A
import qualified Streamly.Network.Socket as NS
import qualified Network.Socket as NS
import qualified Streamly.Prelude as S

main :: IO ()
main = S.drain
    $ parallely $ S.mapM (withSocket handle)
    $ serially  $ fmap fst (NS.recvConnectionsOn 8090)

    where

    withSocket f sk = f sk >> NS.close sk
    handle =
          S.mapM_ print
        . FL.sessionsOf 3 FL.sum
        . S.map A.length
        . NS.readArrays
