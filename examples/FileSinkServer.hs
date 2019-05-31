-- A concurrent TCP server that:
--
-- * receives connections from clients
-- * splits the incoming data into lines
-- * lines from concurrent connections are merged into a single srteam
-- * writes the line stream to an output file

import System.Environment (getArgs)

import Streamly
import Streamly.String
import qualified Streamly.FileSystem.File as File
import qualified Streamly.Fold as FL
import qualified Streamly.Mem.Array as A
import qualified Streamly.Network.Socket as NS
import qualified Streamly.Network.Server as NS
import qualified Streamly.Prelude as S

main :: IO ()
main = do
    file <- fmap head getArgs
    File.append file
        $ encodeChar8Unchecked
        $ S.concatMap A.read
        $ S.concatMapBy parallel recv
        $ NS.connectionsOnAllAddrs 8090

    where

    recv =
          FL.splitSuffixBy' (== '\n') (A.toArrayN 1024)
        . decodeChar8
        . NS.fromSocket
