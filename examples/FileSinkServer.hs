-- A concurrent TCP server that:
--
-- * receives connections from clients
-- * splits the incoming data into lines
-- * lines from concurrent connections are merged into a single srteam
-- * writes the line stream to an output file

import System.Environment (getArgs)

import Streamly
import Streamly.String
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Fold as FL
import qualified Streamly.Mem.Array as A
import qualified Streamly.Network.Socket as NS
import qualified Streamly.Network.Server as NS
import qualified Streamly.Prelude as S

import System.IO (withFile, IOMode(..))

main :: IO ()
main = do
    file <- fmap head getArgs
    withFile file AppendMode
        (\src -> FH.write src
        $ encodeChar8Unchecked
        $ S.concatMap A.read
        $ S.concatMapBy parallel (flip NS.withSocketS recv)
        $ NS.connectionsOnAllAddrs 8090)

    where

    recv =
          FL.splitBySuffix (== '\n') A.toArray
        . decodeChar8
        . NS.read
