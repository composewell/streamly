-- A concurrent TCP server that:
--
-- * receives connections from clients
-- * splits the incoming data into lines
-- * lines from concurrent connections are merged into a single srteam
-- * writes the line stream to an output file

import System.Environment (getArgs)

import Streamly
import Streamly.Data.String
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Memory.Array as A
import qualified Streamly.Network.Socket as NS
import qualified Streamly.Network.Server as NS
import qualified Streamly.Prelude as S

import System.IO (withFile, IOMode(..))

main :: IO ()
main = do
    file <- fmap head getArgs
    withFile file AppendMode
        (\src -> S.fold (FH.write src)
        $ encodeChar8Unchecked
        $ S.concatUnfold A.read
        $ S.concatMapWith parallel (flip NS.withSocket recv)
        $ S.unfold NS.listenOnPort 8090)

    where

    recv =
          S.splitWithSuffix (== '\n') A.write
        . decodeChar8
        . S.unfold NS.read
