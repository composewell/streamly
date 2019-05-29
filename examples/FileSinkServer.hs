-- A concurrent TCP server that:
--
-- * receives connections from clients
-- * splits the incoming data into lines
-- * lines from concurrent connections are merged into a single srteam
-- * writes the line stream to an output file

import Control.Monad.IO.Class (liftIO)
import Data.Char (ord, chr)
import System.IO (openFile, IOMode(..), hClose, hPutStrLn)
import System.Environment (getArgs)

import Streamly
import qualified Streamly.Fold as FL
import qualified Streamly.Mem.Array as A
import qualified Streamly.Network.Socket as NS
import qualified Network.Socket as NS
import qualified Streamly.Prelude as S

main :: IO ()
main = do
    h <- openOutput
    S.mapM_ (hPutLines h)
        $ S.concatMapBy parallel (withSocket recv)
        $ fmap fst (NS.recvConnectionsOn 8090)
    hClose h

    where

    openOutput = fmap head getArgs >>= \file -> openFile file AppendMode

    withSocket f sk = S.finally (liftIO (NS.close sk)) (f sk)
    hPutLines h = hPutStrLn h . map (chr . fromIntegral) . A.toList
    ord' = (fromIntegral . ord)
    recv =
          FL.splitSuffixBy (== ord' '\n') (A.toArrayN 1024)
        . NS.read
