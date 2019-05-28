-- A concurrent TCP server that echoes everything that it receives.

import Streamly
import Streamly.Network.Socket
import qualified Streamly.Prelude as S
import qualified Network.Socket as NS

main :: IO ()
main = S.drain
    $ parallely $ S.mapM (withSocket (\sk -> writeArrays sk $ readArrays sk))
    $ serially  $ fmap fst (recvConnectionsOn 8090)

    where

    withSocket f sk = f sk >> NS.close sk
