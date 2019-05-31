-- A concurrent TCP server that echoes everything that it receives.

import Streamly
import Streamly.Network.Socket
import Streamly.Network.Server
import qualified Streamly.Prelude as S

main :: IO ()
main = S.drain
    $ parallely $ S.mapM (flip withSocket (\sk -> writeArrays sk $ readArrays sk))
    $ serially $ connectionsOnAllAddrs 8090
