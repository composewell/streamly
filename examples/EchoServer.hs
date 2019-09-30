-- A concurrent TCP server that echoes everything that it receives.

import Streamly
import Streamly.Network.Socket
import Streamly.Network.Server
import qualified Streamly.Prelude as S

main :: IO ()
main = S.drain
    $ parallely $ S.mapM (flip withSocket echo)
    $ serially $ S.unfold listenOnPort 8090
    where echo sk = S.fold (writeArrays sk) $ readArrays sk
