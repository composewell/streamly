-- A concurrent TCP server that echoes everything that it receives.

import Streamly
import Streamly.Internal.Network.Socket (readArrays)
import Streamly.Network.Socket
import Streamly.Network.Server
import qualified Streamly.Prelude as S

main :: IO ()
main = S.drain
    $ parallely $ S.mapM (flip withSocketM echo)
    $ serially $ S.unfold listenOnPort 8090
    where echo sk = S.fold (writeArrays sk) $ S.unfold readArrays sk
