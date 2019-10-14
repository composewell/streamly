-- A concurrent TCP server that echoes everything that it receives.

import Control.Exception (finally)
import Control.Monad.IO.Class (liftIO)
import Streamly
import Streamly.Network.Socket
import qualified Network.Socket as Net
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Prelude as S

main :: IO ()
main = S.drain
    $ parallely $ S.mapM (useWith echo)
    $ serially $ S.unfold TCP.acceptOnPort 8090
    where echo sk = S.fold (writeArrays sk) $ S.unfold readArraysOf (32768, sk)
          useWith f sk = finally (liftIO (Net.close sk)) (f sk)
