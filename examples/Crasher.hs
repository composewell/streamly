import Data.Function ((&))

import Streamly (async)
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Network.Socket as SK
import qualified Streamly.Internal.Network.Inet.TCP as TCP

main =
      S.unfold TCP.acceptOnPort 8090            -- SerialT IO Socket
    & S.concatMapWith async (S.unfold SK.read)  -- SerialT IO (Array Word8)
    & S.drain
