import Data.Function ((&))

import Streamly
import qualified Streamly.Memory.Array as A
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Network.Socket as SK
import qualified Streamly.Internal.Network.Inet.TCP as TCP

main =
      S.unfold TCP.acceptOnPort 8090        -- SerialT IO Socket
    & S.concatMapWith async frames  -- SerialT IO (Array Word8)
    & S.drain

    where

    frames sk =
          S.unfold SK.read sk
        & S.chunksOf 1 (A.writeN 1)
