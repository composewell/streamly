import Streamly
import qualified Streamly.Prelude as S
import Streamly.Network.Socket
import qualified Streamly.Mem.Array as A
import qualified Streamly.Fold as FL

main :: IO ()
main = S.drain $ parallely
    -- $ S.mapM (\sk -> writeArrays sk $ readArrays sk)
    $ S.mapM (\sk -> S.mapM_ print $ FL.sessionsOf 10 FL.sum $ S.map A.length $ readArrays sk)
    $ serially $ fmap fst (serveTCPOn 8090)
