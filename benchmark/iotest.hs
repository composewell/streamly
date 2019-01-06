import Streamly
import Streamly.Buffer
import qualified Streamly.Prelude as S

import Data.Word (Word8)
import System.IO (IOMode(..), openFile)
-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
    handle <- openFile "in.txt" ReadMode
    {-
    buf <- hGetBuffer handle 1000000004
    -- S.head (toWord8Stream buf) >>= print
    S.length (toWord8Stream buf) >>= print
    -}

    -- S.length (fromHandleWord8 handle (32*1024 - 16)) >>= print
    -- runStream $ fromHandleWord8 handle (32*1024 - 16)

    -- S.length (fromHandle handle) >>= print
    S.length (fromHandleBuffers handle (32*1024)) >>= print
    -- S.head (fromHandle handle) >>= print
    {-
    bytes <- BS.readFile "in.txt"
    -- print $ BS.head bytes
    print $ BS.length bytes
    -}
