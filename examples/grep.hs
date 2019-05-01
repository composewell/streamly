import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (ord, chr)
import System.IO (openFile, IOMode(..), Handle, hClose, stdout)
import System.IO (putChar)

import qualified Streamly.Array as A
import qualified Streamly.FileIO as IO
import qualified Streamly.Fold as FL
import qualified Streamly.Prelude as S
import qualified Streamly.Sink as Sink

main = do
    inText <- openFile "../benchmark/text-processing/gutenberg-500.txt" ReadMode
    (IO.toHandleWith 80 stdout
        $ S.map (\x ->
                    if x == (fromIntegral $ ord '\r')
                    then (fromIntegral $ ord '\n')
                    else x
                )
        $ S.concatMap S.fromList
            -- S.fromArray
        $ FL.splitOn (A.fromList $ map (fromIntegral .  ord) "aaa")
                     (FL.ltakeWhile (/= (fromIntegral $ ord '\n'))
                     -- (FL.ltake 1000000
                                    FL.toList)
                                    -- (FL.toArrayN 8000000))
        $ IO.fromHandle inText) >>= print
