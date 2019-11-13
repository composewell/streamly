import qualified Streamly.Internal.Prelude as S
import qualified Streamly.Data.Fold as FL
-- import qualified Streamly.Memory.Array as A
import qualified Streamly.Internal.Memory.ArrayStream as AS
import qualified Streamly.Internal.FileSystem.Handle as IFH
import qualified Streamly.FileSystem.Handle as FH
import qualified System.IO as FH
-- import qualified Streamly.FileSystem.FD as FH
-- import qualified Streamly.Data.Unicode.Stream as US

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT(..), get, put)
import Data.Char (ord)
import System.Environment (getArgs)
import System.IO (IOMode(..), hSeek, SeekMode(..))
import Data.Function ((&))

newHandle :: StateT (Maybe (FH.Handle, Int)) IO FH.Handle
newHandle = do
    old <- get
    idx <- case old of
            Nothing -> return 0
            Just (h, i) -> liftIO (FH.hClose h) >> return (i + 1)
    h <- liftIO $ FH.openFile ("dst-xyz-" ++ show idx ++ ".txt") WriteMode
    put (Just (h, idx))
    return h

-- XXX reduce the input stream to a stream of file names
-- The fold can return the file name/handle after it is done.
-- similarly the files can written to directories and we can generate a stream
-- of directory names.
splitFile :: FH.Handle -> IO ()
splitFile inHandle =
      S.unfold FH.read inHandle
    & S.liftInner
    & S.chunksOf2 (180 * 1024 * 1024) newHandle IFH.write2
    & S.evalStateT Nothing  -- generate new handle for each iteration
    & S.drain

main :: IO ()
main = do
    name <- fmap head getArgs
    src <- FH.openFile name ReadMode
    splitFile src
