import qualified Streamly.Prelude as S
import qualified Streamly.FileSystem.Handle as FH
import System.IO (openFile, IOMode(..))

main :: IO ()
main = do
    inh <- openFile "benchmark-tmpin-100MB.txt" ReadMode
    outh <- openFile "/dev/null" WriteMode
    S.fold (FH.write outh) (S.unfold FH.read inh)
