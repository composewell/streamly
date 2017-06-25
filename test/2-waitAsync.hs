import Strands
import Control.Monad.IO.Class (liftIO)

main = do
    xs <- waitAsync $ do
        liftIO $ putStrLn "hello"
        return 5
    print xs
