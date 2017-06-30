import Asyncly
import Control.Monad.IO.Class (liftIO)

main = do
    xs <- wait $ do
        liftIO $ putStrLn "hello"
        return 5
    print xs
