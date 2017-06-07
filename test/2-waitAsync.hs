import Duct
import Control.Monad.IO.Class (liftIO)

main = waitAsync $ do
    liftIO $ putStrLn "hello"
