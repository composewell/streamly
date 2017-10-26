import Control.Monad.IO.Class (liftIO)
import Asyncly

input :: StreamT IO String
input = do
    string <- liftIO getLine
    return string <> input

output :: StreamT IO String -> StreamT IO ()
output strings = do
    s <- strings
    liftIO $ putStrLn s

main = do
    putStrLn "Type something and press enter, repeat"
    runStreamT $ output input
