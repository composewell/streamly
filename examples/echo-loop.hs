import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

import Asyncly (AsyncT, runAsyncly)

input :: AsyncT IO String
input = do
    string <- liftIO getLine
    return string <> input

output :: AsyncT IO String -> AsyncT IO ()
output strings = do
    s <- strings
    liftIO $ putStrLn s

main = do
    putStrLn "Type something and press enter, repeat"
    runAsyncly $ output input
