import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

import Asyncly (AsynclyT, runAsyncly)

input :: AsynclyT IO String
input = do
    string <- liftIO getLine
    return string <> input

output :: AsynclyT IO String -> AsynclyT IO ()
output strings = do
    s <- strings
    liftIO $ putStrLn s

main = do
    putStrLn "Type something and press enter, repeat"
    runAsyncly $ output input
