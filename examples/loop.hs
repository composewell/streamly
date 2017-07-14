import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomIO)
import Asyncly

main = wait_ $ do
    x <- loop
    liftIO $ print x

    where

    loop = do
        liftIO $ threadDelay 1000000
        x <- liftIO (randomIO :: IO Int)
        return x <|> loop
