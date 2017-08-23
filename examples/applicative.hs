{-# LANGUAGE FlexibleContexts          #-}

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad.IO.Class (liftIO)
import System.IO
import Asyncly

main = do
    hSetBuffering stdout LineBuffering
    xs <- runAsyncly $ do
        x <- (,) <$> (event 1 <|> event 2) <*> (event 3 <|> event 4)
        --x <- (,) <$> (for [1,2] return) <*> (for [3,4] return)
        liftIO $ putStrLn $ show x
        return x
    putStrLn $ show xs
    where
        event n = liftIO $ (do putStrLn ("event" ++ show n); return n :: IO Int)
