{-# LANGUAGE FlexibleContexts          #-}

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad.IO.Class (liftIO)
import System.IO
import Strands

main = do
    hSetBuffering stdout LineBuffering
    xs <- wait $ threads 5 $ do
        x <- (,) <$> (event 1 <|> event 2) <*> (event 3 <|> event 4)
        --x <- (,) <$> choose [1,2] <*> choose [3,4]
        liftIO $ putStrLn $ show x
        return x
    putStrLn $ show xs
    where
        event n = async $ liftIO $ (do putStrLn ("event" ++ show n); return n :: IO Int)
