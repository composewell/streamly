{-# LANGUAGE FlexibleContexts          #-}

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad.IO.Class (liftIO)
import System.IO
import Asyncly

main = do
    hSetBuffering stdout LineBuffering
    logs <- waitLogged_ comp

    putStrLn $ "\nResuming with logs:"
    logs1 <- waitLogged_ $ eachWithLog comp logs

    putStrLn $ "\nResuming with logs:"
    logs2 <- waitLogged_ $ eachWithLog comp logs1

    putStrLn $ "\nLogs at the end must be empty:"
    putStrLn $ show logs2

    putStrLn $ "\nRunning the last log again using wait:"
    wait_ $ eachWithLog comp logs1

    where

    comp = logged $ threads 5 $ do
         r <- logged $ each [1..3]
         logged $ liftIO $ print ("A",r)
         suspend
         logged $ liftIO $ print ("B",r)
         x <- logged $ (,) <$> (event 1 <|> event 2) <*> (event 3 <|> event 4)
         suspend
         liftIO $ print ("C", r, x)

    event n = async $ liftIO $ (do putStrLn ("event" ++ show n); return n :: IO Int)
