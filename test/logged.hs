{-# LANGUAGE FlexibleContexts          #-}

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad.IO.Class (liftIO)
import System.IO
import Asyncly
import Control.Monad.Trans.Recorder (pause, record)

main = do
    hSetBuffering stdout LineBuffering
    logs <- waitRecord_ comp

    putStrLn $ "\nResuming with logs:"
    logs1 <- waitRecord_ $ playRecordings comp logs

    putStrLn $ "\nResuming with logs:"
    logs2 <- waitRecord_ $ playRecordings comp logs1

    putStrLn $ "\nLogs at the end must be empty:"
    putStrLn $ show logs2

    where

    comp = record $ threads 5 $ do
         r <- record $ each [1..3]
         record $ liftIO $ print ("A",r)
         record pause
         record $ liftIO $ print ("B",r)
         x <- record $ (,) <$> (event 1 <|> event 2) <*> (event 3 <|> event 4)
         record pause
         liftIO $ print ("C", r, x)

    event n = async $ liftIO $ (do putStrLn ("event" ++ show n); return n :: IO Int)
