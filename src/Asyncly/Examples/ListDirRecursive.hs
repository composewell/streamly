{-# LANGUAGE FlexibleContexts #-}

module Asyncly.Examples.ListDirRecursive where

import Path.IO (listDir, getCurrentDir)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Asyncly

listDirRecursive :: IO ()
listDirRecursive = do
    liftIO $ hSetBuffering stdout LineBuffering
    runAsyncly $ getCurrentDir >>= readdir
    where readdir d = do
            (ds, fs) <- lift $ listDir d
            liftIO $ mapM_ putStrLn $ map show fs ++ map show ds
            --foldWith (<>) $ map readdir ds     -- serial
            --foldWith (<=>) $ map readdir ds    -- serial interleaved
            foldWith (<|) $ map readdir ds     -- concurrent left biased
            --foldWith (<|>) $ map readdir ds    -- concurrent interleaved
