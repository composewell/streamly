#!/usr/bin/env stack
-- stack runghc --package path --package path-io
{-# LANGUAGE FlexibleContexts #-}

import Path.IO (listDir, getCurrentDir)
import Path (Path, Dir, reldir)
import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))
import Asyncly

main = do
    liftIO $ hSetBuffering stdout LineBuffering
    runAsyncly $ getCurrentDir >>= readdir
    where readdir d = do
            (ds, fs) <- lift $ listDir d
            liftIO $ mapM_ putStrLn $ map show fs ++ map show ds
            --foldWith (<>) $ map readdir ds     -- serial
            --foldWith (<=>) $ map readdir ds    -- serial interleaved
            foldWith (<|) $ map readdir ds     -- concurrent left biased
            --foldWith (<|>) $ map readdir ds    -- concurrent interleaved
