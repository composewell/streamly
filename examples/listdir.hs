{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Path.IO (listDir, getCurrentDir)
import Path (Path, Dir, reldir)
import Asyncly

main = runAsyncly $ getCurrentDir >>= readdir
    where readdir d = do
            (ds, fs) <- lift $ listDir d
            liftIO $ mapM_ putStrLn $ map show fs ++ map show ds
            for ds readdir      -- concurrent version
         -- each ds >>= readdir -- serial version
