{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Monoid ((<>))
import Path.IO (listDir, getCurrentDir)
import Path (Path, Dir, reldir)
import Asyncly

main = runAsyncly $ getCurrentDir >>= readdir
    where readdir d = do
            (ds, fs) <- lift $ listDir d
            liftIO $ mapM_ putStrLn $ map show fs ++ map show ds
            --foldWith (<>) $ map readdir ds     -- serial
            --foldWith (<=>) $ map readdir ds    -- serial interleaved
            foldWith (<|) $ map readdir ds     -- concurrent left biased
            --foldWith (<|>) $ map readdir ds    -- concurrent interleaved
