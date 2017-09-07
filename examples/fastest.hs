#!/usr/bin/env stack
-- stack runghc

import Asyncly
import Network.HTTP.Simple

main = do
    let urls = [ "https://www.google.com/search?q=haskell"
               , "https://www.duckduckgo.com/?q=haskell"
               , "https://www.bing.com/search?q=haskell"
               ]
    xs <- toList $ foldWith (<|>) $ map get urls
    mapM_ (putStrLn . show) $ zip [1..] xs

    where get s = liftIO $ httpNoBody (parseRequest_ s) >> return s
