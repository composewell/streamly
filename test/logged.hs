import Strands
import Control.Monad.IO.Class (liftIO)

main = do
    logs <- waitLogged comp
    -- XXX even if we use wait replay should still work, though new logs will
    -- not be generated.
    logs1 <- waitLogged $ eachWithLog comp logs
    logs2 <- waitLogged $ eachWithLog comp logs1
    putStrLn $ show logs2

    where
    comp = logged $ threads 5 $ do
         r <- logged $ each [1..3]
         logged $ liftIO $ print ("A",r)
         suspend
         logged $ liftIO $ print ("B",r)
         suspend
         liftIO $ print ("C",r)
