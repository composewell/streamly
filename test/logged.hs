import Strands
import Control.Monad.IO.Class (liftIO)

main = do
    logs <- waitLogged comp

    putStrLn $ "\nResuming with logs:"
    logs1 <- waitLogged $ eachWithLog comp logs

    putStrLn $ "\nResuming with logs:"
    logs2 <- waitLogged $ eachWithLog comp logs1

    putStrLn $ "\nLogs at the end must be empty:"
    putStrLn $ show logs2

    putStrLn $ "\nRunning the last log again using wait:"
    wait $ eachWithLog comp logs1

    where
    comp = logged $ threads 5 $ do
         r <- logged $ each [1..3]
         logged $ liftIO $ print ("A",r)
         suspend
         logged $ liftIO $ print ("B",r)
         suspend
         liftIO $ print ("C",r)
