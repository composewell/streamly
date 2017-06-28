import Strands
import Control.Monad.IO.Class (liftIO)

main = do
    xs <- waitLogged $ logged $ threads 0 $ do
         r <- logged $ return (5::Int)
         logged $ liftIO $ print ("A",r)
         suspend
         logged $ liftIO $ print ("B",r)
         suspend
         liftIO $ print ("C",r)
    putStrLn $ show xs
