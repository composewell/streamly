{-# LANGUAGE FlexibleContexts #-}

import Streamly
import Streamly.Prelude as S
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Clock

withTimeStamp msg = do
    t <- getTime Monotonic
    let ns = toNanoSecs t `mod` 10^11
    putStrLn $ show (fromIntegral ns / 1.0e9) <> " " <> msg

-- acidRain :: MonadAsync m => SerialT m Event
producer =
      asyncly
    $ avgRate 1
    $ S.repeatM
    $ liftIO $ do
        withTimeStamp "produced"
        return 1

main :: IO ()
main = runStream $ do
    _ <- producer
    liftIO $ withTimeStamp "consumed\n"
