module Main (main) where

import System.IO (BufferMode(..), hSetBuffering, stdout)
import qualified Streamly.Test.FileSystem.Event.Windows as Event

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    Event.testCommonEvents   
