{- $setup

>>> :m
>>> import Control.Monad (when)
>>> import Control.Concurrent (threadDelay)
>>> import Data.Function ((&))
>>> import System.IO (hClose, IOMode(..), openFile)

>>> import Streamly.Data.Stream (Stream)
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Control.Exception as Exception
-}
