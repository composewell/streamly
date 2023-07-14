{- $setup

>>> :m
>>> import Control.Concurrent (threadDelay)
>>> import Data.Function (fix, (&))
>>> import Data.Semigroup (cycle1)

>>> effect n = print n >> return n

>>> import Streamly.Data.StreamK (StreamK)
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Parser as Parser
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Data.StreamK as StreamK
>>> import qualified Streamly.FileSystem.Dir as Dir

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.Data.StreamK as StreamK
>>> import qualified Streamly.Internal.FileSystem.Dir as Dir
-}
