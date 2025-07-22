{- $setup

>>> :m
>>> import Control.Concurrent (threadDelay)
>>> import Data.Function (fix, (&))
>>> import Data.Semigroup (cycle1)

>>> import Streamly.Data.StreamK (StreamK)
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Parser as Parser
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Data.StreamK as StreamK
>>> import qualified Streamly.FileSystem.DirIO as Dir

>>> mk = StreamK.fromStream . Stream.fromList
>>> un = Stream.toList . StreamK.toStream
>>> effect n = print n >> return n

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.FileSystem.Path as Path
>>> import qualified Streamly.Internal.Data.StreamK as StreamK
>>> import qualified Streamly.Internal.FileSystem.DirIO as Dir
-}
