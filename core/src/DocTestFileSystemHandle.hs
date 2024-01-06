{- $setup
>>> :m
>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.FileSystem.Handle as Handle hiding (readChunks)
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Data.Unfold as Unfold

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.Data.Array as Array (unsafeCreateOf)
>>> import qualified Streamly.Internal.Data.Unfold as Unfold (first)
>>> import qualified Streamly.Internal.FileSystem.Handle as Handle
>>> import qualified Streamly.Internal.System.IO as IO (defaultChunkSize)
-}
