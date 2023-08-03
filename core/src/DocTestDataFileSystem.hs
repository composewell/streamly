{- $setup
>>> :m
>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Unfold as Unfold
>>> import qualified Streamly.Data.Stream as Stream

>>> import qualified Streamly.Internal.Data.Array.Type as Array (writeNUnsafe)
>>> import qualified Streamly.Internal.Data.Stream as Stream
>>> import qualified Streamly.Internal.Data.Unfold as Unfold (first)
>>> import qualified Streamly.Internal.FileSystem.Handle as Handle
>>> import qualified Streamly.Internal.System.IO as IO (defaultChunkSize)
-}