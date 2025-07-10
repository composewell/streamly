{- $setup
>>> :m
>>> :set -XQuasiQuotes
>>> import Control.Exception (SomeException, evaluate, try)
>>> import Data.Either (Either, isLeft)
>>> import Data.Maybe (isNothing, isJust)
>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Unicode.Stream as Unicode

For APIs that have not been released yet.

>>> import Streamly.Internal.FileSystem.PosixPath (PosixPath, path)
>>> import qualified Streamly.Internal.FileSystem.PosixPath as Path

Utilities:

>>> fails x = isLeft <$> (try (evaluate x) :: IO (Either SomeException String))
-}
