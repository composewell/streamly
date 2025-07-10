{- $setup
>>> :m
>>> :set -XQuasiQuotes
>>> import Control.Exception (SomeException, evaluate, try)
>>> import Data.Either (Either, isLeft)
>>> import Data.Maybe (fromJust, isJust, isNothing)
>>> import Streamly.FileSystem.Path (Path, path)
>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.FileSystem.Path as Path
>>> import qualified Streamly.Unicode.Stream as Unicode

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.FileSystem.Path as Path

Utilities:

>>> fails x = isLeft <$> (try (evaluate x) :: IO (Either SomeException String))
-}
