{- $setup
>>> :m
>>> :set -XQuasiQuotes
>>> :set -XScopedTypeVariables
>>> import Control.Exception (SomeException, evaluate, try)
>>> import Data.Either (Either, isLeft)
>>> import Data.Maybe (fromJust, isNothing, isJust)
>>> import Data.Word (Word16)
>>> import Streamly.Data.Array (Array)
>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Unicode.Stream as Unicode

For APIs that have not been released yet.

>>> import Streamly.Internal.FileSystem.WindowsPath (WindowsPath, path)
>>> import qualified Streamly.Internal.FileSystem.WindowsPath as Path

Utilities:

>>> fails x = isLeft <$> (try (evaluate x) :: IO (Either SomeException String))
-}
