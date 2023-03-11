{- $setup
>>> :m
>>> :set -XFlexibleContexts
>>> import Data.Function ((&))
>>> import Data.Functor.Identity (Identity(..))
>>> import System.IO.Unsafe (unsafePerformIO)

>>> import Streamly.Data.Array (Array)
>>> import Streamly.Data.Stream (Stream)

>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Stream as Stream
-}
