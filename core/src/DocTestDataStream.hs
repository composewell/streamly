{- $setup

>>> :m
>>> import Control.Concurrent (threadDelay)
>>> import Control.Monad (void)
>>> import Control.Monad.IO.Class (MonadIO (liftIO))
>>> import Control.Monad.Trans.Class (lift)
>>> import Control.Monad.Trans.Identity (runIdentityT)
>>> import Data.Char (isSpace)
>>> import Data.Either (fromLeft, fromRight, isLeft, isRight, either)
>>> import Data.Maybe (fromJust, isJust)
>>> import Data.Function (fix, (&))
>>> import Data.Functor.Identity (runIdentity)
>>> import Data.IORef
>>> import Data.Semigroup (cycle1)
>>> import GHC.Exts (Ptr (Ptr))
>>> import System.IO (stdout, hSetBuffering, BufferMode(LineBuffering))

>>> hSetBuffering stdout LineBuffering
>>> effect n = print n >> return n

>>> import Streamly.Data.Stream (Stream)
>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.Scanl as Scanl
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Data.StreamK as StreamK
>>> import qualified Streamly.Data.Unfold as Unfold
>>> import qualified Streamly.Data.Parser as Parser
>>> import qualified Streamly.FileSystem.DirIO as Dir

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.Data.Fold as Fold
>>> import qualified Streamly.Internal.Data.Parser as Parser
>>> import qualified Streamly.Internal.Data.Stream as Stream
>>> import qualified Streamly.Internal.Data.StreamK as StreamK
>>> import qualified Streamly.Internal.Data.Unfold as Unfold
>>> import qualified Streamly.Internal.FileSystem.DirIO as Dir
-}
