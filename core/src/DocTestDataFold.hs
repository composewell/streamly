{- $setup
>>> :m
>>> :set -XFlexibleContexts
>>> import Control.Monad (void)
>>> import qualified Data.Foldable as Foldable
>>> import Data.Bifunctor(bimap)
>>> import Data.Function ((&))
>>> import Data.Functor.Identity (Identity, runIdentity)
>>> import Data.IORef (newIORef, readIORef, writeIORef)
>>> import Data.Maybe (fromJust, isJust)
>>> import Data.Monoid (Endo(..), Last(..), Sum(..))

>>> import Streamly.Data.Array (Array)
>>> import Streamly.Data.Fold (Fold, Tee(..))
>>> import Streamly.Data.Stream (Stream)

>>> import qualified Data.Map as Map
>>> import qualified Data.Set as Set
>>> import qualified Data.IntSet as IntSet
>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.MutArray as MutArray
>>> import qualified Streamly.Data.Parser as Parser
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Data.StreamK as StreamK
>>> import qualified Streamly.Data.Unfold as Unfold

For APIs that have not been released yet.

>>> import qualified Streamly.Internal.Data.Fold as Fold
>>> import qualified Streamly.Internal.Data.Scanl as Scanl
>>> import qualified Streamly.Internal.Data.Stream as Stream
-}
