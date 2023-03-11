{- $setup
>>> :m
>>> :set -XFlexibleContexts
>>> import Control.Monad (void)
>>> import qualified Data.Foldable as Foldable
>>> import Data.Function ((&))
>>> import Data.Functor.Identity (Identity, runIdentity)
>>> import Data.IORef (newIORef, readIORef, writeIORef)
>>> import Data.Maybe (fromJust, isJust)
>>> import Data.Monoid (Endo(..), Last(..), Sum(..))
>>> import Prelude hiding (concatMap, filter, map, break, span, splitAt)

>>> import Streamly.Data.Array (Array)
>>> import Streamly.Data.Fold (Fold)
>>> import Streamly.Data.Stream (Stream)

>>> import qualified Streamly.Data.Array as Array
>>> import qualified Streamly.Data.Fold as Fold
>>> import qualified Streamly.Data.MutArray as MutArray
>>> import qualified Streamly.Data.Parser as Parser
>>> import qualified Streamly.Data.Stream as Stream
>>> import qualified Streamly.Data.StreamK as StreamK
>>> import qualified Streamly.Data.Unfold as Unfold

#ifndef RELEASED_MODULE
>>> :{
import qualified Streamly.Internal.Data.Fold as Fold
    (Fold(..), foldrM', toStreamKRev, filtering, mapMaybeM, reduce, snoc
    , snocl, snoclM, extractM, toStream, tracing, trace, lengthGeneric
    , defaultSalt, rollingMapM, indexGeneric, maybe, splitAt, takingEndByM
    , takingEndByM_, takeEndBySeq, partitionBy, partitionByM, unzipWith
    , unzipWithM, zipStreamWithM, indexing, indexingWith, indexed, top, bottom
    , bottomBy, droppingWhileM, with)
:}

>>> import qualified Streamly.Internal.Data.Fold.Window as FoldW
#endif
-}
