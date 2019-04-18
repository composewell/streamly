{-# LANGUAGE CPP                       #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

#include "inline.hs"

-- |
-- Module      : Streamly.Streams.Prelude
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Streams.Prelude
    (
    -- * Stream Conversion
      fromStreamS
    , toStreamS

    -- * Running Effects
    , runStream

    -- * Conversion operations
    , fromList
    , fromArray
    , toList

    -- * Fold operations
    , foldrM
    , foldrMx
    , foldr
    , foldx'
    , foldxM'
    , foldl'
    , parselMx'

    -- Lazy left folds are useful only for reversing the stream
    , foldlS
    , foldlT

    , scanx'
    , scanxM'
    , postscanx'
    , postscanxM'

    -- * Zip style operations
    , eqBy
    , cmpBy

    -- * Fold Utilities
    , foldWith
    , foldMapWith
    , forEachWith
    )
where

import Control.Monad.Trans (MonadTrans(..))
import Foreign.Storable (Storable)
import Prelude hiding (foldr)
import qualified Prelude

import Streamly.Parse.Types (Status)

#ifdef USE_STREAMK_ONLY
import qualified Streamly.Streams.StreamK as S
#else
import qualified Streamly.Streams.StreamD as S
#endif

import Streamly.Streams.StreamK (IsStream(..))
import qualified Streamly.Streams.StreamK as K
import qualified Streamly.Streams.StreamD as D
import qualified Streamly.Array.Types as A

------------------------------------------------------------------------------
-- Conversion to and from direct style stream
------------------------------------------------------------------------------

-- These definitions are dependent on what is imported as S
{-# INLINE fromStreamS #-}
fromStreamS :: (IsStream t, Monad m) => S.Stream m a -> t m a
fromStreamS = fromStream . S.toStreamK

{-# INLINE toStreamS #-}
toStreamS :: (IsStream t, Monad m) => t m a -> S.Stream m a
toStreamS = S.fromStreamK . toStream

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

{-# INLINE_EARLY runStream #-}
runStream :: (IsStream t, Monad m) => t m a -> m ()
runStream m = D.runStream $ D.fromStreamK (toStream m)
{-# RULES "runStream fallback to CPS" [1]
    forall a. D.runStream (D.fromStreamK a) = K.runStream a #-}

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- @
-- fromList = 'Prelude.foldr' 'K.cons' 'K.nil'
-- @
--
-- Construct a stream from a list of pure values. This is more efficient than
-- 'K.fromFoldable' for serial streams.
--
-- @since 0.4.0
{-# INLINE_EARLY fromList #-}
fromList :: (Monad m, IsStream t) => [a] -> t m a
fromList = fromStreamS . S.fromList
{-# RULES "fromList fallback to StreamK" [1]
    forall a. S.toStreamK (S.fromList a) = K.fromFoldable a #-}

-- XXX add fallback to StreamK rule
--
-- | Construct a stream from an 'Array'.
--
{-# INLINE_EARLY fromArray #-}
fromArray :: (Monad m, IsStream t, Storable a) => A.Array a -> t m a
fromArray = D.fromStreamD . D.fromArray
-- {-# RULES "fromList fallback to StreamK" [1]
--     forall a. S.toStreamK (S.fromList a) = K.fromFoldable a #-}

-- | Convert a stream into a list in the underlying monad.
--
-- @since 0.1.0
{-# INLINE toList #-}
toList :: (Monad m, IsStream t) => t m a -> m [a]
toList m = S.toList $ toStreamS m

------------------------------------------------------------------------------
-- Folds
------------------------------------------------------------------------------

{-# INLINE foldrM #-}
foldrM :: (Monad m, IsStream t) => (a -> m b -> m b) -> m b -> t m a -> m b
foldrM step acc m = S.foldrM step acc $ toStreamS m

{-# INLINE foldrMx #-}
foldrMx :: (Monad m, IsStream t)
    => (a -> m x -> m x) -> m x -> (m x -> m b) -> t m a -> m b
foldrMx step final project m = S.foldrMx step final project $ toStreamS m

{-# INLINE foldr #-}
foldr :: (Monad m, IsStream t) => (a -> b -> b) -> b -> t m a -> m b
foldr f z = foldrM (\a b -> b >>= return . f a) (return z)

-- | Like 'foldx'', but with a monadic step function.
--
-- @since 0.7.0
{-# INLINE foldxM' #-}
foldxM' :: (IsStream t, Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> m b
foldxM' step begin done m = S.foldxM' step begin done $ toStreamS m

{-# INLINE parselMx' #-}
parselMx' :: (IsStream t, Monad m)
    => (x -> a -> m (Status x)) -> m (Status x) -> (x -> m b) -> t m a -> m b
parselMx' step begin done m = S.parselMx' step begin done $ toStreamS m

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- @since 0.7.0
{-# INLINE foldx' #-}
foldx' :: (IsStream t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> m b
foldx' step begin done m = S.foldx' step begin done $ toStreamS m

-- | Strict left associative fold.
--
-- @since 0.2.0
{-# INLINE foldl' #-}
foldl' :: (Monad m, IsStream t) => (b -> a -> b) -> b -> t m a -> m b
foldl' step begin m = S.foldl' step begin $ toStreamS m

{-# INLINE foldlS #-}
foldlS :: IsStream t => (t m b -> a -> t m b) -> t m b -> t m a -> t m b
foldlS = K.foldlS

-- | Lazy left fold to a transformer monad.
--
-- For example, to reverse a stream:
--
-- > S.toList $ S.foldlT (flip S.cons) S.nil $ (S.fromList [1..5] :: SerialT IO Int)
--
{-# INLINE foldlT #-}
foldlT :: (Monad m, IsStream t, Monad (s m), MonadTrans s)
    => (s m b -> a -> s m b) -> s m b -> t m a -> s m b
foldlT f z s = S.foldlT f z (toStreamS s)

------------------------------------------------------------------------------
-- Scans
------------------------------------------------------------------------------

{-# INLINE postscanxM' #-}
postscanxM' :: (IsStream t, Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> t m b
postscanxM' step begin done m =
    D.fromStreamD $ D.postscanxM' step begin done $ D.toStreamD m

{-# INLINE postscanx' #-}
postscanx' :: (IsStream t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
postscanx' step begin done m =
    D.fromStreamD $ D.postscanx' step begin done $ D.toStreamD m

{-# INLINE scanxM' #-}
scanxM' :: (IsStream t, Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> t m b
scanxM' step begin done m =
    D.fromStreamD $ D.scanxM' step begin done $ D.toStreamD m

-- | Strict left scan with an extraction function. Like 'scanl'', but applies a
-- user supplied extraction function (the third argument) at each step. This is
-- designed to work with the @foldl@ library. The suffix @x@ is a mnemonic for
-- extraction.
--
-- @since 0.7.0
{-# INLINE scanx' #-}
scanx' :: (IsStream t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx' step begin done m = fromStreamS $ S.scanx' step begin done $ toStreamS m

------------------------------------------------------------------------------
-- Comparison
------------------------------------------------------------------------------

-- | Compare two streams for equality
--
-- @since 0.5.3
{-# INLINE eqBy #-}
eqBy :: (IsStream t, Monad m) => (a -> b -> Bool) -> t m a -> t m b -> m Bool
eqBy f m1 m2 = D.eqBy f (D.toStreamD m1) (D.toStreamD m2)

-- | Compare two streams
--
-- @since 0.5.3
{-# INLINE cmpBy #-}
cmpBy
    :: (IsStream t, Monad m)
    => (a -> b -> Ordering) -> t m a -> t m b -> m Ordering
cmpBy f m1 m2 = D.cmpBy f (D.toStreamD m1) (D.toStreamD m2)

------------------------------------------------------------------------------
-- Fold Utilities
------------------------------------------------------------------------------

-- | A variant of 'Data.Foldable.fold' that allows you to fold a 'Foldable'
-- container of streams using the specified stream sum operation.
--
-- @foldWith 'async' $ map return [1..3]@
--
-- @since 0.1.0
{-# INLINABLE foldWith #-}
foldWith :: (IsStream t, Foldable f)
    => (t m a -> t m a -> t m a) -> f (t m a) -> t m a
foldWith f = Prelude.foldr f K.nil

-- | A variant of 'foldMap' that allows you to map a monadic streaming action
-- on a 'Foldable' container and then fold it using the specified stream sum
-- operation.
--
-- @foldMapWith 'async' return [1..3]@
--
-- @since 0.1.0
{-# INLINABLE foldMapWith #-}
foldMapWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> (a -> t m b) -> f a -> t m b
foldMapWith f g = Prelude.foldr (f . g) K.nil

-- | Like 'foldMapWith' but with the last two arguments reversed i.e. the
-- monadic streaming function is the last argument.
--
-- @since 0.1.0
{-# INLINABLE forEachWith #-}
forEachWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> f a -> (a -> t m b) -> t m b
forEachWith f xs g = Prelude.foldr (f . g) K.nil xs
