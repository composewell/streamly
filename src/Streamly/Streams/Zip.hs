{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Streams.Zip
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Streams.Zip
    (
      K.zipWith
    , K.zipWithM
    , zipAsyncWith
    , zipAsyncWithM

    , ZipSerialM
    , ZipSerial
    , ZipStream         -- deprecated
    , zipSerially
    , zipping          -- deprecated

    , ZipAsyncM
    , ZipAsync
    , zipAsyncly
    , zippingAsync     -- deprecated
    )
where

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..))
#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1(..))
#endif
import Data.Functor.Identity (Identity, runIdentity)
import Data.Foldable (fold)
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import GHC.Exts (IsList(..), IsString(..))
import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec,
                  readListPrecDefault)
import Prelude hiding (map, repeat, zipWith)

import Streamly.Streams.StreamK (IsStream(..), Stream, mkStream, foldStream)
import Streamly.Streams.Async (mkAsync')
import Streamly.Streams.Serial (map)
import Streamly.Internal.Data.SVar (MonadAsync, adaptState)

import qualified Streamly.Streams.Prelude as P
import qualified Streamly.Streams.StreamK as K

#include "Instances.hs"

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | The applicative instance of 'ZipSerialM' zips a number of streams serially
-- i.e. it produces one element from each stream serially and then zips all
-- those elements.
--
-- @
-- main = (toList . 'zipSerially' $ (,,) \<$\> s1 \<*\> s2 \<*\> s3) >>= print
--     where s1 = fromFoldable [1, 2]
--           s2 = fromFoldable [3, 4]
--           s3 = fromFoldable [5, 6]
-- @
-- @
-- [(1,3,5),(2,4,6)]
-- @
--
-- The 'Semigroup' instance of this type works the same way as that of
-- 'SerialT'.
--
-- @since 0.2.0
newtype ZipSerialM m a = ZipSerialM {getZipSerialM :: Stream m a}
        deriving (Semigroup, Monoid)

-- |
-- @since 0.1.0
{-# DEPRECATED ZipStream "Please use 'ZipSerialM' instead." #-}
type ZipStream = ZipSerialM

-- | An IO stream whose applicative instance zips streams serially.
--
-- @since 0.2.0
type ZipSerial = ZipSerialM IO

-- | Fix the type of a polymorphic stream as 'ZipSerialM'.
--
-- @since 0.2.0
zipSerially :: IsStream t => ZipSerialM m a -> t m a
zipSerially = K.adapt

-- | Same as 'zipSerially'.
--
-- @since 0.1.0
{-# DEPRECATED zipping "Please use zipSerially instead." #-}
zipping :: IsStream t => ZipSerialM m a -> t m a
zipping = zipSerially

consMZip :: Monad m => m a -> ZipSerialM m a -> ZipSerialM m a
consMZip m ms = fromStream $ K.consMStream m (toStream ms)

instance IsStream ZipSerialM where
    toStream = getZipSerialM
    fromStream = ZipSerialM

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> ZipSerialM IO a -> ZipSerialM IO a #-}
    consM :: Monad m => m a -> ZipSerialM m a -> ZipSerialM m a
    consM = consMZip

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> ZipSerialM IO a -> ZipSerialM IO a #-}
    (|:) :: Monad m => m a -> ZipSerialM m a -> ZipSerialM m a
    (|:) = consMZip

LIST_INSTANCES(ZipSerialM)
NFDATA1_INSTANCE(ZipSerialM)

instance Monad m => Functor (ZipSerialM m) where
    fmap = map

instance Monad m => Applicative (ZipSerialM m) where
    pure = ZipSerialM . K.repeat
    (<*>) = K.zipWith id

FOLDABLE_INSTANCE(ZipSerialM)
TRAVERSABLE_INSTANCE(ZipSerialM)

------------------------------------------------------------------------------
-- Parallel Zipping
------------------------------------------------------------------------------

-- | Like 'zipWith' but zips concurrently i.e. both the streams being zipped
-- are generated concurrently.
--
-- @since 0.1.0
{-# INLINABLE zipAsyncWith #-}
zipAsyncWith :: (IsStream t, MonadAsync m)
    => (a -> b -> c) -> t m a -> t m b -> t m c
zipAsyncWith f m1 m2 = mkStream $ \st stp sng yld -> do
    ma <- mkAsync' (adaptState st) m1
    mb <- mkAsync' (adaptState st) m2
    foldStream st stp sng yld (K.zipWith f ma mb)

-- | Like 'zipWithM' but zips concurrently i.e. both the streams being zipped
-- are generated concurrently.
--
-- @since 0.4.0
{-# INLINABLE zipAsyncWithM #-}
zipAsyncWithM :: (IsStream t, MonadAsync m)
    => (a -> b -> m c) -> t m a -> t m b -> t m c
zipAsyncWithM f m1 m2 = mkStream $ \st stp sng yld -> do
    ma <- mkAsync' (adaptState st) m1
    mb <- mkAsync' (adaptState st) m2
    foldStream st stp sng yld (K.zipWithM f ma mb)

------------------------------------------------------------------------------
-- Parallely Zipping Streams
------------------------------------------------------------------------------
--
-- | Like 'ZipSerialM' but zips in parallel, it generates all the elements to
-- be zipped concurrently.
--
-- @
-- main = (toList . 'zipAsyncly' $ (,,) \<$\> s1 \<*\> s2 \<*\> s3) >>= print
--     where s1 = fromFoldable [1, 2]
--           s2 = fromFoldable [3, 4]
--           s3 = fromFoldable [5, 6]
-- @
-- @
-- [(1,3,5),(2,4,6)]
-- @
--
-- The 'Semigroup' instance of this type works the same way as that of
-- 'SerialT'.
--
-- @since 0.2.0
newtype ZipAsyncM m a = ZipAsyncM {getZipAsyncM :: Stream m a}
        deriving (Semigroup, Monoid)

-- | An IO stream whose applicative instance zips streams wAsyncly.
--
-- @since 0.2.0
type ZipAsync = ZipAsyncM IO

-- | Fix the type of a polymorphic stream as 'ZipAsyncM'.
--
-- @since 0.2.0
zipAsyncly :: IsStream t => ZipAsyncM m a -> t m a
zipAsyncly = K.adapt

-- | Same as 'zipAsyncly'.
--
-- @since 0.1.0
{-# DEPRECATED zippingAsync "Please use zipAsyncly instead." #-}
zippingAsync :: IsStream t => ZipAsyncM m a -> t m a
zippingAsync = zipAsyncly

consMZipAsync :: Monad m => m a -> ZipAsyncM m a -> ZipAsyncM m a
consMZipAsync m ms = fromStream $ K.consMStream m (toStream ms)

instance IsStream ZipAsyncM where
    toStream = getZipAsyncM
    fromStream = ZipAsyncM

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> ZipAsyncM IO a -> ZipAsyncM IO a #-}
    consM :: Monad m => m a -> ZipAsyncM m a -> ZipAsyncM m a
    consM = consMZipAsync

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> ZipAsyncM IO a -> ZipAsyncM IO a #-}
    (|:) :: Monad m => m a -> ZipAsyncM m a -> ZipAsyncM m a
    (|:) = consMZipAsync

instance Monad m => Functor (ZipAsyncM m) where
    fmap = map

instance MonadAsync m => Applicative (ZipAsyncM m) where
    pure = ZipAsyncM . K.repeat
    m1 <*> m2 = zipAsyncWith id m1 m2
