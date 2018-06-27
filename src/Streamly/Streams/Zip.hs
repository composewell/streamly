{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE StandaloneDeriving        #-}
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
      zipWith
    , zipWithM
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

import Data.Semigroup (Semigroup(..))
import Prelude hiding (map, repeat, zipWith)

import Streamly.Streams.StreamK (IsStream(..), Stream(..))
import Streamly.Streams.Async (mkAsync)
import Streamly.Streams.Serial (map)
import Streamly.SVar (MonadAsync)

import qualified Streamly.Streams.StreamK as K

#include "Instances.hs"

------------------------------------------------------------------------------
-- Serial Zipping
------------------------------------------------------------------------------

{-# INLINE zipWithS #-}
zipWithS :: (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWithS f m1 m2 = go m1 m2
    where
    go mx my = Stream $ \_ stp sng yld -> do
        let merge a ra =
                let single2 b = sng (f a b)
                    yield2 b rb = yld (f a b) (go ra rb)
                 in unStream my Nothing stp single2 yield2
        let single1 a   = merge a K.nil
            yield1 a ra = merge a ra
        unStream mx Nothing stp single1 yield1

-- | Zip two streams serially using a pure zipping function.
--
-- @since 0.1.0
{-# INLINABLE zipWith #-}
zipWith :: IsStream t => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f m1 m2 = fromStream $ zipWithS f (toStream m1) (toStream m2)

-- | Zip two streams serially using a monadic zipping function.
--
-- @since 0.1.0
zipWithM :: (IsStream t, Monad m) => (a -> b -> m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \_ stp sng yld -> do
        let merge a ra =
                let runIt x = unStream x Nothing stp sng yld
                    single2 b   = f a b >>= sng
                    yield2 b rb = f a b >>= \x -> runIt (x `K.cons` go ra rb)
                 in unStream my Nothing stp single2 yield2
        let single1 a  = merge a K.nil
            yield1 a ra = merge a ra
        unStream mx Nothing stp single1 yield1

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
type ZipSerial a = ZipSerialM IO a

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

instance IsStream ZipSerialM where
    toStream = getZipSerialM
    fromStream = ZipSerialM

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> ZipSerialM IO a -> ZipSerialM IO a #-}
    consM :: Monad m => m a -> ZipSerialM m a -> ZipSerialM m a
    consM m r = fromStream $ K.consMSerial m (toStream r)

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> ZipSerialM IO a -> ZipSerialM IO a #-}
    (|:) :: Monad m => m a -> ZipSerialM m a -> ZipSerialM m a
    m |: r = fromStream $ K.consMSerial m (toStream r)

instance Monad m => Functor (ZipSerialM m) where
    fmap = map

instance Monad m => Applicative (ZipSerialM m) where
    pure = ZipSerialM . K.repeat
    m1 <*> m2 = fromStream $ zipWith id (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Parallel Zipping
------------------------------------------------------------------------------

-- | Zip two streams concurrently (i.e. both the elements being zipped are
-- generated concurrently) using a pure zipping function.
--
-- @since 0.1.0
zipAsyncWith :: (IsStream t, MonadAsync m)
    => (a -> b -> c) -> t m a -> t m b -> t m c
zipAsyncWith f m1 m2 = fromStream $ Stream $ \_ stp sng yld -> do
    ma <- mkAsync m1
    mb <- mkAsync m2
    unStream (toStream (zipWith f ma mb)) Nothing stp sng yld

-- | Zip two streams asyncly (i.e. both the elements being zipped are generated
-- concurrently) using a monadic zipping function.
--
-- @since 0.4.0
zipAsyncWithM :: (IsStream t, MonadAsync m)
    => (a -> b -> m c) -> t m a -> t m b -> t m c
zipAsyncWithM f m1 m2 = fromStream $ Stream $ \_ stp sng yld -> do
    ma <- mkAsync m1
    mb <- mkAsync m2
    unStream (toStream (zipWithM f ma mb)) Nothing stp sng yld

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
type ZipAsync a = ZipAsyncM IO a

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
instance IsStream ZipAsyncM where
    toStream = getZipAsyncM
    fromStream = ZipAsyncM

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> ZipAsyncM IO a -> ZipAsyncM IO a #-}
    consM :: Monad m => m a -> ZipAsyncM m a -> ZipAsyncM m a
    consM m r = fromStream $ K.consMSerial m (toStream r)

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> ZipAsyncM IO a -> ZipAsyncM IO a #-}
    (|:) :: Monad m => m a -> ZipAsyncM m a -> ZipAsyncM m a
    m |: r = fromStream $ K.consMSerial m (toStream r)

instance Monad m => Functor (ZipAsyncM m) where
    fmap = map

instance MonadAsync m => Applicative (ZipAsyncM m) where
    pure = ZipAsyncM . K.repeat
    m1 <*> m2 = zipAsyncWith id m1 m2
