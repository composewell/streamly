{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.Zip
-- Copyright   : (c) 2017 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run examples in this module:
--
-- >>> import qualified Streamly.Prelude as Stream
--
module Streamly.Internal.Data.Stream.Zip
    (
      ZipSerialM
    , ZipSerial
    , fromZipSerial

    , ZipAsyncM
    , ZipAsync
    , fromZipAsync

    , zipWith
    , zipWithM
    , zipAsyncWith
    , zipAsyncWithM

    -- * Deprecated
    , ZipStream
    , zipping
    , zippingAsync
    )
where

#include "inline.hs"

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData(..))
#if MIN_VERSION_deepseq(1,4,3)
import Control.DeepSeq (NFData1(..))
#endif
import Data.Foldable (Foldable(foldl'), fold)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Endo(..))
#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif
import GHC.Exts (IsList(..), IsString(..))
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)
import Streamly.Internal.BaseCompat ((#.), errorWithoutStackTrace)
import Streamly.Internal.Data.Stream.StreamK (IsStream(..), Stream)
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.SVar (MonadAsync)

import Streamly.Internal.Data.Stream.Serial (SerialT)

import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Prelude as P
    (cmpBy, eqBy, foldl', foldr, fromList, toList)
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Stream.StreamD as D

import Prelude hiding (map, repeat, zipWith, errorWithoutStackTrace)

#include "Instances.hs"

-- $setup
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Control.Concurrent (threadDelay)
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}

-- | Like 'zipWith' but using a monadic zipping function.
--
-- @since 0.4.0
{-# INLINE_EARLY zipWithM #-}
zipWithM ::
       (IsStream t, MonadAsync m) => (a -> b -> m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 =
    K.fromStream $ K.zipWithM f (K.toStream m1) (K.toStream m2)

{-# RULES "zipWithM serial" zipWithM = zipWithMSerial #-}
{-# INLINE zipWithMSerial #-}
zipWithMSerial ::
       Monad m => (a -> b -> m c) -> SerialT m a -> SerialT m b -> SerialT m c
zipWithMSerial f m1 m2 =
    D.fromStreamD $ D.zipWithM f (D.toStreamD m1) (D.toStreamD m2)

-- | Zip two streams serially using a pure zipping function. The zipping
-- function is applied concurrently for concurrent streams.
--
-- @
-- > S.toList $ S.zipWith (+) (S.fromList [1,2,3]) (S.fromList [4,5,6])
-- [5,7,9]
-- @
--
-- @since 0.1.0
{-# INLINE_EARLY zipWith #-}
zipWith ::
       (IsStream t, MonadAsync m) => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f m1 m2 = K.fromStream $ K.zipWith f (K.toStream m1) (K.toStream m2)

{-# RULES "zipWith serial" zipWith = zipWithSerial #-}
{-# INLINE zipWithSerial #-}
zipWithSerial ::
       Monad m => (a -> b -> c) -> SerialT m a -> SerialT m b -> SerialT m c
zipWithSerial f m1 m2 =
    D.fromStreamD $ D.zipWith f (D.toStreamD m1) (D.toStreamD m2)

------------------------------------------------------------------------------
-- Parallel Zipping
------------------------------------------------------------------------------

-- | Like 'zipWithM' but zips concurrently i.e. both the streams being zipped
-- are generated concurrently.
--
-- @since 0.4.0
{-# INLINE zipAsyncWithM #-}
zipAsyncWithM :: (IsStream t, MonadAsync m)
    => (a -> b -> m c) -> t m a -> t m b -> t m c
zipAsyncWithM f m1 m2 = D.fromStreamD $
    D.zipWithM f (Par.mkParallelD $ D.toStreamD m1)
                 (Par.mkParallelD $ D.toStreamD m2)

-- | Like 'zipWith' but zips concurrently i.e. both the streams being zipped
-- are generated concurrently.
--
-- @since 0.1.0
{-# INLINE zipAsyncWith #-}
zipAsyncWith :: (IsStream t, MonadAsync m)
    => (a -> b -> c) -> t m a -> t m b -> t m c
zipAsyncWith f = zipAsyncWithM (\a b -> return (f a b))

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | For 'ZipSerialM' streams:
--
-- @
-- (<>) = 'Streamly.Prelude.serial'
-- (<*>) = 'Streamly.Prelude.serial.zipWith' id
-- @
--
-- Applicative evaluates the streams being zipped serially:
--
-- >>> s1 = Stream.fromFoldable [1, 2]
-- >>> s2 = Stream.fromFoldable [3, 4]
-- >>> s3 = Stream.fromFoldable [5, 6]
-- >>> Stream.toList $ Stream.fromZipSerial $ (,,) <$> s1 <*> s2 <*> s3
-- [(1,3,5),(2,4,6)]
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
newtype ZipSerialM m a = ZipSerialM {getZipSerialM :: Stream m a}
        deriving (Semigroup, Monoid)

-- |
-- @since 0.1.0
{-# DEPRECATED ZipStream "Please use 'ZipSerialM' instead." #-}
type ZipStream = ZipSerialM

-- | An IO stream whose applicative instance zips streams serially.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type ZipSerial = ZipSerialM IO

-- | Fix the type of a polymorphic stream as 'ZipSerialM'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
fromZipSerial :: IsStream t => ZipSerialM m a -> t m a
fromZipSerial = K.adapt

-- | Same as 'fromZipSerial'.
--
-- @since 0.1.0
{-# DEPRECATED zipping "Please use fromZipSerial instead." #-}
zipping :: IsStream t => ZipSerialM m a -> t m a
zipping = fromZipSerial

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
    {-# INLINE fmap #-}
    fmap f (ZipSerialM m) = D.fromStreamD $ D.mapM (return . f) $ D.toStreamD m

instance Monad m => Applicative (ZipSerialM m) where
    pure = ZipSerialM . K.repeat
    {-# INLINE (<*>) #-}
    m1 <*> m2 = fromStream $ toStream $ zipWithSerial id m1_ m2_
        where
        m1_ = fromStream (toStream m1)
        m2_ = fromStream (toStream m2)

FOLDABLE_INSTANCE(ZipSerialM)
TRAVERSABLE_INSTANCE(ZipSerialM)

------------------------------------------------------------------------------
-- Parallely Zipping Streams
------------------------------------------------------------------------------
--
-- | For 'ZipAsyncM' streams:
--
-- @
-- (<>) = 'Streamly.Prelude.serial'
-- (<*>) = 'Streamly.Prelude.serial.zipAsyncWith' id
-- @
--
-- Applicative evaluates the streams being zipped concurrently, the following
-- would take half the time that it would take in serial zipping:
--
-- >>> s = Stream.fromFoldableM $ Prelude.map delay [1, 1, 1]
-- >>> Stream.toList $ Stream.fromZipAsync $ (,) <$> s <*> s
-- ...
-- [(1,1),(1,1),(1,1)]
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
newtype ZipAsyncM m a = ZipAsyncM {getZipAsyncM :: Stream m a}
        deriving (Semigroup, Monoid)

-- | An IO stream whose applicative instance zips streams wAsyncly.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
type ZipAsync = ZipAsyncM IO

-- | Fix the type of a polymorphic stream as 'ZipAsyncM'.
--
-- /Since: 0.2.0 ("Streamly")/
--
-- @since 0.8.0
fromZipAsync :: IsStream t => ZipAsyncM m a -> t m a
fromZipAsync = K.adapt

-- | Same as 'fromZipAsync'.
--
-- @since 0.1.0
{-# DEPRECATED zippingAsync "Please use fromZipAsync instead." #-}
zippingAsync :: IsStream t => ZipAsyncM m a -> t m a
zippingAsync = fromZipAsync

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
    {-# INLINE fmap #-}
    fmap f (ZipAsyncM m) = D.fromStreamD $ D.mapM (return . f) $ D.toStreamD m

instance MonadAsync m => Applicative (ZipAsyncM m) where
    pure = ZipAsyncM . K.repeat
    {-# INLINE (<*>) #-}
    m1 <*> m2 = zipAsyncWith id m1 m2
