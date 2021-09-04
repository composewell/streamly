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
      ZipSerialM (..)
    , ZipSerial
    , consMZip
    , zipWithK
    , zipWithMK

    , ZipAsyncM(..)
    , ZipAsync
    , consMZipAsync
    , zipAsyncWithK
    , zipAsyncWithMK

    -- * Deprecated
    , ZipStream
    )
where

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
import Streamly.Internal.BaseCompat ((#.), errorWithoutStackTrace, oneShot)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.Stream.StreamK.Type (Stream)

import qualified Streamly.Internal.Data.Stream.Parallel as Par
import qualified Streamly.Internal.Data.Stream.Prelude as P
    (cmpBy, eqBy, foldl', foldr, fromList, toList)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.Serial as Serial

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

{-# INLINE zipWithMK #-}
zipWithMK :: Monad m =>
    (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipWithMK f m1 m2 =
    D.toStreamK $ D.zipWithM f (D.fromStreamK m1) (D.fromStreamK m2)

{-# INLINE zipWithK #-}
zipWithK :: Monad m
    => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWithK f = zipWithMK (\a b -> return (f a b))

------------------------------------------------------------------------------
-- Parallel Zipping
------------------------------------------------------------------------------

-- | Like 'zipAsyncWith' but with a monadic zipping function.
--
-- @since 0.4.0
{-# INLINE zipAsyncWithMK #-}
zipAsyncWithMK :: MonadAsync m
    => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipAsyncWithMK f m1 m2 = D.toStreamK $
    D.zipWithM f (Par.mkParallelD $ D.fromStreamK m1)
                 (Par.mkParallelD $ D.fromStreamK m2)

-- XXX Should we rename this to zipParWith or zipParallelWith? This can happen
-- along with the change of behvaior to end the stream concurrently.
--
-- | Like 'zipWith' but zips concurrently i.e. both the streams being zipped
-- are evaluated concurrently using the 'ParallelT' concurrent evaluation
-- style. The maximum number of elements of each stream evaluated in advance
-- can be controlled by 'maxBuffer'.
--
-- The stream ends if stream @a@ or stream @b@ ends. However, if stream @b@
-- ends while we are still evaluating stream @a@ and waiting for a result then
-- stream will not end until after the evaluation of stream @a@ finishes. This
-- behavior can potentially be changed in future to end the stream immediately
-- as soon as any of the stream end is detected.
--
-- @since 0.1.0
{-# INLINE zipAsyncWithK #-}
zipAsyncWithK :: MonadAsync m
    => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipAsyncWithK f = zipAsyncWithMK (\a b -> return (f a b))

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

consMZip :: Monad m => m a -> ZipSerialM m a -> ZipSerialM m a
consMZip m (ZipSerialM r) = ZipSerialM $ K.consM m r

LIST_INSTANCES(ZipSerialM)
NFDATA1_INSTANCE(ZipSerialM)

instance Monad m => Functor (ZipSerialM m) where
    {-# INLINE fmap #-}
    fmap f (ZipSerialM m) = ZipSerialM $ getSerialT $ fmap f (SerialT m)

instance Monad m => Applicative (ZipSerialM m) where
    pure = ZipSerialM . getSerialT . Serial.repeat

    {-# INLINE (<*>) #-}
    ZipSerialM m1 <*> ZipSerialM m2 = ZipSerialM $ zipWithK id m1 m2

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

consMZipAsync :: Monad m => m a -> ZipAsyncM m a -> ZipAsyncM m a
consMZipAsync m (ZipAsyncM r) = ZipAsyncM $ K.consM m r

instance Monad m => Functor (ZipAsyncM m) where
    {-# INLINE fmap #-}
    fmap f (ZipAsyncM m) = ZipAsyncM $ getSerialT $ fmap f (SerialT m)

instance MonadAsync m => Applicative (ZipAsyncM m) where
    pure = ZipAsyncM . getSerialT . Serial.repeat

    {-# INLINE (<*>) #-}
    ZipAsyncM m1 <*> ZipAsyncM m2 = ZipAsyncM $ zipAsyncWithK id m1 m2
