{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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
-- >>> import qualified Streamly.Internal.Data.Stream as D
-- >>> import qualified Streamly.Data.Fold as Fold
--
module Streamly.Internal.Data.Stream.Zip
    {-# DEPRECATED "Use \"Streamly.Data.Stream.MkType\" instead." #-}
    (
      ZipSerialM (..)
    , ZipSerial
    , consMZip
    , zipWithK
    , zipWithMK

    , ZipConcurrent (..)

    -- * Deprecated
    , ZipStream
    )
where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
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
import GHC.Exts (IsList(..), IsString(..), oneShot)
import Text.Read
       ( Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec
       , readListPrecDefault)
import Streamly.Internal.BaseCompat ((#.))
import Streamly.Internal.Data.Maybe.Strict (Maybe'(..), toMaybe)
import Streamly.Internal.Data.Stream.Serial (SerialT(..))
import Streamly.Internal.Data.StreamK (Stream)
import Streamly.Internal.Data.Stream.Concurrent (MonadAsync, parZipWith)

import qualified Streamly.Internal.Data.Stream.Common as P
import qualified Streamly.Internal.Data.StreamK as K
import qualified Streamly.Internal.Data.Stream as D
import qualified Streamly.Internal.Data.Stream.Serial as Serial

import Prelude hiding (map, repeat, zipWith)

#include "Instances.hs"

-- $setup
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream as D
-- >>> import qualified Streamly.Data.Fold as Fold
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

-------------------------------------------------------------------------------
-- ZipConcurrent
-------------------------------------------------------------------------------

newtype ZipConcurrent m a = ZipConcurrent {getZipConcurrent :: D.Stream m a}
      deriving (Functor)

-- | An IO stream whose applicative instance zips streams concurrently. Note
-- that it uses the default concurrency options.
--
-- >>> s = ZipConcurrent $ D.fromList [1, 2, 3]
-- >>> x = (,,) <$> s <*> s <*> s
-- >>> D.fold Fold.toList (getZipConcurrent x)
-- [(1,1,1),(2,2,2),(3,3,3)]
--
-- @since 0.9.0

instance MonadAsync m => Applicative (ZipConcurrent m) where
    pure = ZipConcurrent . D.repeat

    {-# INLINE (<*>) #-}
    ZipConcurrent m1 <*> ZipConcurrent m2 =
        ZipConcurrent $ parZipWith id id m1 m2
