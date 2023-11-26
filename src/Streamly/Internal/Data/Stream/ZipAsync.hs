{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.ZipAsync
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run examples in this module:
--
-- >>> import qualified Streamly.Prelude as Stream
--
module Streamly.Internal.Data.Stream.ZipAsync
    {-# DEPRECATED "Use \"Streamly.Data.Stream.MkType\" instead." #-}
    ( ZipAsyncM(..)
    , ZipAsync
    , consMZipAsync
    , zipAsyncWithK
    , zipAsyncWithMK
    )
where

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.StreamK (Stream)

import qualified Streamly.Internal.Data.StreamK as K
    (mkStream, foldStream, zipWithM, consM)
import qualified Streamly.Internal.Data.Stream as D (fromStreamK)
import qualified Streamly.Internal.Data.Stream.Serial as Serial
import qualified Streamly.Internal.Data.Stream.SVar.Eliminate as SVar
import qualified Streamly.Internal.Data.Stream.SVar.Generate as SVar
import qualified Streamly.Internal.Data.Stream.Serial as Stream
    (fromStreamK, toStreamK)
import Streamly.Internal.Data.SVar

import Prelude hiding (map, repeat, zipWith, errorWithoutStackTrace)

#include "Instances.hs"

-- $setup
-- >>> :set -fno-warn-deprecations
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import Control.Concurrent (threadDelay)
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}

------------------------------------------------------------------------------
-- Parallel Zipping
------------------------------------------------------------------------------

-- | Like 'zipAsyncWith' but with a monadic zipping function.
--
-- @since 0.4.0
{-# INLINE zipAsyncWithMK #-}
zipAsyncWithMK :: MonadAsync m
    => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
zipAsyncWithMK f m1 m2 = K.mkStream $ \st yld sng stp -> do
    sv <- newParallelVar StopNone (adaptState st)
    SVar.toSVarParallel (adaptState st) sv $ D.fromStreamK m2
    K.foldStream st yld sng stp
        $ K.zipWithM f m1 (Stream.toStreamK (SVar.fromSVar sv))

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
    fmap f (ZipAsyncM m) =
        ZipAsyncM $ Stream.toStreamK $ fmap f (Stream.fromStreamK m)

instance MonadAsync m => Applicative (ZipAsyncM m) where
    pure = ZipAsyncM . Stream.toStreamK . Serial.repeat

    {-# INLINE (<*>) #-}
    ZipAsyncM m1 <*> ZipAsyncM m2 = ZipAsyncM $ zipAsyncWithK id m1 m2
