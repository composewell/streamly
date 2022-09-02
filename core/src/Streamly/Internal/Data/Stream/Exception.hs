-- |
-- Module      : Streamly.Internal.Data.Stream.Exception
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Exception
    (
      before
    , onException
    , ghandle
    , handle
    , retry
    )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch)
import Data.Map.Strict (Map)
import Streamly.Internal.Data.Stream.Type (Stream, fromStreamD, toStreamD)

import qualified Streamly.Internal.Data.Stream.StreamD as D


-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Stream as Stream

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Run the action @m b@ before the stream yields its first element.
--
-- Same as the following but more efficient due to fusion:
--
-- >>> before action xs = Stream.nilM action <> xs
-- >>> before action xs = Stream.concatMap (const xs) (Stream.fromEffect action)
--
{-# INLINE before #-}
before :: Monad m => m b -> Stream m a -> Stream m a
before action xs = fromStreamD $ D.before action $ toStreamD xs

-- | Run the action @m b@ if the stream aborts due to an exception. The
-- exception is not caught, simply rethrown.
--
-- /Inhibits stream fusion/
--
{-# INLINE onException #-}
onException :: MonadCatch m => m b -> Stream m a -> Stream m a
onException action xs = fromStreamD $ D.onException action $ toStreamD xs

-- | Like 'handle' but the exception handler is also provided with the stream
-- that generated the exception as input. The exception handler can thus
-- re-evaluate the stream to retry the action that failed. The exception
-- handler can again call 'ghandle' on it to retry the action multiple times.
--
-- This is highly experimental. In a stream of actions we can map the stream
-- with a retry combinator to retry each action on failure.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
--
{-# INLINE ghandle #-}
ghandle :: (MonadCatch m, Exception e)
    => (e -> Stream m a -> Stream m a) -> Stream m a -> Stream m a
ghandle handler =
      fromStreamD
    . D.ghandle (\e xs -> toStreamD $ handler e (fromStreamD xs))
    . toStreamD

-- | When evaluating a stream if an exception occurs, stream evaluation aborts
-- and the specified exception handler is run with the exception as argument.
--
-- /Inhibits stream fusion/
--
{-# INLINE handle #-}
handle :: (MonadCatch m, Exception e)
    => (e -> Stream m a) -> Stream m a -> Stream m a
handle handler xs =
    fromStreamD $ D.handle (toStreamD . handler) $ toStreamD xs


-- | @retry@ takes 3 arguments
--
-- 1. A map @m@ whose keys are exceptions and values are the number of times to
-- retry the action given that the exception occurs.
--
-- 2. A handler @han@ that decides how to handle an exception when the exception
-- cannot be retried.
--
-- 3. The stream itself that we want to run this mechanism on.
--
-- When evaluating a stream if an exception occurs,
--
-- 1. The stream evaluation aborts
--
-- 2. The exception is looked up in @m@
--
--    a. If the exception exists and the mapped value is > 0 then,
--
--       i. The value is decreased by 1.
--
--       ii. The stream is resumed from where the exception was called, retrying
--       the action.
--
--    b. If the exception exists and the mapped value is == 0 then the stream
--    evaluation stops.
--
--    c. If the exception does not exist then we handle the exception using
--    @han@.
--
-- /Internal/
--
{-# INLINE retry #-}
retry :: (MonadCatch m, Exception e, Ord e)
    => Map e Int
       -- ^ map from exception to retry count
    -> (e -> Stream m a)
       -- ^ default handler for those exceptions that are not in the map
    -> Stream m a
    -> Stream m a
retry emap handler inp =
    fromStreamD $ D.retry emap (toStreamD . handler) $ toStreamD inp
