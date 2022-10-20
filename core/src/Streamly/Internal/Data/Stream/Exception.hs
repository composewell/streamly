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
    , after_
    , afterIO
    , bracket_
    , bracketIO
    , bracket3IO
    , onException
    , finally_
    , finallyIO
    , ghandle
    , handle
    , retry
    )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
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

-- | Like 'after', with following differences:
--
-- * action @m b@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * Monad @m@ does not require any other constraints.
-- * has slightly better performance than 'after'.
--
-- Same as the following, but with stream fusion:
--
-- >>> after_ action xs = xs <> Stream.nilM action
--
-- /Pre-release/
--
{-# INLINE after_ #-}
after_ :: Monad m => m b -> Stream m a -> Stream m a
after_ action xs = fromStreamD $ D.after_ action $ toStreamD xs

-- | Run the action @IO b@ whenever the stream stream stops normally, or
-- if it is garbage collected after a partial lazy evaluation.
--
-- The semantics of the action @IO b@ are similar to the semantics of cleanup
-- action in 'bracketIO'.
--
-- /See also 'after_'/
--
{-# INLINE afterIO #-}
afterIO :: MonadIO m => IO b -> Stream m a -> Stream m a
afterIO action xs = fromStreamD $ D.after action $ toStreamD xs

-- | Run the action @m b@ if the stream aborts due to an exception. The
-- exception is not caught, simply rethrown.
--
-- /Inhibits stream fusion/
--
{-# INLINE onException #-}
onException :: MonadCatch m => m b -> Stream m a -> Stream m a
onException action xs = fromStreamD $ D.onException action $ toStreamD xs

-- | Like 'finally' with following differences:
--
-- * action @m b@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * has slightly better performance than 'finallyIO'.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
--
{-# INLINE finally_ #-}
finally_ :: MonadCatch m => m b -> Stream m a -> Stream m a
finally_ action xs = fromStreamD $ D.finally_ action $ toStreamD xs

-- | Run the action @IO b@ whenever the stream stream stops normally, aborts
-- due to an exception or if it is garbage collected after a partial lazy
-- evaluation.
--
-- The semantics of running the action @IO b@ are similar to the cleanup action
-- semantics described in 'bracketIO'.
--
-- >>> finallyIO release = Stream.bracketIO (return ()) (const release)
--
-- /See also 'finally_'/
--
-- /Inhibits stream fusion/
--
{-# INLINE finallyIO #-}
finallyIO :: (MonadIO m, MonadCatch m) =>
    IO b -> Stream m a -> Stream m a
finallyIO action xs = fromStreamD $ D.finally action $ toStreamD xs

-- | Like 'bracket' but with following differences:
--
-- * alloc action @m b@ runs with async exceptions enabled
-- * cleanup action @b -> m c@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * has slightly better performance than 'bracketIO'.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
--
{-# INLINE bracket_ #-}
bracket_ :: MonadCatch m
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracket_ bef aft bet = fromStreamD $
    D.bracket_ bef aft (toStreamD . bet)

-- | Run the alloc action @IO b@ with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Use the
-- output @b@ as input to @b -> Stream m a@ to generate an output stream.
--
-- @b@ is usually a resource under the IO monad, e.g. a file handle, that
-- requires a cleanup after use. The cleanup action @b -> IO c@, runs whenever
-- the stream ends normally, due to a sync or async exception or if it gets
-- garbage collected after a partial lazy evaluation.
--
-- 'bracketIO' only guarantees that the cleanup action runs, and it runs with
-- async exceptions enabled. The action must ensure that it can successfully
-- cleanup the resource in the face of sync or async exceptions.
--
-- When the stream ends normally or on a sync exception, cleanup action runs
-- immediately in the current thread context, whereas in other cases it runs in
-- the GC context, therefore, cleanup may be delayed until the GC gets to run.
--
-- /See also: 'bracket_'/
--
-- /Inhibits stream fusion/
--
{-# INLINE bracketIO #-}
bracketIO :: (MonadIO m, MonadCatch m)
    => IO b -> (b -> IO c) -> (b -> Stream m a) -> Stream m a
bracketIO bef aft = bracket3IO bef aft aft aft

-- For a use case of this see the "streamly-process" package. It needs to kill
-- the process in case of exception or garbage collection, but waits for the
-- process to terminate in normal cases.

-- | Like 'bracketIO' but can use separate cleanup actions depending on the mode
-- of termination.  @bracket3IO before onStop onGC onException action@ runs
-- @action@ using the result of @before@. If the stream stops, @onStop@ action
-- is executed, if the stream is abandoned @onGC@ is executed, if the stream
-- encounters an exception @onException@ is executed.
--
-- /Pre-release/
{-# INLINE bracket3IO #-}
bracket3IO :: (MonadIO m, MonadCatch m)
    => IO b
    -> (b -> IO c)
    -> (b -> IO d)
    -> (b -> IO e)
    -> (b -> Stream m a)
    -> Stream m a
bracket3IO bef aft gc exc bet = fromStreamD $
    D.bracket' bef aft exc gc (toStreamD . bet)

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
