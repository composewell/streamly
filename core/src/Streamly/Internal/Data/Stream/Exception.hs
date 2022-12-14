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
    , afterUnsafe
    , afterIO
    , bracketUnsafe
    , bracketIO
    , bracketIO3
    , onException
    , finallyUnsafe
    , finallyIO
    , ghandle
    , handle
    )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
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
-- >>> afterUnsafe action xs = xs <> Stream.nilM action
--
-- /Pre-release/
--
{-# INLINE afterUnsafe #-}
afterUnsafe :: Monad m => m b -> Stream m a -> Stream m a
afterUnsafe action xs = fromStreamD $ D.after_ action $ toStreamD xs

-- | Run the action @IO b@ whenever the stream is evaluated to completion, or
-- if it is garbage collected after a partial lazy evaluation.
--
-- The semantics of the action @IO b@ are similar to the semantics of cleanup
-- action in 'bracketIO'.
--
-- /See also 'afterUnsafe'/
--
{-# INLINE afterIO #-}
afterIO :: MonadIO m => IO b -> Stream m a -> Stream m a
afterIO action xs = fromStreamD $ D.after action $ toStreamD xs

-- | Run the action @m b@ if the stream evaluation is aborted due to an
-- exception. The exception is not caught, simply rethrown.
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
{-# INLINE finallyUnsafe #-}
finallyUnsafe :: MonadCatch m => m b -> Stream m a -> Stream m a
finallyUnsafe action xs = fromStreamD $ D.finally_ action $ toStreamD xs

-- | Run the action @IO b@ whenever the stream stream stops normally, aborts
-- due to an exception or if it is garbage collected after a partial lazy
-- evaluation.
--
-- The semantics of running the action @IO b@ are similar to the cleanup action
-- semantics described in 'bracketIO'.
--
-- >>> finallyIO release = Stream.bracketIO (return ()) (const release)
--
-- /See also 'finallyUnsafe'/
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
{-# INLINE bracketUnsafe #-}
bracketUnsafe :: MonadCatch m
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracketUnsafe bef aft bet = fromStreamD $ D.bracket_ bef aft (toStreamD . bet)

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
-- /See also: 'bracketUnsafe'/
--
-- /Inhibits stream fusion/
--
{-# INLINE bracketIO #-}
bracketIO :: (MonadIO m, MonadCatch m)
    => IO b -> (b -> IO c) -> (b -> Stream m a) -> Stream m a
bracketIO bef aft = bracketIO3 bef aft aft aft

-- For a use case of this see the "streamly-process" package. It needs to kill
-- the process in case of exception or garbage collection, but waits for the
-- process to terminate in normal cases.

-- | Like 'bracketIO' but can use 3 separate cleanup actions depending on the
-- mode of termination:
--
-- 1. When the stream stops normally
-- 2. When the stream is garbage collected
-- 3. When the stream encounters an exception
--
-- @bracketIO3 before onStop onGC onException action@ runs @action@ using the
-- result of @before@. If the stream stops, @onStop@ action is executed, if the
-- stream is abandoned @onGC@ is executed, if the stream encounters an
-- exception @onException@ is executed.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
{-# INLINE bracketIO3 #-}
bracketIO3 :: (MonadIO m, MonadCatch m)
    => IO b
    -> (b -> IO c)
    -> (b -> IO d)
    -> (b -> IO e)
    -> (b -> Stream m a)
    -> Stream m a
bracketIO3 bef aft gc exc bet = fromStreamD $
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
