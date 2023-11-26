{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module      : Streamly.Internal.Data.Stream.IsStream.Exception
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.IsStream.Exception {-# DEPRECATED "Please use \"Streamly.Data.Stream.*\" instead." #-}
    (
      before
    , after_
    , after
    , bracket_
    , bracket
    , bracket'
    , onException
    , finally_
    , finally
    , ghandle
    , handle
    , retry
    )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch)
import Data.Map.Strict (Map)
import Streamly.Internal.Control.Concurrent (MonadRunInIO, MonadAsync)
import Streamly.Internal.Data.Stream.IsStream.Type
    (IsStream(..), fromStreamD, toStreamD)

import qualified Streamly.Internal.Data.Stream as D
    ( before
    , afterUnsafe
    , onException
    , bracketUnsafe
    , finallyUnsafe
    , ghandle
    , handle
    )

import qualified Streamly.Internal.Data.Stream.Lifted as D
    ( afterD
    , bracket3D
    , retryD
    )


-- $setup
-- >>> :m
-- >>> :set -fno-warn-deprecations
-- >>> import qualified Streamly.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream.IsStream as Stream (nilM)

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
-- @since 0.7.0
{-# INLINE before #-}
before :: (IsStream t, Monad m) => m b -> t m a -> t m a
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
-- > after_ action xs = xs <> 'nilM' action
--
-- /Pre-release/
--
{-# INLINE after_ #-}
after_ :: (IsStream t, Monad m) => m b -> t m a -> t m a
after_ action xs = fromStreamD $ D.afterUnsafe action $ toStreamD xs

-- | Run the action @m b@ whenever the stream @t m a@ stops normally, or if it
-- is garbage collected after a partial lazy evaluation.
--
-- The semantics of the action @m b@ are similar to the semantics of cleanup
-- action in 'bracket'.
--
-- /See also 'after_'/
--
-- @since 0.7.0
--
{-# INLINE after #-}
after :: (IsStream t, MonadRunInIO m)
    => m b -> t m a -> t m a
after action xs = fromStreamD $ D.afterD action $ toStreamD xs

-- | Run the action @m b@ if the stream aborts due to an exception. The
-- exception is not caught, simply rethrown.
--
-- /Inhibits stream fusion/
--
-- @since 0.7.0
{-# INLINE onException #-}
onException :: (IsStream t, MonadCatch m) => m b -> t m a -> t m a
onException action xs = fromStreamD $ D.onException action $ toStreamD xs

-- | Like 'finally' with following differences:
--
-- * action @m b@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * does not require a 'MonadAsync' constraint.
-- * has slightly better performance than 'finally'.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
--
{-# INLINE finally_ #-}
finally_ :: (IsStream t, MonadCatch m) => m b -> t m a -> t m a
finally_ action xs = fromStreamD $ D.finallyUnsafe action $ toStreamD xs

-- | Run the action @m b@ whenever the stream @t m a@ stops normally, aborts
-- due to an exception or if it is garbage collected after a partial lazy
-- evaluation.
--
-- The semantics of running the action @m b@ are similar to the cleanup action
-- semantics described in 'bracket'.
--
-- /See also 'finally_'/
--
-- /Inhibits stream fusion/
--
-- @since 0.7.0
--
{-# INLINE finally #-}
finally :: (IsStream t, MonadAsync m, MonadCatch m) => m b -> t m a -> t m a
finally action xs = bracket (return ()) (const action) (const xs)

-- | Like 'bracket' but with following differences:
--
-- * alloc action @m b@ runs with async exceptions enabled
-- * cleanup action @b -> m c@ won't run if the stream is garbage collected
--   after partial evaluation.
-- * does not require a 'MonadAsync' constraint.
-- * has slightly better performance than 'bracket'.
--
-- /Inhibits stream fusion/
--
-- /Pre-release/
--
{-# INLINE bracket_ #-}
bracket_ :: (IsStream t, MonadCatch m)
    => m b -> (b -> m c) -> (b -> t m a) -> t m a
bracket_ bef aft bet = fromStreamD $
    D.bracketUnsafe bef aft (toStreamD . bet)

-- | Run the alloc action @m b@ with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Use the
-- output @b@ as input to @b -> t m a@ to generate an output stream.
--
-- @b@ is usually a resource under the state of monad @m@, e.g. a file
-- handle, that requires a cleanup after use. The cleanup action @b -> m c@,
-- runs whenever the stream ends normally, due to a sync or async exception or
-- if it gets garbage collected after a partial lazy evaluation.
--
-- 'bracket' only guarantees that the cleanup action runs, and it runs with
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
-- @since 0.7.0
--
{-# INLINE bracket #-}
bracket :: (IsStream t, MonadAsync m, MonadCatch m)
    => m b -> (b -> m c) -> (b -> t m a) -> t m a
bracket bef aft = bracket' bef aft aft aft

-- For a use case of this see the "streamly-process" package. It needs to kill
-- the process in case of exception or garbage collection, but waits for the
-- process to terminate in normal cases.
--
-- | Like 'bracket' but can use separate cleanup actions depending on the mode
-- of termination.  @bracket' before onStop onGC onException action@ runs
-- @action@ using the result of @before@. If the stream stops, @onStop@ action
-- is executed, if the stream is abandoned @onGC@ is executed, if the stream
-- encounters an exception @onException@ is executed.
--
-- /Pre-release/
{-# INLINE bracket' #-}
bracket' :: (IsStream t, MonadAsync m, MonadCatch m)
    => m b -> (b -> m c) -> (b -> m d) -> (b -> m e) -> (b -> t m a) -> t m a
bracket' bef aft gc exc bet = fromStreamD $
    D.bracket3D bef aft exc gc (toStreamD . bet)

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
ghandle :: (IsStream t, MonadCatch m, Exception e)
    => (e -> t m a -> t m a) -> t m a -> t m a
ghandle handler =
      fromStreamD
    . D.ghandle (\e xs -> return $ toStreamD $ handler e (fromStreamD xs))
    . toStreamD

-- | When evaluating a stream if an exception occurs, stream evaluation aborts
-- and the specified exception handler is run with the exception as argument.
--
-- /Inhibits stream fusion/
--
-- @since 0.7.0
{-# INLINE handle #-}
handle :: (IsStream t, MonadCatch m, Exception e)
    => (e -> t m a) -> t m a -> t m a
handle handler xs =
    fromStreamD $ D.handle (return . toStreamD . handler) $ toStreamD xs


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
retry :: (IsStream t, MonadCatch m, Exception e, Ord e)
    => Map e Int
       -- ^ map from exception to retry count
    -> (e -> t m a)
       -- ^ default handler for those exceptions that are not in the map
    -> t m a
    -> t m a
retry emap handler inp =
    fromStreamD $ D.retryD emap (toStreamD . handler) $ toStreamD inp
