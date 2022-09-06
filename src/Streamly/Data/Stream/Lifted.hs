module Streamly.Data.Stream.Lifted
(
     after
    , bracket
    , finally
    )
where

import Control.Monad.Catch (MonadCatch)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream.Type (Stream, fromStreamD, toStreamD)

import qualified Streamly.Internal.Data.Stream.StreamD.Lifted as LE

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Stream.Lifted as LE

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

-- | Run the action @m b@ whenever the stream @Stream m a@ stops normally, or
-- if it is garbage collected after a partial lazy evaluation.
--
-- The semantics of the action @m b@ are similar to the semantics of cleanup
-- action in 'bracket'.
--
-- /See also 'after_'/
--
{-# INLINE after #-}
after :: MonadAsync m
    => m b -> Stream m a -> Stream m a
after action xs = fromStreamD $ LE.after action $ toStreamD xs

-- | Run the action @m b@ whenever the stream @Stream m a@ stops normally,
-- aborts due to an exception or if it is garbage collected after a partial
-- lazy evaluation.
--
-- The semantics of running the action @m b@ are similar to the cleanup action
-- semantics described in 'bracket'.
--
-- >>> finally release = LE.bracket (return ()) (const release)
--
-- /See also 'finally_'/
--
-- /Inhibits stream fusion/
--
{-# INLINE finally #-}
finally :: (MonadAsync m, MonadCatch m) => m b -> Stream m a -> Stream m a
finally action xs = fromStreamD $ LE.finally action $ toStreamD xs

-- | Run the alloc action @m b@ with async exceptions disabled but keeping
-- blocking operations interruptible (see 'Control.Exception.mask').  Use the
-- output @b@ as input to @b -> Stream m a@ to generate an output stream.
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
{-# INLINE bracket #-}
bracket :: (MonadAsync m, MonadCatch m)
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracket bef aft = bracket' bef aft aft aft

-- For a use case of this see the "streamly-process" package. It needs to kill
-- the process in case of exception or garbage collection, but waits for the
-- process to terminate in normal cases.

-- | Like 'bracket' but can use separate cleanup actions depending on the mode
-- of termination.  @bracket' before onStop onGC onException action@ runs
-- @action@ using the result of @before@. If the stream stops, @onStop@ action
-- is executed, if the stream is abandoned @onGC@ is executed, if the stream
-- encounters an exception @onException@ is executed.
--
-- /Pre-release/
{-# INLINE bracket' #-}
bracket' :: (MonadAsync m, MonadCatch m)
    => m b
    -> (b -> m c)
    -> (b -> m d)
    -> (b -> m e)
    -> (b -> Stream m a)
    -> Stream m a
bracket' bef aft gc exc bet = fromStreamD $
    LE.bracket' bef aft exc gc (toStreamD . bet)
