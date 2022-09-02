module Streamly.Data.Stream.Exceptions.Lifted
(
     after_
    , after
    , bracket_
    , bracket
    , bracket'
    , finally_
    , finally
    )
where

import Control.Monad.Catch (MonadCatch)
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Stream.Type (Stream, fromStreamD, toStreamD)

import qualified Streamly.Internal.Data.Stream.Exceptions.Lifted as LE

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Stream.Exceptions.Lifted as LE

------------------------------------------------------------------------------
-- Exceptions
------------------------------------------------------------------------------

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
after_ action xs = fromStreamD $ LE.after_ action $ toStreamD xs

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
finally_ :: MonadCatch m => m b -> Stream m a -> Stream m a
finally_ action xs = fromStreamD $ LE.finally_ action $ toStreamD xs

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
bracket_ :: MonadCatch m
    => m b -> (b -> m c) -> (b -> Stream m a) -> Stream m a
bracket_ bef aft bet = fromStreamD $
    LE.bracket_ bef aft (toStreamD . bet)

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
