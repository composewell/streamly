-- |
-- Module      : Streamly.Internal.Control.Exception
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Additional "Control.Exception" utilities.

module Streamly.Internal.Control.Exception
    (
    -- * Verify
      verify
    , verifyM

    -- * Resource Management
    -- | Exception safe, thread safe resource managment operations, similar to
    -- but more powerful than the bracket and finally operations available in
    -- the base package.
    --
    -- These operations support allocation and free only in the IO monad,
    -- therefore, they have the IO suffix.
    --
    , AllocateIO(..) -- XXX rename to BracketIO or AcquirerIO
    , Priority(..)
    , withAllocateIO
    , acquireWith
    , acquire
    , registerWith
    , register
    , hook
    )
where

-- import Control.Concurrent (myThreadId)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (mask_)
import Control.Monad.Catch (MonadMask)
import Data.IORef (newIORef, readIORef, atomicModifyIORef')

import qualified Control.Monad.Catch as MC
import qualified Data.IntMap.Strict as Map

-------------------------------------------------------------------------------
-- Asserts
-------------------------------------------------------------------------------

-- | Like 'assert' but is not removed by the compiler, it is always present in
-- production code.
--
-- /Pre-release/
--
{-# INLINE verify #-}
verify :: Bool -> a -> a
verify predicate val =
    if predicate
    -- XXX it would be nice if we can print the predicate expr.
    then error "verify failed"
    else val

-- Like 'verify' but returns @()@ in an 'Applicative' context so that it can be
-- used as an independent statement in a @do@ block.
--
-- /Pre-release/
--
{-# INLINE verifyM #-}
verifyM :: Applicative f => Bool -> f ()
verifyM predicate = verify predicate (pure ())

-------------------------------------------------------------------------------
-- Resource management
-------------------------------------------------------------------------------

-- XXX In a manual release mechanism of resources we always have the risk of
-- using the resource by some persisting thread even after it has been freed.
-- Ideally, we should use the GC to clean up resources because that way we do
-- not need to worry about references, we can pass around resources to other
-- threads and we get an automatic reference counting. Is it possible to use
-- compact regions to confine resource to smaller areas so that we can perform
-- a limited GC to free them? We can then just put gc sync barriers at points
-- where we want to ensure that resources are freed.

-- | Resources with Priority1 are freed before Priority2. This is especially
-- introduced to take care of the case where we need to free channels, so that
-- all the workers of the channel are cleanup before we free the resources
-- allocated by the workers of the channel. Otherwise we might free the
-- resources and workers may be trying to use them and start misbehaving.
--
data Priority = Priority1 | Priority2 deriving Show

-- To keep the type signatures simple and to avoid inference problems we should
-- use this newtype. We cannot pass around a foralled type without wrapping
-- it in a newtype.

-- | @AllocateIO@ is used to acquire a resource safely such that it is
-- automatically released if not released manually.
--
-- See 'withAllocateIO'.
--
newtype AllocateIO = AllocateIO
    (forall b c. Priority -> IO b -> (b -> IO c) -> IO (b, IO ()))

-- | @withAllocateIO action@ runs the given @action@, providing it with a
-- special function called @allocator@ as argument. An @allocator alloc
-- free@ call can be used within @action@ any number of times to allocate
-- resources that are automatically freed when 'withAllocateIO' ends or if an
-- exception occurs at any time. @alloc@ is a function used to allocate a
-- resource and @free@ is to free the allocated resource. @allocator@
-- returns @(resource, release)@ -- the allocated @resource@ and a @release@
-- action to release it.
--
-- @allocator@ allocates a resource in an exception safe manner and
-- sets up its automatic release on exception or when @withAllocateIO@ ends.
-- The @release@ function returned by @allocator@ can be used to free the
-- resource manually at any time. @release@ is guaranteed to free the resource
-- only once even if it is called concurrently or multiple times.
--
-- Here is an example to allocate resources that are guaranteed to be released,
-- and can be released manually as well:
--
-- >>> :{
-- close x h = do
--  putStrLn $ "closing: " ++ x
--  hClose h
--
-- action (AllocateIO alloc) =
--      Stream.fromList ["file1", "file2"]
--    & Stream.mapM
--        (\x -> do
--            (h, release) <- alloc (openFile x ReadMode) (close x)
--            -- use h here
--            threadDelay 1000000
--            when (x == "file1") $ do
--                putStrLn $ "Manually releasing: " ++ x
--                release
--            return x
--        )
--    & Stream.trace print
--    & Stream.fold Fold.drain
--
-- run = Exception.withAllocateIO action
-- :}
--
-- In the above code, you should see the \"closing:\" message for both the
-- files, and only once for each file. Even if you interrupt the program with
-- CTRL-C you should still see the \"closing:\" message for the files opened
-- before the interrupt. Make sure you create "file1" and "file2" before
-- running it.
--
-- Cleanup is guaranteed to happen as soon as the scope of 'withAllocateIO'
-- finishes or if an exception occurs.
--
-- Here is an example for just registering hooks to be called eventually:
--
-- >>> :{
-- action f =
--      Stream.fromList ["file1", "file2"]
--    & Stream.mapM
--        (\x -> do
--            register f $ putStrLn $ "saw: " ++ x
--            threadDelay 1000000
--            return x
--        )
--    & Stream.trace print
--    & Stream.fold Fold.drain
--
-- run = Exception.withAllocateIO action
-- :}
--
-- In the above code, even if you interrupt the program with CTRL-C you should
-- still see the "saw:" message for the elements seen before the interrupt.
--
-- The registered hooks are guaranteed to be invoked as soon as the scope of
-- 'withAllocateIO' finishes or if an exception occurs.
--
-- This function provides functionality similar to the @bracket@ function
-- available in the base library. However, it is more powerful as any number of
-- resources can be allocated at any time within the scope and can be released
-- at any time.
--
-- Exception safe, thread safe.
{-# INLINE withAllocateIO #-}
withAllocateIO :: (MonadIO m, MonadMask m) => (AllocateIO -> m a) -> m a
withAllocateIO action = do
    -- Assuming 64-bit int counter will never overflow
    ref <- liftIO $ newIORef (0 :: Int, Map.empty, Map.empty)
    action (AllocateIO (allocator ref)) `MC.finally` aft ref

    where

    -- XXX can we ensure via GC that the resources that we are freeing are all
    -- dead, there are no other references to them?
    --
    -- XXX Take a lock to avoid parallel release. By using an MVar for each
    -- "release" instance we can make release thread-safe such that it can be
    -- called from multiple threads concurrently. We can use acquireSafeRelease
    -- to optionally use that feature. Using that will make release
    -- interruptible. Also using a shared resource in multiple threads may have
    -- more issues like one thread releasing it means other threads should not
    -- use it any more.
    --
    -- XXX We can use atomicModifyIORef to run each release action only once,
    -- in one of the threads instead of using an MVar.

    aft ref =
        -- XXX free them atomically, even if another release executes in
        -- parallel.
        liftIO $ mask_ $ do
            (_, mp1, _) <- readIORef ref
            -- liftIO $ putStrLn "cleaning up priority 1"
            -- Note that the channel cleanup function is interruptible because
            -- it has blocking points.
            sequence_ mp1
            -- Now nobody would be changing mp2, we can read it safely
            (_, _, mp2) <- readIORef ref
            -- liftIO $ putStrLn "cleaning up priority 2"
            sequence_ mp2

    allocator ref pri alloc free = do
        let insertResource r (i, mp1, mp2) =
                case pri of
                    Priority1 ->
                        ((i + 1, Map.insert i (void $ free r) mp1, mp2), i)
                    Priority2 ->
                        ((i + 1, mp1, Map.insert i (void $ free r) mp2), i)

        (r, index) <-
            liftIO $ mask_ $ do
                -- tid <- myThreadId
                r <- alloc
                idx <- atomicModifyIORef' ref (insertResource r)
                -- liftIO $ putStrLn $ "insert: " ++ show pri
                --      ++ " " ++ show idx ++ " " ++ show tid
                return (r, idx)

        let deleteResource (i, mp1, mp2) =
                case pri of
                    Priority1 ->
                        let res = Map.lookup index mp1
                         in ((i, Map.delete index mp1, mp2), res)
                    Priority2 ->
                        let res = Map.lookup index mp2
                         in ((i, mp1, Map.delete index mp2), res)

            release =
                -- IMPORTANT: do not use interruptible operations in this
                -- critical section. Even putStrLn can make tests fail.
                liftIO $ mask_ $ do
                    -- tid <- myThreadId
                    -- liftIO $ putStrLn $ "releasing index: " ++ show index
                    --      ++ " " ++ show tid
                    f <- atomicModifyIORef' ref deleteResource
                    -- restoring exceptions makes it non-atomic, tests fail.
                    -- Can use allowInterrupt in "free" if desired.
                    sequence_ f
        return (r, release)

{-# INLINE acquireWith #-}
acquireWith :: AllocateIO -> Priority -> IO b -> (b -> IO c) -> IO (b, IO ())
acquireWith (AllocateIO f) = f

-- | @acquire bracket alloc free@ is used in bracket-style safe resource
-- allocation functions, where @alloc@ is a function used to allocate a
-- resource and @free@ is used to free it. @acquire@ returns a tuple
-- @(resource, release)@ where @resource@ is the allocated resource and
-- @release@ is an action that can be called later to release the resource.
-- Both alloc and free are invoked with async signals masked. You can use
-- allowInterrupt for allowing interrupts if required.
--
-- The release action can be called multiple times but it will release the
-- resource only once. However, it cannot be called concurrently from multiple
-- threads. If @release@ is never called it will be called at the end of the
-- bracket scope.
--
acquire :: AllocateIO -> IO b -> (b -> IO c) -> IO (b, IO ())
acquire alloc = acquireWith alloc Priority2

{-# INLINE registerWith #-}
registerWith :: AllocateIO -> Priority -> IO () -> IO ()
registerWith (AllocateIO f) pri g = void $ f pri (return ()) (\() -> g)

-- | Register a hook to be executed at the end of a bracket.
register :: AllocateIO -> IO () -> IO ()
register alloc = registerWith alloc Priority2

-- | Like 'register' but returns a hook release function as well. When the
-- returned hook release function is called, the hook is invoked and removed.
-- If the returned function is never called then it will be automatically
-- invoked at the end of the bracket. The hook is invoked once and only once.
--
hook :: AllocateIO -> IO () -> IO (IO())
hook (AllocateIO f) g = fmap snd $ f Priority2 (return ()) (\() -> g)
