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
    , AcquireIO(..)
    , Priority(..)
    , allocator
    , releaser
    , withAcquireIO
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
import Data.IntMap.Strict (IntMap)
import Data.IORef (IORef, newIORef, atomicModifyIORef')

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

-- | @AcquireIO@ is used to acquire a resource safely such that it is
-- automatically released if not released manually.
--
-- See 'withAcquireIO'.
--
newtype AcquireIO = AcquireIO
    (forall b c. Priority -> IO b -> (b -> IO c) -> IO (b, IO ()))

allocator :: MonadIO m =>
       IORef (Int, IntMap (IO ()), IntMap (IO ()))
    -> Priority
    -> IO a
    -> (a -> IO b)
    -> m (a, m ())
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

-- XXX can we ensure via GC that the resources that we are freeing are all
-- dead, there are no other references to them?

-- | We ensure that all async workers for concurrent streams are stopped
-- before we release the resources so that nobody could be using the
-- resource after they are freed.
--
-- The only other possibility, could be user issued forkIO not being
-- tracked by us, however, that would be a programming error and any such
-- threads could misbehave if we freed the resources from under them.
--
-- We use GC based hooks in Stream.bracketIO' so there could be async threads
-- spawned by GC, releasing resources concurrently with us. For that reason we
-- need to make sure that the "release" in the bracket end action is executed
-- only once in that case.
--
releaser :: MonadIO m => IORef (a, IntMap (IO b), IntMap (IO b)) -> m ()
releaser ref =
    liftIO $ mask_ $ do
        -- Delete the map from the ref first so that anyone else (GC)
        -- releasing concurrently cannot find the map.
        -- liftIO $ putStrLn "cleaning up priority 1"
        mp1 <- atomicModifyIORef' ref
            (\(i, mp1,mp2) -> ((i, Map.empty, mp2), mp1))
        -- Note that the channel cleanup function is interruptible because
        -- it has blocking points.
        sequence_ mp1
        -- Now nobody would be changing mp2, we can read it safely
        -- liftIO $ putStrLn "cleaning up priority 2"
        mp2 <- atomicModifyIORef' ref
            (\(i, mp,mp2) -> ((i, mp, Map.empty), mp2))
        sequence_ mp2
        -- XXX We can now assert that the IORef has both maps empty.

-- | @withAcquireIO action@ runs the given @action@, providing it with a
-- special function called @allocator@ as argument. An @allocator alloc
-- free@ call can be used within @action@ any number of times to allocate
-- resources that are automatically freed when 'withAcquireIO' ends or if an
-- exception occurs at any time. @alloc@ is a function used to allocate a
-- resource and @free@ is to free the allocated resource. @allocator@
-- returns @(resource, release)@ -- the allocated @resource@ and a @release@
-- action to release it.
--
-- @allocator@ allocates a resource in an exception safe manner and
-- sets up its automatic release on exception or when @withAcquireIO@ ends.
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
-- action (AcquireIO alloc) =
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
-- run = Exception.withAcquireIO action
-- :}
--
-- In the above code, you should see the \"closing:\" message for both the
-- files, and only once for each file. Even if you interrupt the program with
-- CTRL-C you should still see the \"closing:\" message for the files opened
-- before the interrupt. Make sure you create "file1" and "file2" before
-- running it.
--
-- Cleanup is guaranteed to happen as soon as the scope of 'withAcquireIO'
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
-- run = Exception.withAcquireIO action
-- :}
--
-- In the above code, even if you interrupt the program with CTRL-C you should
-- still see the "saw:" message for the elements seen before the interrupt.
--
-- The registered hooks are guaranteed to be invoked as soon as the scope of
-- 'withAcquireIO' finishes or if an exception occurs.
--
-- This function provides functionality similar to the @bracket@ function
-- available in the base library. However, it is more powerful as any number of
-- resources can be allocated at any time within the scope and can be released
-- at any time.
--
-- Exception safe, thread safe.
{-# INLINE withAcquireIO #-}
withAcquireIO :: (MonadIO m, MonadMask m) => (AcquireIO -> m a) -> m a
withAcquireIO action = do
    -- Assuming 64-bit int counter will never overflow
    ref <- liftIO $ newIORef (0 :: Int, Map.empty, Map.empty)
    action (AcquireIO (allocator ref)) `MC.finally` releaser ref

-- | Like 'acquire' but allows specifying a priority for releasing the
-- resource. Priority1 resources are released before Priority2. This allows us
-- to specify a dependency between resource release.
{-# INLINE acquireWith #-}
acquireWith :: Priority -> AcquireIO -> IO b -> (b -> IO c) -> IO (b, IO ())
acquireWith pri (AcquireIO f) = f pri

-- | @acquire bracket alloc free@ is used in bracket-style safe resource
-- allocation functions, where @alloc@ is a function used to allocate a
-- resource and @free@ is used to free it. @acquire@ returns a tuple
-- @(resource, release)@ where @resource@ is the allocated resource and
-- @release@ is an action that can be called later to release the resource.
-- Both alloc and free are invoked with async signals masked. You can use
-- allowInterrupt for allowing interrupts if required.
--
-- The release action can be called multiple times or even concurrently from
-- multiple threads,  but it will release the resource only once. If @release@
-- is never called it will be called at the end of the bracket scope.
--
acquire :: AcquireIO -> IO b -> (b -> IO c) -> IO (b, IO ())
acquire = acquireWith Priority2

{-# INLINE registerWith #-}
registerWith :: Priority -> AcquireIO -> IO () -> IO ()
registerWith pri (AcquireIO f) g = void $ f pri (return ()) (\() -> g)

-- | Register a hook to be executed at the end of a bracket.
register :: AcquireIO -> IO () -> IO ()
register = registerWith Priority2

-- | Like 'register' but returns a hook release function as well. When the
-- returned hook release function is called, the hook is invoked and removed.
-- If the returned function is never called then it will be automatically
-- invoked at the end of the bracket. The hook is invoked once and only once.
--
hook :: AcquireIO -> IO () -> IO (IO())
hook (AcquireIO f) g = fmap snd $ f Priority2 (return ()) (\() -> g)
