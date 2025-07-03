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
    , withAllocateIO
    , acquire
    , register
    , hook
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (mask_)
import Control.Monad.Catch (MonadMask)
import Data.IORef (newIORef, readIORef, atomicModifyIORef)

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

-- To keep the type signatures simple and to avoid inference problems we should
-- use this newtype. We cannot pass around a foralled type without wrapping
-- it in a newtype.

-- | @AllocateIO@ is used to acquire a resource safely such that it is
-- automatically released if not released manually.
--
-- See 'withAllocateIO'.
--
newtype AllocateIO = AllocateIO
    (forall b c. IO b -> (b -> IO c) -> IO (b, IO ()))

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
    ref <- liftIO $ newIORef (0 :: Int, Map.empty)
    action (AllocateIO (allocator ref)) `MC.finally` aft ref

    where

    -- The thread doing manual release
    -- do not need to worry about concurrent execution.
    aft ref = liftIO $ do
        xs <- readIORef ref
        sequence_ (snd xs)

    allocator ref alloc free = do
        (r, index) <- liftIO $ mask_ $ do
            r <- alloc
            idx <- atomicModifyIORef ref (\(i, mp) ->
                ((i + 1, Map.insert i (void $ free r) mp), i))
            return (r, idx)

        let modify (i, mp) =
                let res = Map.lookup index mp
                 in ((i, Map.delete index mp), res)

            release = do
                f <- atomicModifyIORef ref modify
                sequence_ f
        return (r, release)

-- | @acquire bracket alloc free@ is used in bracket-style safe resource
-- allocation functions, where @alloc@ is a function used to allocate a
-- resource and @free@ is used to free it. @acquire@ returns a tuple
-- @(resource, release)@ where @resource@ is the allocated resource and
-- @release@ is an action that can be called later to release the resource.
--
-- The release action can be called multiple times but it will release the
-- resource only once. If @release@ is never called it will be called at the
-- end of the bracket scope.
--
acquire :: AllocateIO -> IO b -> (b -> IO c) -> IO (b, IO ())
acquire (AllocateIO f) = f

-- | Register a hook to be executed at the end of a bracket.
register :: AllocateIO -> IO () -> IO ()
register (AllocateIO f) g = void $ f (return ()) (\() -> g)

-- | Like 'register' but returns a hook release function as well. When the
-- returned hook release function is called, the hook is invoked and removed.
-- If the returned function is never called then it will be automatically
-- invoked at the end of the bracket. The hook is invoked once and only once.
--
hook :: AllocateIO -> IO () -> IO (IO())
hook (AllocateIO f) g = fmap snd $ f (return ()) (\() -> g)
