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
    , AllocateIO(..)
    , RegisterIO(..)
    , allocToRegIO
    , withRegisterIO
    , withAllocateIO
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Exception (mask_)
import Control.Monad.Catch (MonadMask)
import Data.Foldable (sequenceA_)
import Data.IORef (newIORef, readIORef, atomicModifyIORef)

import qualified Control.Monad.Catch as MC
import qualified Data.Map.Strict as Map

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
-- them it in a newtype.

-- | @AllocateIO f@ is a newtype wrapper for an IO monad allocator function @f@.
--
-- The allocator function @f alloc free@ is used in bracket-style safe resource
-- allocation functions, where @alloc@ is a function used to allocate a
-- resource and @free@ is used to free it. The allocator returns a tuple
-- @(resource, release)@ where @resource@ is the allocated resource and
-- @release@ is an action that can be called later to release the resource.
--
newtype AllocateIO = AllocateIO
    (forall b c. IO b -> (b -> IO c) -> IO (b, IO ()))

-- | @RegisterIO f@ is a newtype wrapper for a hook registration function @f@.
--
-- @f hook@ is used to register hooks to be executed at the end of
-- finally style functions.
--
newtype RegisterIO = RegisterIO (forall c. IO c -> IO ())

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
-- This function provides functionality similar to the @bracket@ function
-- available in the base library. However, it is more powerful as any number of
-- resources can be allocated at any time within the scope and can be released
-- at any time.
--
-- Exception safe, thread safe.
{-# INLINE withAllocateIO #-}
withAllocateIO :: (MonadIO m, MonadMask m) => (AllocateIO -> m a) -> m a
withAllocateIO action = do
    ref <- liftIO $ newIORef (0 :: Int, Map.empty)
    action (AllocateIO (bracket ref)) `MC.finally` aft ref

    where

    -- This is called from a the same thread as the main action, therefore, we
    -- do not need to worry about concurrent execution.
    aft ref = liftIO $ do
        xs <- readIORef ref
        sequence_ (snd xs)

    bracket ref alloc free = do
        (r, index) <- liftIO $ mask_ $ do
            r <- alloc
            idx <- atomicModifyIORef ref (\(i, mp) ->
                ((i + 1, Map.insert i (void $ free r) mp), i))
            return (r, idx)

        let modify (i, mp) =
                let res = Map.lookup index mp
                 in ((i, Map.delete index mp), res)
            free1 = do
                res <- atomicModifyIORef ref modify
                sequence_ res
        return (r, free1)

-- | Convert an @allocate@ function to a hook registration function.
--
allocToRegIO :: AllocateIO -> RegisterIO
allocToRegIO (AllocateIO f) = RegisterIO (void . g)

    where

    g x = f (return ()) (\() -> x)

-- | @withRegisterIO action@ runs the given @action@, providing it with a
-- special function called @register@ as argument. A @register hook@ call can
-- be used within @action@ any number of times to register hooks that would
-- run automatically when 'withRegisterIO' ends or if an exception occurs at
-- any time.
--
-- This function provides functionality similar to the @finally@ function
-- available in the @base@ package. However, it is more powerful as any number
-- of hooks can be registered at any time within the scope of @withRegisterIO@.
--
-- Exception safe, thread safe.
{-# INLINE withRegisterIO #-}
withRegisterIO :: forall m a. (MonadIO m, MonadMask m) =>
    (RegisterIO -> m a) -> m a
{-
withRegisterIO action = do
    let f bracket = do
            let reg hook = void $ bracket (return ()) (\() -> void hook)
            action reg
     in withAllocateIO f
-}
withRegisterIO action = do
    ref <- liftIO $ newIORef []
    action (RegisterIO (register ref)) `MC.finally` aft ref

    where

    aft ref = liftIO $ do
        xs <- readIORef ref
        sequenceA_ xs

    register ref f =
        atomicModifyIORef ref (\xs -> (void f : xs, ()))
