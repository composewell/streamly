-- |
-- Module      : Streamly.Internal.Data.LinkedQueue
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A FIFO queue with the API of "Data.Concurrent.Queue.MichaelScott" from the
-- lockfree-queue package, for the JavaScript backend. lockfree-queue depends
-- on atomic-primops which does not build with the JavaScript backend.

module Streamly.Internal.Data.LinkedQueue
    ( LinkedQueue
    , newQ
    , nullQ
    , pushL
    , tryPopR
    )
where

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')

-- | Elements are popped from the first list and pushed to the second list.
newtype LinkedQueue a = LinkedQueue (IORef ([a], [a]))

newQ :: IO (LinkedQueue a)
newQ = LinkedQueue <$> newIORef ([], [])

nullQ :: LinkedQueue a -> IO Bool
nullQ (LinkedQueue ref) = do
    (front, back) <- readIORef ref
    return $ null front && null back

-- | Push an element at the end of the queue.
pushL :: LinkedQueue a -> a -> IO ()
pushL (LinkedQueue ref) x =
    atomicModifyIORef' ref $ \(front, back) -> ((front, x : back), ())

-- | Pop the element at the front of the queue.
tryPopR :: LinkedQueue a -> IO (Maybe a)
tryPopR (LinkedQueue ref) = atomicModifyIORef' ref pop

    where

    pop (x : front, back) = ((front, back), Just x)
    pop ([], back) =
        case reverse back of
            [] -> (([], []), Nothing)
            x : front -> ((front, []), Just x)
