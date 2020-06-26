-- |
-- Module      : Streamly.Internal.Data.Atomics
-- Copyright   : (c) 2018-2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Atomics
    (
      atomicModifyIORefCAS
    , atomicModifyIORefCAS_
    , writeBarrier
    , storeLoadBarrier
    )
where

import Data.IORef (IORef, atomicModifyIORef)
#ifdef ghcjs_HOST_OS
import Data.IORef (modifyIORef)
#else
import qualified Data.Atomics as A
#endif

#ifndef ghcjs_HOST_OS

-- XXX Does it make sense to have replacements for atomicModifyIORef etc. on a
-- single threaded system.
--
-- Slightly faster version of CAS. Gained some improvement by avoiding the use
-- of "evaluate" because we know we do not have exceptions in fn.
{-# INLINE atomicModifyIORefCAS #-}
atomicModifyIORefCAS :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORefCAS ref fn = do
    tkt <- A.readForCAS ref
    loop tkt retries

    where

    retries = 25 :: Int
    loop _   0     = atomicModifyIORef ref fn
    loop old tries = do
        let (new, result) = fn $ A.peekTicket old
        (success, tkt) <- A.casIORef ref old new
        if success
        then return result
        else loop tkt (tries - 1)

{-# INLINE atomicModifyIORefCAS_ #-}
atomicModifyIORefCAS_ :: IORef t -> (t -> t) -> IO ()
atomicModifyIORefCAS_ = A.atomicModifyIORefCAS_

{-# INLINE writeBarrier #-}
writeBarrier :: IO ()
writeBarrier = A.writeBarrier

{-# INLINE storeLoadBarrier #-}
storeLoadBarrier :: IO ()
storeLoadBarrier = A.storeLoadBarrier

#else

{-# INLINE atomicModifyIORefCAS #-}
atomicModifyIORefCAS :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORefCAS = atomicModifyIORef

{-# INLINE atomicModifyIORefCAS_ #-}
atomicModifyIORefCAS_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORefCAS_ = modifyIORef

{-# INLINE writeBarrier #-}
writeBarrier :: IO ()
writeBarrier = return ()

{-# INLINE storeLoadBarrier #-}
storeLoadBarrier :: IO ()
storeLoadBarrier = return ()

#endif
