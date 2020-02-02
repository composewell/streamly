{-# LANGUAGE CPP         #-}

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
import qualified Data.Atomics as A

-- XXX Does it make sense to have replacements for atomicModifyIORef etc. on a
-- single threaded system.
--
-- Slightly faster version of CAS. Gained some improvement by avoiding the use
-- of "evaluate" because we know we do not have exceptions in fn.
{-# INLINE atomicModifyIORefCAS #-}
atomicModifyIORefCAS :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORefCAS = atomicModifyIORef
{-
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
-}

{-# INLINE atomicModifyIORefCAS_ #-}
atomicModifyIORefCAS_ :: IORef t -> (t -> t) -> IO ()
-- atomicModifyIORefCAS_ = A.atomicModifyIORefCAS_
atomicModifyIORefCAS_ ref fn = do
    _ <- atomicModifyIORef ref (\x -> (fn x, ()))
    return ()

{-# INLINE writeBarrier #-}
writeBarrier :: IO ()
writeBarrier = A.writeBarrier

{-# INLINE storeLoadBarrier #-}
storeLoadBarrier :: IO ()
storeLoadBarrier = A.storeLoadBarrier
