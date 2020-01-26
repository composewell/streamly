{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE UnboxedTuples       #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Mutable.Prim.Var
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- A mutable variable in a mutation capable monad (IO/ST) holding a 'Prim'
-- value. This allows fast modification because of unboxed storage.
--
-- = Multithread Consistency Notes
--
-- In general, any value that straddles a machine word cannot be guaranteed to
-- be consistently read from another thread without a lock.  GHC heap objects
-- are always machine word aligned, therefore, a 'Var' is also word aligned. On
-- a 64-bit platform, writing a 64-bit aligned type from one thread and reading
-- it from another thread should give consistent old or new value. The same
-- holds true for 32-bit values on a 32-bit platform.

module Streamly.Internal.Mutable.Prim.Var
    (
      Var
    , MonadMut
    , Prim

    -- * Construction
    , newVar

    -- * Write
    , writeVar
    , modifyVar'

    -- * Read
    , readVar
    )
where

import Control.Monad.Primitive (PrimMonad(..), primitive_)
import Data.Primitive.Types (Prim, sizeOf#, readByteArray#, writeByteArray#)
import GHC.Exts (MutableByteArray#, newByteArray#)

-- | A 'Var' holds a single 'Prim' value.
data Var m a = Var (MutableByteArray# (PrimState m))

-- The name PrimMonad does not give a clue what it means, an explicit "Mut"
-- suffix provides a better hint. MonadMut is just a generalization of MonadIO.
--
-- | A monad that allows mutable operations using a state token.
type MonadMut = PrimMonad

-- | Create a new mutable variable.
{-# INLINE newVar #-}
newVar :: forall m a. (MonadMut m, Prim a) => a -> m (Var m a)
newVar x = primitive (\s# ->
      case newByteArray# (sizeOf# (undefined :: a)) s# of
        (# s1#, arr# #) ->
            case writeByteArray# arr# 0# x s1# of
                s2# -> (# s2#, Var arr# #)
    )

-- | Write a value to a mutable variable.
{-# INLINE writeVar #-}
writeVar :: (MonadMut m, Prim a) => Var m a -> a -> m ()
writeVar (Var arr#) x = primitive_ (writeByteArray# arr# 0# x)

-- | Read a value from a variable.
{-# INLINE readVar #-}
readVar :: (MonadMut m, Prim a) => Var m a -> m a
readVar (Var arr#) = primitive (readByteArray# arr# 0#)

-- | Modify the value of a mutable variable using a function with strict
-- application.
{-# INLINE modifyVar' #-}
modifyVar' :: (MonadMut m, Prim a) => Var m a -> (a -> a) -> m ()
modifyVar' (Var arr#) g = primitive_ $ \s# ->
  case readByteArray# arr# 0# s# of
    (# s'#, a #) -> let a' = g a in a' `seq` writeByteArray# arr# 0# a' s'#
