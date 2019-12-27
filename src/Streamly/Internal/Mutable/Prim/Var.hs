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
-- A mutable variable in a mutable monad (IO/ST) holding a 'Prim' value. This
-- allows fast modification of a value.

module Streamly.Internal.Mutable.Prim.Var
    (
      Var
    , MonadMut
    , Prim

    -- * Construction

    , newVar

    -- * modification
    , writeVar

    -- * modification
    , readVar
    )
where

import Control.Monad.Primitive (PrimMonad(..), primitive_)
import Data.Primitive.Types (Prim, sizeOf#, readByteArray#, writeByteArray#)
import GHC.Exts (MutableByteArray#, newByteArray#)

-- XXX use unlifted newtype?
--
-- | A 'Var' holds a single 'Prim' value.
data Var m a = Var (MutableByteArray# (PrimState m))

type MonadMut = PrimMonad

-- | Create a new mutable variable.
{-# INLINE newVar #-}
newVar :: forall m a. (MonadMut m, Prim a) => m (Var m a)
newVar = primitive (\s# ->
      case newByteArray# (sizeOf# (undefined :: a)) s# of
        (# s'#, arr# #) -> (# s'#, Var arr# #)
    )

-- | Write a value to a mutable variable.
{-# INLINE writeVar #-}
writeVar :: (MonadMut m, Prim a) => Var m a -> a -> m ()
writeVar (Var arr#) x = primitive_ (writeByteArray# arr# 0# x)

{-# INLINE readVar #-}
readVar :: (MonadMut m, Prim a) => Var m a -> m a
readVar (Var arr#) = primitive (readByteArray# arr# 0#)
