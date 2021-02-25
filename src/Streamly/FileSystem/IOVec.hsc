{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "MachDeps.h"

-- |
-- Module      : Streamly.FileSystem.IOVec
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Low level IO routines interfacing the operating system.
--

module Streamly.FileSystem.IOVec
    ( IOVec(..)
    , c_writev
    , c_safe_writev
    )
where

import Data.Word (Word8)
#if (WORD_SIZE_IN_BITS == 32)
import Data.Word (Word32)
#else
import Data.Word (Word64)
#endif
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)
import System.Posix.Types (CSsize(..))
#if !defined(mingw32_HOST_OS)
import Foreign.Storable (Storable(..))
#endif

-------------------------------------------------------------------------------
-- IOVec
-------------------------------------------------------------------------------

data IOVec = IOVec
  { iovBase :: {-# UNPACK #-} !(Ptr Word8)
#if (WORD_SIZE_IN_BITS == 32)
  , iovLen  :: {-# UNPACK #-} !Word32
#else
  , iovLen  :: {-# UNPACK #-} !Word64
#endif
  } deriving (Eq, Show)

#if !defined(mingw32_HOST_OS)

#include <sys/uio.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

instance Storable IOVec where
  sizeOf _ = #{size struct iovec}
  alignment _ = #{alignment struct iovec}
  peek ptr = do
      base <- #{peek struct iovec, iov_base} ptr
      len  :: #{type size_t} <- #{peek struct iovec, iov_len}  ptr
      return $ IOVec base len
  poke ptr vec = do
      let base = iovBase vec
          len  :: #{type size_t} = iovLen vec
      #{poke struct iovec, iov_base} ptr base
      #{poke struct iovec, iov_len}  ptr len
#endif

-- capi calling convention does not work without -fobject-code option with GHCi
-- so using this in DEVBUILD only for now.
--
#if !defined(mingw32_HOST_OS) && defined DEVBUILD
foreign import capi unsafe "sys/uio.h writev"
   c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize

foreign import capi safe "sys/uio.h writev"
   c_safe_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize

#else
c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize
c_writev = error "writev not implemented"

c_safe_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize
c_safe_writev = error "writev not implemented"
#endif
