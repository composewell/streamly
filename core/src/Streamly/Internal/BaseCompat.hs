-- |
-- Module      : Streamly.Internal.BaseCompat
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Compatibility functions for "base" package.
--
module Streamly.Internal.BaseCompat
    (
      (#.)
    , unsafeWithForeignPtr
    )
where

import Data.Coerce (Coercible, coerce)
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Ptr (Ptr(..))

#if MIN_VERSION_base(4,15,0)
import qualified GHC.ForeignPtr as GHCForeignPtr
#else
import Foreign.ForeignPtr (withForeignPtr)
#endif


{-# INLINE (#.) #-}
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce

unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
#if MIN_VERSION_base(4,15,0)
unsafeWithForeignPtr = GHCForeignPtr.unsafeWithForeignPtr
#else
unsafeWithForeignPtr = withForeignPtr
#endif
