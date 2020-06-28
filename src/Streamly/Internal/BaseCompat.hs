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
    , errorWithoutStackTrace
    )
where

import Data.Coerce (Coercible, coerce)

{-# INLINE (#.) #-}
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce

#if !(MIN_VERSION_base(4,9,0))
{-# NOINLINE errorWithoutStackTrace #-}
errorWithoutStackTrace :: [Char] -> a
errorWithoutStackTrace s = error s
#endif
