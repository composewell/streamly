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
    , fromLeft
    , fromRight
    )
where

import Data.Coerce (Coercible, coerce)
#if MIN_VERSION_base(4,10,0)
import Data.Either (fromRight, fromLeft)
#endif

{-# INLINE (#.) #-}
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce

#if !(MIN_VERSION_base(4,9,0))
{-# NOINLINE errorWithoutStackTrace #-}
errorWithoutStackTrace :: String -> a
errorWithoutStackTrace = error
#endif

#if !(MIN_VERSION_base(4,10,0))
fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b
#endif
