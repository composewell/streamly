{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Streamly.Data.Internal.Unicode.Char
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Unicode.Char
    (
    -- * Predicates
      isAsciiAlpha

    -- * Unicode aware operations
    {-
      toCaseFold
    , toLower
    , toUpper
    , toTitle
    -}
    )
where

import Data.Char (isAsciiUpper, isAsciiLower)

-- import Streamly (IsStream)

-------------------------------------------------------------------------------
-- Unicode aware operations on strings
-------------------------------------------------------------------------------

-- | Select alphabetic characters in the ascii character set.
--
-- /Internal/
--
{-# INLINE isAsciiAlpha #-}
isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAsciiUpper c || isAsciiLower c

-------------------------------------------------------------------------------
-- Unicode aware operations on strings
-------------------------------------------------------------------------------

{-
-- |
-- /undefined/
toCaseFold :: IsStream t => Char -> t m Char
toCaseFold = undefined

-- |
-- /undefined/
toLower :: IsStream t => Char -> t m Char
toLower = undefined

-- |
-- /undefined/
toUpper :: IsStream t => Char -> t m Char
toUpper = undefined

-- |
-- /undefined/
toTitle :: IsStream t => Char -> t m Char
toTitle = undefined
-}
