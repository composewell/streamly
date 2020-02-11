{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      : Streamly.Internal.Fusion
-- Copyright   : (c) Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Fusion
    (
      Fuse (..)
    )
where

#if defined(__GHCJS__) || __GLASGOW_HASKELL__ < 806
import Data.Data (Data)
data Fuse = Fuse deriving (Data)
#else
import Fusion.Plugin.Types (Fuse(..))
#endif
