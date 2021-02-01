-- |
-- Module      : Streamly.Test.Data.Array.Foreign
-- Copyright   : (c) 2019 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Array.Foreign (main) where

#define TEST_ARRAY
#include "Streamly/Test/Common/Array.hs"
