{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
-- |
-- Module      : Streamly.Internal.Data.Fold
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Streamly.Data.Fold" for an overview and
-- "Streamly.Internal.Data.Fold.Type" for design notes.

module Streamly.Internal.Data.Fold
    (
    -- * Imports
    -- $setup

      module Streamly.Internal.Data.Fold.Type
    , module Streamly.Internal.Data.Fold.Tee
    , module Streamly.Internal.Data.Fold.Combinators
    , module Streamly.Internal.Data.Fold.Container
    , module Streamly.Internal.Data.Fold.Window
    , module Streamly.Internal.Data.Fold.Exception
    )
where

import Streamly.Internal.Data.Fold.Combinators
import Streamly.Internal.Data.Fold.Container
import Streamly.Internal.Data.Fold.Exception
import Streamly.Internal.Data.Fold.Tee
import Streamly.Internal.Data.Fold.Type
import Streamly.Internal.Data.Fold.Window

#include "DocTestDataFold.hs"
