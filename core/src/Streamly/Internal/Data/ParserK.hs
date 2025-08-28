{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.ParserK
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.ParserK
    (
      module Streamly.Internal.Data.ParserK.Type

    -- * Deprecated
    , adaptC
    , adaptCG
    )
where

import Streamly.Internal.Data.Parser (Parser)
import Streamly.Internal.Data.Array (Array)
import Streamly.Internal.Data.Unbox (Unbox)
import Streamly.Internal.Data.ParserK.Type

import qualified Streamly.Internal.Data.Array as Array
import qualified Streamly.Internal.Data.Array.Generic as GenArray

#include "inline.hs"

{-# DEPRECATED adaptC "Use Streamly.Data.Array.toParserK" #-}
{-# INLINE_LATE adaptC #-}
adaptC :: (Monad m, Unbox a) => Parser a m b -> ParserK (Array a) m b
adaptC = Array.toParserK

{-# DEPRECATED adaptCG "Use Streamly.Data.Array.Generic.toParserK" #-}
{-# INLINE_LATE adaptCG #-}
adaptCG ::
       Monad m => Parser a m b -> ParserK (GenArray.Array a) m b
adaptCG = GenArray.toParserK
