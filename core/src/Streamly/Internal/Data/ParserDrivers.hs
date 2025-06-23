{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.ParserDrivers
-- Copyright   : (c) 2018 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.ParserDrivers
    (
    -- * Running a Parser
      parseBreak
    , parseBreakPos
    , parseBreakStreamK
    , parseBreakStreamKPos
    , parseBreakChunks
    , parseBreakChunksPos
    , parseBreakChunksGeneric
    , parseBreakChunksGenericPos
    , parseMany
    , parseManyPos
    , parseIterate
    , parseIteratePos
    )
    where

#include "assert.hs"
#include "inline.hs"
#include "ArrayMacros.h"

import Data.Proxy (Proxy(..))
import Fusion.Plugin.Types (Fuse(..))
import GHC.Exts (SpecConstrAnnotation(..))
import GHC.Types (SPEC(..))
import Streamly.Internal.Data.Array.Type (Array(..))
import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.ParserK.Type (ParserK)
import Streamly.Internal.Data.StreamK.Type (StreamK)
import Streamly.Internal.Data.SVar.Type (adaptState, defState)
import Streamly.Internal.Data.Unbox (Unbox(..))

import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Array.Generic.Type as GArray
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser as PRD
import qualified Streamly.Internal.Data.ParserK.Type as ParserK
import qualified Streamly.Internal.Data.Stream.Type as Nesting
import qualified Streamly.Internal.Data.Stream.Type as Stream
import qualified Streamly.Internal.Data.Stream.Generate as StreamD
import qualified Streamly.Internal.Data.StreamK.Type as StreamK

import Streamly.Internal.Data.Stream.Type hiding (splitAt)
import Prelude hiding (splitAt)

-- GHC parser does not accept {-# ANN type [] NoSpecConstr #-}, so we need
-- to make a newtype.
{-# ANN type List NoSpecConstr #-}
newtype List a = List {getList :: [a]}

-- The backracking buffer consists of arrays in the most-recent-first order. We
-- want to take a total of n array elements from this buffer. Note: when we
-- have to take an array partially, we must take the last part of the array.
{-# INLINE backtrack #-}
backtrack :: forall m a. Unbox a =>
       Int
    -> [Array a]
    -> StreamK m (Array a)
    -> (StreamK m (Array a), [Array a])
backtrack count buf inp
  | count < 0 = seekOver count
  -- XXX this is handled at the call site, so we can assert that here.
  | count == 0 = (inp, buf)
  | otherwise = go count buf inp

    where

    go n [] _ = seekUnder count n
    go n (x:xs) stream =
        let len = Array.length x
        in if n > len
           then go (n - len) xs (StreamK.cons x stream)
           else if n == len
           then (StreamK.cons x stream, xs)
           else let !(Array contents start end) = x
                    !start1 = end - (n * SIZE_OF(a))
                    arr1 = Array contents start1 end
                    arr2 = Array contents start start1
                 in (StreamK.cons arr1 stream, arr2:xs)

    seekOver x =
        error $ "Array.parseBreak: bug in parser, seeking ["
            ++ show (negate x)
            ++ "] elements in future"

    seekUnder x y =
        error $ "Array.parseBreak: bug in parser, backtracking ["
            ++ show x
            ++ "] elements. Goes ["
            ++ show y
            ++ "] elements beyond backtrack buffer"

{-# INLINE backtrackGeneric #-}
backtrackGeneric ::
       Int
    -> [GArray.Array a]
    -> StreamK m (GArray.Array a)
    -> (StreamK m (GArray.Array a), [GArray.Array a])
backtrackGeneric count buf inp
  | count < 0 = seekOver count
  | count == 0 = (inp, buf)
  | otherwise = go count buf inp

    where

    go n [] _ = seekUnder count n
    go n (x:xs) stream =
        let len = GArray.length x
        in if n > len
           then go (n - len) xs (StreamK.cons x stream)
           else if n == len
           then (StreamK.cons x stream, xs)
           else let arr1 = GArray.unsafeSliceOffLen (len - n) n x
                    arr2 = GArray.unsafeSliceOffLen 0 (len - n) x
                 in (StreamK.cons arr1 stream, arr2:xs)

    seekOver x =
        error $ "Array.Generic.parseBreak: bug in parser, seeking ["
            ++ show (negate x)
            ++ "] elements in future"

    seekUnder x y =
        error $ "Array.Generic.parseBreak: bug in parser, backtracking ["
            ++ show x
            ++ "] elements. Goes ["
            ++ show y
            ++ "] elements beyond backtrack buffer"

#include "ParserDrivers.h"
#define PARSER_WITH_POS
#include "ParserDrivers.h"
