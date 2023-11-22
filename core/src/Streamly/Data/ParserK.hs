-- |
-- Module      : Streamly.Data.ParserK
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : pre-release
-- Portability : GHC
--
-- See the general notes about parsing in the "Streamly.Data.Parser" module.
-- This module implements a using Continuation Passing Style (CPS) wrapper over
-- the "Streamly.Data.Parser" module. It is as fast or faster than attoparsec.
--
-- == Parser vs ParserK
--
-- 'ParserK' is preferred over 'Streamly.Data.Parser.Parser' when extensive
-- applicative, alternative and monadic composition is required, or when
-- recursive or dynamic composition of parsers is required. The
-- 'Streamly.Data.Parser.Parser' type fuses statically and creates efficient
-- loops whereas 'ParserK' uses function call based composition and has
-- comparatively larger runtime overhead but it is better suited to the
-- specific use cases mentioned above. 'ParserK' also allows to efficient parse
-- a stream of arrays, it can also break the input stream into a parse result
-- and remaining stream so that the stream can be parsed independently in
-- segments.
--
-- == Using ParserK
--
-- All the parsers from the "Streamly.Data.Parser" module can be adapted to
-- ParserK using the 'Streamly.Data.ParserK.adaptC',
-- 'Streamly.Internal.Data.ParserK.adapt', and
-- 'Streamly.Internal.Data.ParserK.adaptCG' combinators.
--
-- 'Streamly.Data.StreamK.parseChunks' runs a parser on a stream of unboxed
-- arrays, this is the preferred and most efficient way to parse chunked input.
-- The more general 'Streamly.Data.StreamK.parseBreakChunks' function returns
-- the remaining stream as well along with the parse result. There are
-- 'Streamly.Internal.Data.StreamK.parseChunksGeneric',
-- 'Streamly.Internal.Data.StreamK.parseBreakChunksGeneric' as well to run
-- parsers on boxed arrays. 'Streamly.Internal.Data.StreamK.parse',
-- 'Streamly.Internal.Data.StreamK.parseBreak' run parsers on a stream of
-- individual elements instead of stream of arrays.
--
-- == Monadic Composition
--
-- Monad composition can be used for lookbehind parsers, we can dynamically
-- compose new parsers based on the results of the previously parsed values.
--
-- >>> :m
-- >>> import Control.Applicative ((<|>))
-- >>> import Data.Char (isDigit, isAlpha)
-- >>> import Streamly.Data.Parser (Parser)
-- >>> import Streamly.Data.ParserK (ParserK)
-- >>> import qualified Streamly.Data.Parser as Parser
-- >>> import qualified Streamly.Data.ParserK as ParserK
-- >>> import qualified Streamly.Internal.Data.ParserK as ParserK (adapt)
--
-- If we have to parse "a9" or "9a" but not "99" or "aa" we can use the
-- following non-monadic, backtracking parser:
--
-- >>> digits p1 p2 = ((:) <$> p1 <*> ((:) <$> p2 <*> pure []))
-- >>> :{
-- backtracking :: Monad m => ParserK Char m String
-- backtracking = ParserK.adapt $
--     digits (Parser.satisfy isDigit) (Parser.satisfy isAlpha)
--     <|>
--     digits (Parser.satisfy isAlpha) (Parser.satisfy isDigit)
-- :}
--
-- We know that if the first parse resulted in a digit at the first place then
-- the second parse is going to fail.  However, we waste that information and
-- parse the first character again in the second parse only to know that it is
-- not an alphabetic char.  By using lookbehind in a 'Monad' composition we can
-- avoid redundant work:
--
-- >>> data DigitOrAlpha = Digit Char | Alpha Char
--
-- >>> :{
-- lookbehind :: Monad m => ParserK Char m String
-- lookbehind = do
--     x1 <- ParserK.adapt $
--              Digit <$> Parser.satisfy isDigit
--          <|> Alpha <$> Parser.satisfy isAlpha
--     -- Note: the parse depends on what we parsed already
--     x2 <- ParserK.adapt $
--           case x1 of
--              Digit _ -> Parser.satisfy isAlpha
--              Alpha _ -> Parser.satisfy isDigit
--     return $ case x1 of
--         Digit x -> [x,x2]
--         Alpha x -> [x,x2]
-- :}
--
-- == Experimental APIs
--
-- Please refer to "Streamly.Internal.Data.ParserK" for functions that have
-- not yet been released.
--
module Streamly.Data.ParserK
    (
    -- * Setup
    -- | To execute the code examples provided in this module in ghci, please
    -- run the following commands first.
    --
    -- $setup

    -- * Parser Type
      ParserK

    -- * Parsers
    -- ** Conversions
    , adapt
    , adaptC
    , adaptCG
    -- , toParser

    -- ** Without Input
    , fromPure
    , fromEffect
    , die

    -- * Deprecated
    , fromFold
    , fromParser
    )

where

import Control.Monad.IO.Class (MonadIO)
import Streamly.Internal.Data.Fold (Fold)
import Streamly.Internal.Data.Unbox (Unbox)
import Streamly.Internal.Data.Array (Array)
import qualified Streamly.Internal.Data.Parser as ParserD

import Streamly.Internal.Data.ParserK.Type

#include "DocTestDataParserK.hs"

{-# DEPRECATED fromFold "Please use \"ParserK.adaptC . Parser.fromFold\" instead." #-}
{-# INLINE fromFold #-}
fromFold :: (MonadIO m, Unbox a) => Fold m a b -> ParserK (Array a) m b
fromFold = adaptC . ParserD.fromFold

{-# DEPRECATED fromParser "Please use \"adaptC\" instead." #-}
{-# INLINE fromParser #-}
fromParser ::
       (MonadIO m, Unbox a) => ParserD.Parser a m b -> ParserK (Array a) m b
fromParser = adaptC
