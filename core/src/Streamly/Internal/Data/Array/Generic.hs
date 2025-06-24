-- |
-- Module      : Streamly.Internal.Data.Array.Generic
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
module Streamly.Internal.Data.Array.Generic
    (
    module Streamly.Internal.Data.Array.Generic.Type

    -- * Parsing Stream of Arrays
    , parse
    , parsePos
    , parseBreak
    , parseBreakPos
    )
where

import Streamly.Internal.Data.Parser (ParseError(..))
import Streamly.Internal.Data.StreamK.Type (StreamK)

import qualified Streamly.Internal.Data.ParserDrivers as Drivers
import qualified Streamly.Internal.Data.ParserK.Type as ParserK

import Prelude hiding (Foldable(..), read)
import Streamly.Internal.Data.Array.Generic.Type

-------------------------------------------------------------------------------
-- ParserK Chunked Generic
-------------------------------------------------------------------------------

{-# INLINE parseBreak #-}
parseBreak
    :: forall m a b. Monad m
    => ParserK.ParserK (Array a) m b
    -> StreamK m (Array a)
    -> m (Either ParseError b, StreamK m (Array a))
parseBreak = Drivers.parseBreakChunksGeneric

-- | Like 'parseBreak' but includes stream position information in the error
-- messages.
--
{-# INLINE parseBreakPos #-}
parseBreakPos
    :: forall m a b. Monad m
    => ParserK.ParserK (Array a) m b
    -> StreamK m (Array a)
    -> m (Either ParseError b, StreamK m (Array a))
parseBreakPos = Drivers.parseBreakChunksGenericPos

{-# INLINE parse #-}
parse ::
       (Monad m)
    => ParserK.ParserK (Array a) m b
    -> StreamK m (Array a)
    -> m (Either ParseError b)
parse f = fmap fst . parseBreak f

{-# INLINE parsePos #-}
parsePos ::
       (Monad m)
    => ParserK.ParserK (Array a) m b
    -> StreamK m (Array a)
    -> m (Either ParseError b)
parsePos f = fmap fst . parseBreakPos f
