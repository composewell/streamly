-- |
-- Module      : Streamly.Internal.Data.Consable
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Consable
    ( Consable (..)
    )
where

import Streamly.Internal.Data.Stream.Serial (SerialT)
import qualified Streamly.Internal.Data.Stream.StreamK as Serial
import Prelude hiding (map)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

infixr 5 `cons`

-- Originally to unify some operations (builder) on lists and streams.
--
-- | Pure list like structures that can be constructed using "cons" and "nil".
class Consable t where
    -- | An empty value.
    nil :: t a
    -- | A right associative cons operation.
    cons :: a -> t a -> t a

instance Consable [] where
    nil = []
    cons = (:)

-- XXX should move to Stream.Serial?
instance Consable (SerialT m) where
    nil = Serial.nil
    cons = Serial.cons
