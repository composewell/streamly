{-# LANGUAGE TemplateHaskell #-}

module Streamly.Test.Data.Serialize.CompatV0
    ( Rec(..)
    , River(..)
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Streamly.Internal.Data.Serialize (Serialize)
import qualified Streamly.Internal.Data.Serialize as Serialize

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Rec a =
    Rec
        { initialField :: Int
        , otherField :: a
        , theLastField :: Maybe String
        }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Rec a) where
    arbitrary = Rec <$> arbitrary <*> arbitrary <*> arbitrary

$(Serialize.deriveSerializeWith
      (Serialize.encodeRecordFields True Serialize.defaultConfig)
      [d|instance Serialize a => Serialize (Rec a)|])

data River
    = Ganga
    | Yamuna
    | Godavari
    deriving (Eq, Show, Read)

instance Arbitrary River where
    arbitrary = elements [Ganga, Yamuna, Godavari]

$(Serialize.deriveSerializeWith
      (Serialize.encodeConstrNames True Serialize.defaultConfig)
      [d|instance Serialize River|])
