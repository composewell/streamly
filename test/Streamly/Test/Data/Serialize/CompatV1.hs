{-# LANGUAGE TemplateHaskell #-}

module Streamly.Test.Data.Serialize.CompatV1
    ( Rec(..)
    , River(..)
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Test.QuickCheck (Arbitrary, arbitrary, elements)
import Streamly.Internal.Data.Serialize (Serialize)
import qualified Streamly.Internal.Data.Serialize.TH as Serialize

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Rec a =
    Rec
        { initialField :: Int
        , otherField :: a
        , theLastField :: Maybe String
        , aNewField :: Maybe Char
        }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Rec a) where
    arbitrary = Rec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

$(Serialize.deriveSerializeWith
      (Serialize.defaultConfig {Serialize.recordSyntaxWithHeader = True})
      [d|instance Serialize a => Serialize (Rec a)|])

data River
    = Yamuna
    | Krishna
    | Godavari
    | Ganga
    deriving (Eq, Show, Read)

instance Arbitrary River where
    arbitrary = elements [Ganga, Yamuna, Godavari]

$(Serialize.deriveSerializeWith
      (Serialize.defaultConfig {Serialize.constructorTagAsString = True})
      [d|instance Serialize River|])
