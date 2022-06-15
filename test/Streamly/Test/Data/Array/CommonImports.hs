
import Streamly.Internal.Data.Unboxed (sizeOf)

import Test.Hspec.QuickCheck
import Test.QuickCheck (Property, forAll, Gen, vectorOf, arbitrary, choose)
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import Test.Hspec as H

import Streamly.Data.Fold (Fold)
import Streamly.Prelude (SerialT)
import Streamly.Test.Common (listEquals)

import qualified Streamly.Prelude as S
