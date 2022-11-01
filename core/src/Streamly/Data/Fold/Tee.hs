-- |
-- Module      : Streamly.Data.Fold.Tee
-- Copyright   : (c) 2021 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : released
-- Portability : GHC
--
-- The 'Tee' type is a newtype wrapper over the 'Streamly.Data.Fold.Fold' type providing
-- distributive 'Applicative', 'Semigroup', 'Monoid', 'Num', 'Floating' and
-- 'Fractional' instances. The input received by the composed 'Tee' is
-- replicated and distributed to both the constituent Tees.
--
-- For example, to compute the average of numbers in a stream without going
-- through the stream twice:
--
-- >>> import Streamly.Data.Fold.Tee (Tee(..))
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Fold as Fold
--
-- >>> avg = (/) <$> (Tee Fold.sum) <*> (Tee $ fmap fromIntegral Fold.length)
-- >>> Stream.fold (toFold avg) $ Stream.fromList [1.0..100.0]
-- 50.5
--
-- Similarly, the 'Semigroup' and 'Monoid' instances of 'Tee' distribute the
-- input to both the folds and combine the outputs using Monoid or Semigroup
-- instances of the output types:
--
-- >>> import Data.Monoid (Sum(..))
-- >>> t = Tee Fold.one <> Tee Fold.latest
-- >>> Stream.fold (toFold t) (fmap Sum $ Stream.enumerateFromTo 1.0 100.0)
-- Just (Sum {getSum = 101.0})
--
-- The 'Num', 'Floating', and 'Fractional' instances work in the same way.
--
module Streamly.Data.Fold.Tee
    ( Tee(..)
    )
where

import Streamly.Internal.Data.Fold.Tee
