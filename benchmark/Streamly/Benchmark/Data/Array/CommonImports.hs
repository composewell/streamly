{-# LANGUAGE FlexibleContexts #-}

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common hiding (benchPureSrc)
import qualified Stream.Common as P

import Prelude as P hiding (map)
