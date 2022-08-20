{-# LANGUAGE FlexibleContexts #-}

import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import System.Random (randomRIO)

import qualified Streamly.Prelude  as S
import qualified Streamly.Internal.Data.Stream  as Stream

import Gauge
import Streamly.Benchmark.Common hiding (benchPureSrc)
import qualified Streamly.Benchmark.Prelude as P

import Prelude as P hiding (map)
