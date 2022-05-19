{-# LANGUAGE FlexibleContexts #-}

import Control.DeepSeq (NFData(..))
import System.Random (randomRIO)

import qualified Streamly.Prelude  as S
import Streamly.Internal.Data.Stream.Serial.Type (SerialT)

import Gauge
import Streamly.Benchmark.Common hiding (benchPureSrc)
import qualified Streamly.Benchmark.Prelude as P


import Control.Monad.IO.Class (MonadIO)

import Prelude as P hiding (map)

import Data.Functor ((<&>))
