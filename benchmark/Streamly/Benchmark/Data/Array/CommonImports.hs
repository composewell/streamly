{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.IO.Class (MonadIO)

import qualified Streamly.Data.Scanl as Scanl
import qualified Streamly.Data.Stream as S
import qualified Streamly.Internal.Data.Stream as Stream

import Test.Tasty.Bench
import Streamly.Benchmark.Common hiding (benchPureSrc)

import Prelude as P hiding (map)
