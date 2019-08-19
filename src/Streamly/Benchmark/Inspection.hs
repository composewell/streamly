-- |
-- Module      : Streamly.Benchmark.Inspection
-- Copyright   : (c) 2019 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}

module Streamly.Benchmark.Inspection
    (
      hinspect
    )
where

import Test.Inspection
import Language.Haskell.TH (Q, Dec)

hinspect :: Obligation -> Q [Dec]
#ifdef __HADDOCK_VERSION__
hinspect _ = return []
#else
hinspect = inspect
#endif
