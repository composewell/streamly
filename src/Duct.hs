-- |
-- Module      : Duct
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : MIT-style
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

module Duct
    ( waitAsync
    , async
    , sample
    , threads
    )
where

import Duct.AsyncT
import Duct.Threads
