{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Examples
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- To run these examples:
--
-- You need to build the library with the "examples" flag on e.g.
-- @stack build --flag streamly:examples@. To include the SDL examples as well
-- use @stack build --flag streamly:examples-sdl@. You will have to make sure
-- that you have the SDL OS package installed on your system and the headers
-- are visible to Haskell build tool.
--
-- You can directly evaluate the respective file and its main function using
-- ghc, like this (this may not work when built with @examples-sdl@ flag):
--
-- @
-- \$ stack ghc -- -e acidRainGame src\/Streamly\/Examples\/AcidRainGame.hs
-- @
--
-- Alternatively, you can create a file calling the main function and compile
-- it:
--
-- @
-- \$ cat ex.hs
-- import Streamly.Examples
-- main = acidRainGame
-- \$ stack ghc ex.hs
-- @
--
-- Alternatively, you can just import "Streamly.Examples" and evaluate the
-- respective function in GHCi.
--
module Streamly.Examples
    (
    -- Reactive Programming
      acidRainGame
#ifdef EXAMPLES_SDL
    , circlingSquare
#endif

    -- Concurrent Programming
    , listDirRecursive
    , mergeSortedStreams
    , searchEngineQuery
    )
where

import Streamly.Examples.AcidRainGame
#ifdef EXAMPLES_SDL
import Streamly.Examples.CirclingSquare
#endif
import Streamly.Examples.ListDirRecursive
import Streamly.Examples.MergeSortedStreams
import Streamly.Examples.SearchEngineQuery
