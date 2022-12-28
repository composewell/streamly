module Main where

import TestRunner (mainWith)
import BuildLib (Quickness(..))
import Control.Applicative ((<|>))
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Map (Map)

import qualified Data.Map as Map
import Targets (targets)

rtsOpts :: Bool -> String -> String -> String
rtsOpts relaxMem exeName target =

    if relaxMem
    then "-K16M -M1024M"
    else "-K8M -M64M " <> targetSpecific

    where

    targetSpecific =
        case target of
            -- XXX Data.Array.* heap requirement increased for GHC-8.10
            "Data.Array.Unboxed" -> "-M128M"
            "Data.Array.Generic" -> "-M128M"
            "Prelude.Rate" -> "-M512M"
            -- For -O0 case writeChunks test fails, maybe we should have a
            -- separate flag for O0 case?
            "FileSystem.Handle" -> "-K16M"
            _ -> ""

main :: IO ()
main = mainWith targets rtsOpts
