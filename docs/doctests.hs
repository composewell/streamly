module Main where

import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import System.Environment.Compat (getArgs, unsetEnv)
import Test.DocTest (doctest)

import qualified Data.List as List

main :: IO ()
main = do
    cliArgs <- getArgs
    let (cliArgs1, mods) =
            if (List.null (filter (== "--modules") cliArgs))
            then (cliArgs, module_sources)
            else (List.delete "--modules" cliArgs, [])
    let args =
              [ "-outputdir=./_doctests/"
              , "-fobject-code"
              , "--fast"
              ]
            ++ cliArgs1
            ++ flags
            ++ pkgs
            ++ mods
    traverse_ putStrLn args
    unsetEnv "GHC_ENVIRONMENT"
    doctest args
