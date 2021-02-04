module Main where

import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import System.Environment.Compat (unsetEnv)
import Test.DocTest (doctest)

main :: IO ()
main = do
    traverse_ putStrLn args
    unsetEnv "GHC_ENVIRONMENT"
    doctest args
  where
    args = flags ++ ["-outputdir=./_doctests/", "-fobject-code"] ++ pkgs ++ module_sources
