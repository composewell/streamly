module Main (main) where

import Distribution.Extra.Doctest (defaultMainAutoconfWithDoctests)

main :: IO ()
main = defaultMainAutoconfWithDoctests "doctests"
