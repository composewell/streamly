{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Test.Data.Serialize.TH
-- Copyright   : (c) 2022 Composewell technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Test.Data.Serialize.TH (genDatatype) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Language.Haskell.TH

import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (oneof)

--------------------------------------------------------------------------------
-- Generated types
--------------------------------------------------------------------------------

genDatatype :: String -> Int -> Q [Dec]
genDatatype tyName numCons =
    sequence
        ([ dataD
               (pure [])
               (mkName tyName)
               []
               Nothing
               (mkCon <$> [0 .. (numCons - 1)])
               [derivClause Nothing [conT ''Eq, conT ''Show]]
         , instanceD
               (pure [])
               (appT (conT ''Arbitrary) (conT (mkName tyName)))
               [ funD
                     'arbitrary
                      [ clause
                           []
                           (normalB
                               (appE
                                    (varE 'oneof)
                                    (listE
                                        (mkArbitraryExp <$>
                                            [0 .. (numCons - 1)]))))
                           []
                      ]
               ]
         ])

    where

    fieldTypeChoices =
        [ conT ''Int
        , conT ''Char
        , conT ''String
        , appT listT (conT ''Int)
        , appT listT (conT ''String)
        , appT listT (appT listT (conT ''Int))
        , appT listT (appT listT (conT ''String))
        ]

    chooseCycle i xs = xs !! (i `mod` length xs)

    mkArbitraryExp i =
        foldl
            (\b a -> [|$(b) <*> $(a)|])
            [|pure $(conE (mkName ("Constructor" ++ show i)))|]
            (replicate i (varE 'arbitrary))

    mkCon i =
        normalC
            (mkName ("Constructor" ++ show i))
            (mkField <$> [0..(i - 1)])

    mkField i =
        bangType
            (bang noSourceUnpackedness noSourceStrictness)
            (chooseCycle i fieldTypeChoices)
