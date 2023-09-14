{-# LANGUAGE TemplateHaskell #-}

module Streamly.Benchmark.Data.Serialize.TH (genLargeRecord) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Language.Haskell.TH

import Streamly.Internal.Data.Serialize.TH.Bottom (makeI)
import Control.DeepSeq (NFData(..))

--------------------------------------------------------------------------------
-- Large Record
--------------------------------------------------------------------------------

genLargeRecord :: String -> Int -> Q [Dec]
genLargeRecord tyName numFields =
    sequence
        ([ dataD
               (pure [])
               (mkName tyName)
               []
               Nothing
               [mkCon tyName]
               [derivClause Nothing [conT ''Eq, conT ''Show]]
         , mkValueSigDec
         , mkValueDec
         , nfDataInstance tyName
         ])

    where

    fieldTypeChoices = [conT ''()]
    chooseCycle i xs = xs !! (i `mod` length xs)
    nfDataInstance nm =
        instanceD
            (pure [])
            (appT (conT ''NFData) (conT (mkName nm)))
            [ funD
                  'rnf
                  [ clause
                        [ conP
                              (mkName nm)
                              (varP . makeI <$> [0 .. (numFields - 1)])
                        ]
                        (normalB
                             (foldl
                                  (\b a -> [|rnf $(b) `seq` rnf $(a)|])
                                  [|()|]
                                  (varE . makeI <$> [0 .. (numFields - 1)])))
                        []
                  ]
            ]
    valueName = mkName $ "val" ++ tyName
    mkValueSigDec = sigD valueName [t|$(conT (mkName tyName))|]
    mkValueDec =
        funD
            valueName
            [ clause
                  []
                  (normalB
                       (foldl
                            (\b a -> [|$(b) $(a)|])
                            (conE (mkName tyName))
                            (const (conE '()) <$> [0 .. (numFields - 1)])))
                  []
            ]
    mkCon nm = recC (mkName nm) (mkField <$> [0 .. (numFields - 1)])
    mkField i =
        varBangType
            (mkName ("field" ++ show i))
            (bangType
                 (bang noSourceUnpackedness noSourceStrictness)
                 (chooseCycle i fieldTypeChoices))
