{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Internal.Data.Serialize.TH.Common
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Serialize.TH.Common
    ( mkDeserializeExprOne
    , mkSerializeExprFields
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Language.Haskell.TH
import Streamly.Internal.Data.Serialize.Type
import Streamly.Internal.Data.Serialize.TH.Bottom

--------------------------------------------------------------------------------
-- Code
--------------------------------------------------------------------------------

mkDeserializeExprOne :: SimpleDataCon -> Q Exp
mkDeserializeExprOne (SimpleDataCon cname fields) =
    case fields of
        -- Only tag is serialized for unit fields, no actual value
        [] -> [|pure ($(varE (mkName "i0")), $(conE cname))|]
        _ ->
            doE
                (concat
                     [ fmap makeBind [0 .. (numFields - 1)]
                     , [ noBindS
                             (appE
                                  (varE 'pure)
                                  (tupE
                                       [ varE (makeI numFields)
                                       , appsE
                                             (conE cname :
                                              (map (varE . makeA)
                                                   [0 .. (numFields - 1)]))
                                       ]))
                       ]
                     ])
  where
    numFields = length fields
    makeBind i =
        bindS
            (tupP [varP (makeI (i + 1)), varP (makeA i)])
            [|deserialize $(varE (makeI i)) $(varE _arr) $(varE _endOffset)|]

mkSerializeExprFields :: [Field] -> Q Exp
mkSerializeExprFields fields =
    case fields of
        -- Unit constructor, do nothing just tag is enough
        [] -> [|pure ($(varE (mkName "i0")))|]
        _ ->
            doE
                (fmap makeBind [0 .. (numFields - 1)] ++
                 [noBindS ([|pure $(varE (makeI numFields))|])])
  where
    numFields = length fields
    makeBind i =
        bindS
            (varP (makeI (i + 1)))
            [|serialize $(varE (makeI i)) $(varE _arr) $(varE (mkFieldName i))|]
