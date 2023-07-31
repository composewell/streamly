{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Internal.Data.Serialize.TH
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Serialize.TH
    ( deriveSerialize
    , deriveSerializeWith
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Word (Word16, Word32, Word64, Word8)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Streamly.Internal.Data.Serialize

import Streamly.Internal.Data.Unbox.TH
    ( DataCon(..)
    , DataType(..)
    , appsT
    , plainInstanceD
    , reifyDataType
    )

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

type Field = (Maybe Name, Type)

_x :: Name
_x = mkName "x"

_arr :: Name
_arr = mkName "arr"

_tag :: Name
_tag = mkName "tag"

_initialOffset :: Name
_initialOffset = mkName "initialOffset"

_val :: Name
_val = mkName "val"

mkFieldName :: Int -> Name
mkFieldName i = mkName ("field" ++ show i)

makeI :: Int -> Name
makeI i = mkName $ "i" ++ show i

makeA :: Int -> Name
makeA i = mkName $ "a" ++ show i

--------------------------------------------------------------------------------
-- Domain specific helpers
--------------------------------------------------------------------------------

matchConstructor :: Name -> Int -> Q Exp -> Q Match
matchConstructor cname numFields exp0 =
    match
        (conP cname (map varP (map mkFieldName [0 .. (numFields - 1)])))
        (normalB exp0)
        []

exprGetSize :: Int -> Type -> Q Exp
exprGetSize i ty =
    caseE
        (sigE (varE 'size) (appT (conT ''Size) (pure ty)))
        [ match
              (conP 'VarSize [varP _f])
              (normalB (appE (varE _f) (varE (mkFieldName i))))
              []
        , match (conP 'ConstSize [varP _sz]) (normalB (varE _sz)) []
        ]

    where

    _f = mkName $ "f"
    _sz = mkName $ "sz"


getTagSize :: Int -> Int
getTagSize numConstructors
    | numConstructors == 1 = 0
    | fromIntegral (maxBound :: Word8) >= numConstructors = 1
    | fromIntegral (maxBound :: Word16) >= numConstructors = 2
    | fromIntegral (maxBound :: Word32) >= numConstructors = 4
    | fromIntegral (maxBound :: Word64) >= numConstructors = 8
    | otherwise = error "Too many constructors"

getTagType :: Int -> Name
getTagType numConstructors
    | numConstructors == 1 = error "No tag for 1 constructor"
    | fromIntegral (maxBound :: Word8) >= numConstructors = ''Word8
    | fromIntegral (maxBound :: Word16) >= numConstructors = ''Word16
    | fromIntegral (maxBound :: Word32) >= numConstructors = ''Word32
    | fromIntegral (maxBound :: Word64) >= numConstructors = ''Word64
    | otherwise = error "Too many constructors"

--------------------------------------------------------------------------------
-- Size
--------------------------------------------------------------------------------

isUnitType :: [DataCon] -> Bool
isUnitType [DataCon _ _ _ []] = True
isUnitType _ = False

mkSizeOfExpr :: Type -> [DataCon] -> Q Exp
mkSizeOfExpr headTy constructors =
    case constructors of
        [] ->
            [|error
                  ("Attempting to get size with no constructors (" ++
                   $(lift (pprint headTy)) ++ ")")|]
        -- One constructor with no fields is a unit type. Size of a unit type is
        -- 1.
        [constructor@(DataCon _ _ _ fields)] ->
            case fields of
                [] -> appE (conE 'ConstSize) (litE (IntegerL 1))
                _ ->
                    appE
                        (conE 'VarSize)
                        (lamE
                             [varP _x]
                             (caseE (varE _x) [matchCons constructor]))
        _ -> sizeOfHeadDt

    where

    tagSizeExp =
        litE (IntegerL (fromIntegral (getTagSize (length constructors))))

    sizeOfField (i, (_, ty)) = exprGetSize i ty

    sizeOfFields fields =
        appE (varE 'sum) (listE (map sizeOfField (zip [0..] fields)))

    matchCons (DataCon cname _ _ fields) =
        matchConstructor cname (length fields) (sizeOfFields fields)

    -- XXX We fix VarSize for simplicity. Should be changed later.
    sizeOfHeadDt =
        appE
            (conE 'VarSize)
            (lamE
                 [varP _x]
                 [|$(tagSizeExp)
                    + $(caseE (varE _x) (fmap matchCons constructors))|])

--------------------------------------------------------------------------------
-- Peek
--------------------------------------------------------------------------------

mkDeserializeExprOne :: DataCon -> Q Exp
mkDeserializeExprOne (DataCon cname _ _ fields) =
    case fields of
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
            [|deserialize $(varE (makeI i)) $(varE _arr)|]


mkDeserializeExpr :: Type -> [DataCon] -> Q Exp
mkDeserializeExpr headTy cons =
    case cons of
        [] ->
            [|error
                  ("Attempting to peek type with no constructors (" ++
                   $(lift (pprint headTy)) ++ ")")|]
        [(DataCon cname _ _ [])] ->
            [|pure ($(varE _initialOffset) + 1, $(conE cname))|]
        [con] ->
            letE
                [valD (varP (mkName "i0")) (normalB (varE _initialOffset)) []]
                (mkDeserializeExprOne con)
        _ ->
            doE
                [ bindS
                      (tupP [varP (mkName "i0"), varP _tag])
                      [|deserialize $(varE _initialOffset) $(varE _arr)|]
                , noBindS
                      (caseE
                           (sigE (varE _tag) (conT tagType))
                           (map peekMatch (zip [0 ..] cons) ++ [peekErr]))
                ]
  where
    lenCons = length cons
    tagType = getTagType lenCons
    peekMatch (i, con) =
        match (litP (IntegerL i)) (normalB (mkDeserializeExprOne con)) []
    peekErr =
        match
            wildP
            (normalB
                 [|error
                       ("Found invalid tag while peeking (" ++
                        $(lift (pprint headTy)) ++ ")")|])
            []

--------------------------------------------------------------------------------
-- Poke
--------------------------------------------------------------------------------

mkSerializeExprTag :: Name -> Int -> Q Exp
mkSerializeExprTag tagType tagVal =
    [|serialize
          $(varE _initialOffset)
          $(varE _arr)
          $((sigE (litE (IntegerL (fromIntegral tagVal))) (conT tagType)))|]

mkSerializeExprFields :: [Field] -> Q Exp
mkSerializeExprFields fields =
    case fields of
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

mkSerializeExpr :: Type -> [DataCon] -> Q Exp
mkSerializeExpr headTy cons =
    case cons of
        [] ->
            [|error
                  ("Attempting to poke type with no constructors (" ++
                   $(lift (pprint headTy)) ++ ")")|]
        [(DataCon _ _ _ [])] -> [|pure ($(varE _initialOffset) + 1)|]
        [(DataCon cname _ _ fields)] ->
            letE
                [valD (varP (mkName "i0")) (normalB (varE _initialOffset)) []]
                (caseE
                     (varE _val)
                     [ matchConstructor
                           cname
                           (length fields)
                           (mkSerializeExprFields fields)
                     ])
        _ ->
            caseE
                (varE _val)
                (map (\(tagVal, (DataCon cname _ _ fields)) ->
                          matchConstructor
                              cname
                              (length fields)
                              (doE [ bindS
                                         (varP (mkName "i0"))
                                         (mkSerializeExprTag tagType tagVal)
                                   , noBindS (mkSerializeExprFields fields)
                                   ]))
                     (zip [0 ..] cons))
  where
    lenCons = length cons
    tagType = getTagType lenCons

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- | A general function to derive Serialize instances where you can control
-- which Constructors of the datatype to consider and what the Context for the
-- 'Serialize' instance would be.
--
-- Consider the datatype:
-- @
-- data CustomDataType a b
--     = CDTConstructor1
--     | CDTConstructor2 Bool
--     | CDTConstructor3 Bool b
--     deriving (Show, Eq)
-- @
--
-- Usage:
-- @
-- $(deriveSerializeInternal
--       [AppT (ConT ''Serialize) (VarT (mkName "b"))]
--       (AppT
--            (AppT (ConT ''CustomDataType) (VarT (mkName "a")))
--            (VarT (mkName "b")))
--       [ DataCon 'CDTConstructor1 [] [] []
--       , DataCon 'CDTConstructor2 [] [] [(Nothing, (ConT ''Bool))]
--       , DataCon
--             'CDTConstructor3
--             []
--             []
--             [(Nothing, (ConT ''Bool)), (Nothing, (VarT (mkName "b")))]
--       ])
-- @
deriveSerializeInternal :: Cxt -> Type -> [DataCon] -> Q [Dec]
deriveSerializeInternal preds headTy cons = do
    sizeOfMethod <- mkSizeOfExpr headTy cons
    peekMethod <- mkDeserializeExpr headTy cons
    pokeMethod <- mkSerializeExpr headTy cons
    let methods =
            [ FunD 'size [Clause [] (NormalB sizeOfMethod) []]
            , FunD
                  'deserialize
                  [ Clause
                        (if isUnitType cons
                             then [VarP _initialOffset, WildP]
                             else [VarP _initialOffset, VarP _arr])
                        (NormalB peekMethod)
                        []
                  ]
            , FunD
                  'serialize
                  [ Clause
                        (if isUnitType cons
                             then [VarP _initialOffset, WildP, WildP]
                             else [VarP _initialOffset, VarP _arr, VarP _val])
                        (NormalB pokeMethod)
                        []
                  ]
            ]
    return [plainInstanceD preds (AppT (ConT ''Serialize) headTy) methods]

-- | Template haskell helper to create instances of 'Serialize' automatically.
--
-- Consider the datatype:
-- @
-- data CustomDataType a b c = ...
-- @
--
-- Usage: @$(deriveSerialize ''CustomDataType)@
--
-- Note: All type variables automatcally get an "Serialize" constraint.
-- The derived code will look like the following,
-- @
-- instance (Serialize a, Serialize b, Serialize c) => Serialize (CustomDataType a b c) where
-- ...
-- @
--
-- To control which type variables get the Serialize constraint, use
-- 'deriveSerializeWith'.
deriveSerialize :: Name -> Q [Dec]
deriveSerialize name = do
    dt <- reifyDataType name
    let preds = map (unboxPred . VarT) (dtTvs dt)
        headTy = appsT (ConT name) (map VarT (dtTvs dt))
        cons = dtCons dt
    deriveSerializeInternal preds headTy cons

    where

    unboxPred ty =
#if MIN_VERSION_template_haskell(2,10,0)
        AppT (ConT ''Serialize) ty
#else
        ClassP ''Serialize [ty]
#endif

-- | Like 'deriveSerialize' but control which types variables get the 'Serialize'
-- constraint.
--
-- Consider the datatype:
-- @
-- data CustomDataType a b c = ...
-- @
--
-- Usage: @$(deriveSerializeWith ["a", "c"] ''CustomDataType)@
--
-- @
-- instance (Serialize a, Serialize c) => Serialize (CustomDataType a b c) where
-- ...
-- @
--
deriveSerializeWith :: [String] -> Name -> Q [Dec]
deriveSerializeWith vars name = do
    dt <- reifyDataType name
    let preds = map (unboxPred . VarT) (fmap mkName vars)
        headTy = appsT (ConT name) (map VarT (dtTvs dt))
        cons = dtCons dt
    deriveSerializeInternal preds headTy cons

    where

    unboxPred ty =
#if MIN_VERSION_template_haskell(2,10,0)
        AppT (ConT ''Serialize) ty
#else
        ClassP ''Serialize [ty]
#endif
