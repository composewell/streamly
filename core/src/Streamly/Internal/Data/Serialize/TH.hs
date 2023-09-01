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
    , SerializeTHConfig(..)
    , defaultSerializeTHConfig
    , deriveSerializeWith
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.List (foldl')
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

_acc :: Name
_acc = mkName "acc"

_arr :: Name
_arr = mkName "arr"

_tag :: Name
_tag = mkName "tag"

_initialOffset :: Name
_initialOffset = mkName "initialOffset"

_endOffset :: Name
_endOffset = mkName "endOffset"

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

exprGetSize :: Q Exp -> (Int, Type) -> Q Exp
exprGetSize acc (i, _) = [|size $(acc) $(varE (mkFieldName i))|]

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
        -- 1. XXX Use isUnitType?
        [constructor@(DataCon _ _ _ fields)] ->
            case fields of
                -- Unit type
                [] -> lamE [varP _acc, wildP] [| $(varE _acc) + 1 |]
                -- Product type
                _ ->
                    lamE
                        [varP _acc, varP _x]
                        (caseE (varE _x)
                             [matchCons (varE _acc) constructor])
        -- Sum type
        _ -> sizeOfHeadDt

    where

    tagSizeExp =
        litE (IntegerL (fromIntegral (getTagSize (length constructors))))

    -- XXX fields of the same type can be folded together, will reduce the code
    -- size when there are many fields of the same type.
    -- XXX const size fields can be calculated statically.
    -- XXX This can result in large compilation times due to nesting when there
    -- are many constructors. We can create a list and sum the list at run time
    -- to avoid that depending on the number of constructors. Or using a let
    -- statement for each case may help?
    -- appE (varE 'sum) (listE (acc : map (exprGetSize (litE (IntegerL 0))) (zip [0..] fields)))
    sizeOfFields acc fields =
        foldl' exprGetSize acc $ zip [0..] fields

    matchCons acc (DataCon cname _ _ fields) =
        let expr = sizeOfFields acc (map snd fields)
         in matchConstructor cname (length fields) expr

    -- XXX We fix VarSize for simplicity. Should be changed later.
    sizeOfHeadDt =
        let acc = [|$(varE _acc) + $(tagSizeExp)|]
         in lamE
                [varP _acc, varP _x]
                (caseE (varE _x) (fmap (matchCons acc) constructors))

--------------------------------------------------------------------------------
-- Peek
--------------------------------------------------------------------------------

mkDeserializeExprOne :: DataCon -> Q Exp
mkDeserializeExprOne (DataCon cname _ _ fields) =
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


mkDeserializeExpr :: Type -> [DataCon] -> Q Exp
mkDeserializeExpr headTy cons =
    case cons of
        [] ->
            [|error
                  ("Attempting to peek type with no constructors (" ++
                   $(lift (pprint headTy)) ++ ")")|]
        -- Unit constructor
        [(DataCon cname _ _ [])] ->
            [|pure ($(varE _initialOffset) + 1, $(conE cname))|]
        -- Product type
        [con] ->
            letE
                [valD (varP (mkName "i0")) (normalB (varE _initialOffset)) []]
                (mkDeserializeExprOne con)
        -- Sum type
        _ ->
            doE
                [ bindS
                      (tupP [varP (mkName "i0"), varP _tag])
                      [|deserialize $(varE _initialOffset) $(varE _arr) $(varE _endOffset)|]
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
                -- XXX Print the tag
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

mkSerializeExpr :: Type -> [DataCon] -> Q Exp
mkSerializeExpr headTy cons =
    case cons of
        [] ->
            [|error
                  ("Attempting to poke type with no constructors (" ++
                   $(lift (pprint headTy)) ++ ")")|]
        -- Unit type
        [(DataCon _ _ _ [])] -> [|pure ($(varE _initialOffset) + 1)|]
        -- Product type
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
        -- Sum type
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
--       defaultSerializeTHConfig
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
deriveSerializeInternal ::
       SerializeTHConfig -> Cxt -> Type -> [DataCon] -> Q [Dec]
deriveSerializeInternal (SerializeTHConfig{..}) preds headTy cons = do
    sizeOfMethod <- mkSizeOfExpr headTy cons
    peekMethod <- mkDeserializeExpr headTy cons
    pokeMethod <- mkSerializeExpr headTy cons
    let methods =
            -- INLINE on sizeOf actually worsens some benchmarks, and improves
            -- none
            [ PragmaD (InlineP 'size inlineSize FunLike AllPhases)
            , FunD 'size [Clause [] (NormalB sizeOfMethod) []]
            , PragmaD (InlineP 'deserialize inlineDeserialize FunLike AllPhases)
            , FunD
                  'deserialize
                  [ Clause
                        (if isUnitType cons
                             then [VarP _initialOffset, WildP, WildP]
                             else [VarP _initialOffset, VarP _arr, VarP _endOffset])
                        (NormalB peekMethod)
                        []
                  ]
            , PragmaD (InlineP 'serialize inlineSerialize FunLike AllPhases)
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

-- | Config to control how the 'Serialize' instance is generated.
data SerializeTHConfig =
    SerializeTHConfig
        { unconstrainedTypeVars :: [String]
          -- ^ Type variables that should not have the 'Serialize' constraint.
          --
          -- @
          -- data CustomDataType a b c d = CustomDataType a c
          -- @
          --
          -- @
          -- conf = defaultConf {unconstrainedTypeVars = ["b", "d"]}
          -- $(deriveSerializeWith conf ''CustomDataType)
          -- @
          --
          -- @
          -- instance (Serialize a, Serialize c) => Serialize (CustomDataType a b c d) where
          -- ...
          -- @
        , typeVarSubstitutions :: [(String, Type)]
          -- ^ Substitute the type variable with the given type. All type
          -- variables listed here will not have the 'Serialize' constriant as
          -- they are replaced.
          --
          -- @
          -- data CustomDataType f a = CustomDataType (f a)
          -- @
          --
          -- @
          -- conf = defaultConf {typeVarSubstitutions = [("f", ''Identity)]}
          -- $(deriveSerializeWith conf ''CustomDataType)
          -- @
          --
          -- @
          -- instance (Serialize a) => Serialize (CustomDataType Identity a) where
          -- ...
          -- @
          --
          -- @f@ is replaced with 'Identity' and becomes unconstrained.
        , inlineSize :: Inline
          -- ^ Inline value for 'size'. Default is Inline.
        , inlineSerialize :: Inline
          -- ^ Inline value for 'serialize'. Default is Inline.
        , inlineDeserialize :: Inline
          -- ^ Inline value for 'deserialize'. Default is Inline.
{-
        , constructorTagAsString :: Bool
          -- ^ If True, encode constructors as Latin-1 byte sequence. This
          -- allows addition, removal, and reordering of constructors. If False
          -- encode them as numbers. The default value is 'False'.
        , recordSyntaxWithHeader :: Bool
          -- ^ If True, constructors with record syntax will be encoded in a
          -- more compatible way. Allows addition, removal, and reordering of
          -- fields. The default value is 'False'.
-}
        }

defaultSerializeTHConfig :: SerializeTHConfig
defaultSerializeTHConfig =
    SerializeTHConfig
        { unconstrainedTypeVars = []
        , typeVarSubstitutions = []
        , inlineSize = Inline
        , inlineSerialize = Inline
        , inlineDeserialize = Inline
        }

-- | Similar to 'deriveSerialize,' but take a 'SerializeTHConfig' to control how
-- the instance is generated.
--
-- Usage: @$(deriveSerializeWith config ''CustomDataType)@
deriveSerializeWith :: SerializeTHConfig -> Name -> Q [Dec]
deriveSerializeWith conf@(SerializeTHConfig {..}) name = do
    dt <- reifyDataType name
    let preds = map (unboxPred . VarT) (filterOutVars (dtTvs dt))
        headTy = appsT (ConT name) (map substituteVar (dtTvs dt))
        cons = dtCons dt
    deriveSerializeInternal conf preds headTy cons

    where
    allUnconstrainedTypeVars =
        unconstrainedTypeVars ++ map fst typeVarSubstitutions
    filterOutVars vs =
        map mkName
            $ filter (not . flip elem allUnconstrainedTypeVars)
            $ map nameBase vs
    substituteVar v =
        case lookup (nameBase v) typeVarSubstitutions of
            Nothing -> VarT v
            Just ty -> ty

    unboxPred ty =
#if MIN_VERSION_template_haskell(2,10,0)
        AppT (ConT ''Serialize) ty
#else
        ClassP ''Serialize [ty]
#endif

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
-- To control which type variables don't get the Serialize constraint, use
-- 'deriveSerializeWith'.
--
-- >>> deriveSerialize = deriveSerializeWith 'defaultSerializeTHConfig'
deriveSerialize :: Name -> Q [Dec]
deriveSerialize name = deriveSerializeWith defaultSerializeTHConfig name
