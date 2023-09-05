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
    , Config(..)
    , defaultConfig
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
-- Constructor types
--------------------------------------------------------------------------------

data SimpleDataCon =
    SimpleDataCon Name [Field]
    deriving (Eq)

simplifyDataCon :: DataCon -> SimpleDataCon
simplifyDataCon (DataCon cname _ _ fields) = SimpleDataCon cname fields

data TypeOfType
    = UnitType Name             -- 1 constructor and 1 field
    | TheType SimpleDataCon      -- 1 constructor and 1+ fields
    | MultiType [SimpleDataCon] -- 1+ constructors
    deriving (Eq)

typeOfType :: Type -> [DataCon] -> TypeOfType
typeOfType headTy [] =
    error
        ("Attempting to get size with no constructors (" ++
         (pprint headTy) ++ ")")
typeOfType _ [DataCon cname _ _ []] = UnitType cname
typeOfType _ [con@(DataCon _ _ _ _)] = TheType $ simplifyDataCon con
typeOfType _ cons = MultiType $ map simplifyDataCon cons

isUnitType :: [DataCon] -> Bool
isUnitType [DataCon _ _ _ []] = True
isUnitType _ = False

--------------------------------------------------------------------------------
-- Size
--------------------------------------------------------------------------------

mkSizeOfExpr :: TypeOfType -> Q Exp
mkSizeOfExpr tyOfTy =
    case tyOfTy of
        UnitType _ -> lamE [varP _acc, wildP] [|$(varE _acc) + 1|]
        TheType con ->
            lamE
                [varP _acc, varP _x]
                (caseE (varE _x) [matchCons (varE _acc) con])
        MultiType constructors -> sizeOfHeadDt constructors

    where

    tagSizeExp numConstructors =
        litE (IntegerL (fromIntegral (getTagSize numConstructors)))

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

    matchCons acc (SimpleDataCon cname fields) =
        let expr = sizeOfFields acc (map snd fields)
         in matchConstructor cname (length fields) expr

    -- XXX We fix VarSize for simplicity. Should be changed later.
    sizeOfHeadDt cons =
        let numCons = length cons
            acc = [|$(varE _acc) + $(tagSizeExp numCons)|]
         in lamE
                [varP _acc, varP _x]
                (caseE (varE _x) (fmap (matchCons acc) cons))

--------------------------------------------------------------------------------
-- Peek
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


mkDeserializeExpr :: Type -> TypeOfType -> Q Exp
mkDeserializeExpr headTy tyOfTy =
    case tyOfTy of
        -- Unit constructor
        UnitType cname ->
            [|pure ($(varE _initialOffset) + 1, $(conE cname))|]
        -- Product type
        TheType con ->
            letE
                [valD (varP (mkName "i0")) (normalB (varE _initialOffset)) []]
                (mkDeserializeExprOne con)
        -- Sum type
        MultiType cons -> do
            let lenCons = length cons
                tagType = getTagType lenCons
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

mkSerializeExpr :: TypeOfType -> Q Exp
mkSerializeExpr tyOfTy =
    case tyOfTy of
        -- Unit type
        UnitType _ -> [|pure ($(varE _initialOffset) + 1)|]
        -- Product type
        (TheType (SimpleDataCon cname fields)) ->
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
        (MultiType cons) -> do
            let lenCons = length cons
                tagType = getTagType lenCons
            caseE
                (varE _val)
                (map (\(tagVal, (SimpleDataCon cname fields)) ->
                          matchConstructor
                              cname
                              (length fields)
                              (doE [ bindS
                                         (varP (mkName "i0"))
                                         (mkSerializeExprTag tagType tagVal)
                                   , noBindS (mkSerializeExprFields fields)
                                   ]))
                     (zip [0 ..] cons))

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
--       defaultConfig
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
       Config -> Cxt -> Type -> [DataCon] -> Q [Dec]
deriveSerializeInternal (Config{..}) preds headTy cons = do
    sizeOfMethod <- mkSizeOfExpr (typeOfType headTy cons)
    peekMethod <- mkDeserializeExpr headTy (typeOfType headTy cons)
    pokeMethod <- mkSerializeExpr (typeOfType headTy cons)
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
data Config =
    Config
        { unconstrained :: [String]
          -- ^ Type variables that should not have the 'Serialize' constraint.
          --
          -- @
          -- data CustomDataType a b c d = CustomDataType a c
          -- @
          --
          -- @
          -- conf = defaultConf {unconstrained = ["b", "d"]}
          -- \$(deriveSerializeWith conf ''CustomDataType)
          -- @
          --
          -- @
          -- instance (Serialize a, Serialize c) => Serialize (CustomDataType a b c d) where
          -- ...
          -- @
        , specializations :: [(String, Type)]
          -- ^ Specialize the type variable with the given type. All type
          -- variables listed here will not have the 'Serialize' constriant as
          -- they are specialized.
          --
          -- @
          -- data CustomDataType f a = CustomDataType (f a)
          -- @
          --
          -- @
          -- conf = defaultConf {specializations = [("f", ''Identity)]}
          -- \$(deriveSerializeWith conf ''CustomDataType)
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
        , constructorTagAsString :: Bool
          -- ^ __Experimental__
          --
          -- If True, encode constructors using the constructor names as Latin-1
          -- byte sequence.
        , recordSyntaxWithHeader :: Bool
          -- ^ __Experimental__
          --
          -- If True, encode the keys of the record as a header and then
          -- serialize the data.
        }

defaultConfig :: Config
defaultConfig =
    Config
        { unconstrained = []
        , specializations = []
        , inlineSize = Inline
        , inlineSerialize = Inline
        , inlineDeserialize = Inline
        , constructorTagAsString = False
        , recordSyntaxWithHeader = False
        }

-- | Similar to 'deriveSerialize,' but take a 'Config' to control how
-- the instance is generated.
--
-- Usage: @$(deriveSerializeWith config ''CustomDataType)@
deriveSerializeWith :: Config -> Name -> Q [Dec]
deriveSerializeWith conf@(Config {..}) name = do
    dt <- reifyDataType name
    let preds = map (unboxPred . VarT) (filterOutVars (dtTvs dt))
        headTy = appsT (ConT name) (map substituteVar (dtTvs dt))
        cons = dtCons dt
    deriveSerializeInternal conf preds headTy cons

    where
    allUnconstrainedTypeVars =
        unconstrained ++ map fst specializations
    filterOutVars vs =
        map mkName
            $ filter (not . flip elem allUnconstrainedTypeVars)
            $ map nameBase vs
    substituteVar v =
        case lookup (nameBase v) specializations of
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
-- >>> import qualified Streamly.Internal.Data.Serialize.TH as Serialize
-- >>> deriveSerialize = Serialize.deriveSerializeWith Serialize.defaultConfig
deriveSerialize :: Name -> Q [Dec]
deriveSerialize name = deriveSerializeWith defaultConfig name
