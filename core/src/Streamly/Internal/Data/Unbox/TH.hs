{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Internal.Data.Unbox.TH
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Unbox.TH
    ( deriveUnbox
    , deriveUnboxWith

    -- th-helpers
    , DataCon(..)
    , DataType(..)
    , reifyDataType
    , appsT
    , plainInstanceD
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.Word (Word16, Word32, Word64, Word8)
import Data.Proxy (Proxy(..))

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Streamly.Internal.Data.Unbox

--------------------------------------------------------------------------------
-- th-utilities
--------------------------------------------------------------------------------

-- The following are copied to remove the dependency on th-utilities.
-- The code has been copied from th-abstraction and th-utilities.

-- Some CPP macros in the following code are not required but are kept
-- anyway. They can be removed if deemed as a maintainance burden.

#if MIN_VERSION_template_haskell(2,17,0)
type TyVarBndr_ flag = TyVarBndr flag
#else
type TyVarBndr_ flag = TyVarBndr
#endif

-- | Case analysis for a 'TyVarBndr'. If the value is a @'PlainTV' n _@, apply
-- the first function to @n@; if it is @'KindedTV' n _ k@, apply the second
-- function to @n@ and @k@.
elimTV :: (Name -> r) -> (Name -> Kind -> r) -> TyVarBndr_ flag -> r
#if MIN_VERSION_template_haskell(2,17,0)
elimTV ptv _ktv (PlainTV n _)    = ptv n
elimTV _ptv ktv (KindedTV n _ k) = ktv n k
#else
elimTV ptv _ktv (PlainTV n)    = ptv n
elimTV _ptv ktv (KindedTV n k) = ktv n k
#endif

-- | Extract the type variable name from a 'TyVarBndr', ignoring the
-- kind signature if one exists.
tvName :: TyVarBndr_ flag -> Name
tvName = elimTV id (\n _ -> n)

-- | Get the 'Name' of a 'TyVarBndr'
tyVarBndrName :: TyVarBndr_ flag -> Name
tyVarBndrName = tvName

appsT :: Type -> [Type] -> Type
appsT x [] = x
appsT x (y:xs) = appsT (AppT x y) xs

-- | Utility to conveniently handle change to 'InstanceD' API in
-- template-haskell-2.11.0
plainInstanceD :: Cxt -> Type -> [Dec] -> Dec
plainInstanceD =
#if MIN_VERSION_template_haskell(2,11,0)
    InstanceD Nothing
#else
    InstanceD
#endif

-- | Simplified info about a 'DataD'. Omits deriving, strictness,
-- kind info, and whether it's @data@ or @newtype@.
data DataType = DataType
    { dtName :: Name
    , dtTvs :: [Name]
    , dtCxt :: Cxt
    , dtCons :: [DataCon]
    } deriving (Eq, Show, Ord) --, Data, Typeable, Generic)

-- | Simplified info about a 'Con'. Omits deriving, strictness, and kind
-- info. This is much nicer than consuming 'Con' directly, because it
-- unifies all the constructors into one.
data DataCon = DataCon
    { dcName :: Name
    , dcTvs :: [Name]
    , dcCxt :: Cxt
    , dcFields :: [(Maybe Name, Type)]
    } deriving (Eq, Show, Ord) --, Data, Typeable, Generic)


-- | Convert a 'Con' to a list of 'DataCon'. The result is a list
-- because 'GadtC' and 'RecGadtC' can define multiple constructors.
conToDataCons :: Con -> [DataCon]
conToDataCons = \case
    NormalC name slots ->
        [DataCon name [] [] (map (\(_, ty) -> (Nothing, ty)) slots)]
    RecC name fields ->
        [DataCon name [] [] (map (\(n, _, ty) -> (Just n, ty)) fields)]
    InfixC (_, ty1) name (_, ty2) ->
        [DataCon name [] [] [(Nothing, ty1), (Nothing, ty2)]]
    ForallC tvs preds con ->
        map (\(DataCon name tvs0 preds0 fields) ->
            DataCon name (tvs0 ++ map tyVarBndrName tvs) (preds0 ++ preds) fields) (conToDataCons con)
#if MIN_VERSION_template_haskell(2,11,0)
    GadtC ns slots _ ->
        map (\dn -> DataCon dn [] [] (map (\(_, ty) -> (Nothing, ty)) slots)) ns
    RecGadtC ns fields _ ->
        map (\dn -> DataCon dn [] [] (map (\(fn, _, ty) -> (Just fn, ty)) fields)) ns
#endif

-- | Reify the given data or newtype declaration, and yields its
-- 'DataType' representation.
reifyDataType :: Name -> Q DataType
reifyDataType name = do
    info <- reify name
    case infoToDataType info of
        Nothing -> fail $ "Expected to reify a datatype. Instead got:\n" ++ pprint info
        Just x -> return x

infoToDataType :: Info -> Maybe DataType
infoToDataType info = case info of
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (DataD preds name tvs _kind cons _deriving) ->
#else
    TyConI (DataD preds name tvs cons _deriving) ->
#endif
        Just $ DataType name (map tyVarBndrName tvs) preds (concatMap conToDataCons cons)
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (NewtypeD preds name tvs _kind con _deriving) ->
#else
    TyConI (NewtypeD preds name tvs con _deriving) ->
#endif
        Just $ DataType name (map tyVarBndrName tvs) preds (conToDataCons con)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

type Field = (Maybe Name, Type)

_arr :: Name
_arr = mkName "arr"

_tag :: Name
_tag = mkName "tag"

_initialOffset :: Name
_initialOffset = mkName "initialOffset"

_val :: Name
_val = mkName "val"

mkOffsetName :: Int -> Name
mkOffsetName i = mkName ("offset" ++ show i)

mkFieldName :: Int -> Name
mkFieldName i = mkName ("field" ++ show i)

--------------------------------------------------------------------------------
-- Domain specific helpers
--------------------------------------------------------------------------------

exprGetSize :: Type -> Q Exp
exprGetSize ty = appE (varE 'sizeOf) [|Proxy :: Proxy $(pure ty)|]

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

mkOffsetDecls :: Int -> [Field] -> [Q Dec]
mkOffsetDecls tagSize fields =
    init
        ((:) (valD
                  (varP (mkOffsetName 0))
                  (normalB
                       [|$(litE (IntegerL (fromIntegral tagSize))) +
                         $(varE _initialOffset)|])
                  [])
             (map mkOffsetExpr (zip [1 ..] fields)))

    where

    mkOffsetExpr (i, (_, ty)) =
        valD
            (varP (mkOffsetName i))
            (normalB [|$(varE (mkOffsetName (i - 1))) + $(exprGetSize ty)|])
            []

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
        [con@(DataCon _ _ _ fields)] ->
            case fields of
                [] -> litE (IntegerL 1)
                _ -> [|$(sizeOfConstructor con)|]
        _ -> [|$(litE (IntegerL (fromIntegral tagSize))) + $(sizeOfHeadDt)|]

    where

    tagSize = getTagSize (length constructors)
    sizeOfField (_, ty) = exprGetSize ty
    sizeOfConstructor (DataCon _ _ _ fields) =
        appE (varE 'sum) (listE (map sizeOfField fields))
    -- The size of any Unbox type is atleast 1
    sizeOfHeadDt =
        appE (varE 'maximum) (listE (map sizeOfConstructor constructors))

--------------------------------------------------------------------------------
-- Peek
--------------------------------------------------------------------------------

mkPeekExprOne :: Int -> DataCon -> Q Exp
mkPeekExprOne tagSize (DataCon cname _ _ fields) =
    case fields of
        [] -> [|pure $(conE cname)|]
        _ ->
            letE
                (mkOffsetDecls tagSize fields)
                (foldl
                     (\acc i -> [|$(acc) <*> $(peekField i)|])
                     [|$(conE cname) <$> $(peekField 0)|]
                     [1 .. (length fields - 1)])

    where

    peekField i = [|peekByteIndex $(varE (mkOffsetName i)) $(varE _arr)|]

mkPeekExpr :: Type -> [DataCon] -> Q Exp
mkPeekExpr headTy cons =
    case cons of
        [] ->
            [|error
                  ("Attempting to peek type with no constructors (" ++
                   $(lift (pprint headTy)) ++ ")")|]
        [con] -> mkPeekExprOne 0 con
        _ ->
            doE
                [ bindS
                      (varP _tag)
                      [|peekByteIndex $(varE _initialOffset) $(varE _arr)|]
                , noBindS
                      (caseE
                           (sigE (varE _tag) (conT tagType))
                           (map peekMatch (zip [0 ..] cons) ++ [peekErr]))
                ]

    where

    lenCons = length cons
    tagType = getTagType lenCons
    tagSize = getTagSize lenCons
    peekMatch (i, con) =
        match (litP (IntegerL i)) (normalB (mkPeekExprOne tagSize con)) []
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

mkPokeExprTag :: Name -> Int -> Q Exp
mkPokeExprTag tagType tagVal = pokeTag

    where

    pokeTag =
        [|pokeByteIndex
              $(varE _initialOffset)
              $(varE _arr)
              $((sigE (litE (IntegerL (fromIntegral tagVal))) (conT tagType)))|]

mkPokeExprFields :: Int -> [Field] -> Q Exp
mkPokeExprFields tagSize fields = do
    case fields of
        [] -> [|pure ()|]
        _ ->
            letE
                (mkOffsetDecls tagSize fields)
                (doE $ map (noBindS . pokeField) [0 .. (numFields - 1)])

    where

    numFields = length fields
    pokeField i =
        [|pokeByteIndex
              $(varE (mkOffsetName i))
              $(varE _arr)
              $(varE (mkFieldName i))|]

mkPokeMatch :: Name -> Int -> Q Exp -> Q Match
mkPokeMatch cname numFields exp0 =
    match
        (conP cname (map varP (map mkFieldName [0 .. (numFields - 1)])))
        (normalB exp0)
        []

mkPokeExpr :: Type -> [DataCon] -> Q Exp
mkPokeExpr headTy cons =
    case cons of
        [] ->
            [|error
                  ("Attempting to poke type with no constructors (" ++
                   $(lift (pprint headTy)) ++ ")")|]
        [(DataCon _ _ _ [])] -> [|pure ()|]
        [(DataCon cname _ _ fields)] ->
            caseE
                (varE _val)
                [mkPokeMatch cname (length fields) (mkPokeExprFields 0 fields)]
        _ ->
            caseE
                (varE _val)
                (map (\(tagVal, (DataCon cname _ _ fields)) ->
                          mkPokeMatch
                              cname
                              (length fields)
                              (doE [ noBindS $ mkPokeExprTag tagType tagVal
                                   , noBindS $ mkPokeExprFields tagSize fields
                                   ]))
                     (zip [0 ..] cons))

    where

    lenCons = length cons
    tagType = getTagType lenCons
    tagSize = getTagSize lenCons

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- | A general function to derive Unbox instances where you can control which
-- Constructors of the datatype to consider and what the Context for the 'Unbox'
-- instance would be.
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
-- $(deriveUnboxInternal
--       [AppT (ConT ''Unbox) (VarT (mkName "b"))]
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
deriveUnboxInternal :: Cxt -> Type -> [DataCon] -> Q [Dec]
deriveUnboxInternal preds headTy cons = do
    sizeOfMethod <- mkSizeOfExpr headTy cons
    peekMethod <- mkPeekExpr headTy cons
    pokeMethod <- mkPokeExpr headTy cons
    let methods =
            [ FunD 'sizeOf [Clause [WildP] (NormalB sizeOfMethod) []]
            , FunD
                  'peekByteIndex
                  [ Clause
                        (if isUnitType cons
                             then [WildP, WildP]
                             else [VarP _initialOffset, VarP _arr])
                        (NormalB peekMethod)
                        []
                  ]
            , FunD
                  'pokeByteIndex
                  [ Clause
                        (if isUnitType cons
                             then [WildP, WildP, WildP]
                             else [VarP _initialOffset, VarP _arr, VarP _val])
                        (NormalB pokeMethod)
                        []
                  ]
            ]
    return [plainInstanceD preds (AppT (ConT ''Unbox) headTy) methods]

-- | Template haskell helper to create instances of 'Unbox' automatically.
--
-- Consider the datatype:
-- @
-- data CustomDataType a b c = ...
-- @
--
-- Usage: @$(deriveUnbox ''CustomDataType)@
--
-- Note: All type variables automatcally get an "Unbox" constraint.
-- The derived code will look like the following,
-- @
-- instance (Unbox a, Unbox b, Unbox c) => Unbox (CustomDataType a b c) where
-- ...
-- @
--
-- To control which type variables get the Unbox constraint, use
-- 'deriveUnboxWith'.
deriveUnbox :: Name -> Q [Dec]
deriveUnbox name = do
    dt <- reifyDataType name
    let preds = map (unboxPred . VarT) (dtTvs dt)
        headTy = appsT (ConT name) (map VarT (dtTvs dt))
        cons = dtCons dt
    deriveUnboxInternal preds headTy cons

    where

    unboxPred ty =
#if MIN_VERSION_template_haskell(2,10,0)
        AppT (ConT ''Unbox) ty
#else
        ClassP ''Unbox [ty]
#endif

-- | Like 'deriveUnbox' but control which type variables get the 'Unbox'
-- constraint.
--
-- Consider the datatype:
-- @
-- data CustomDataType a b c = ...
-- @
--
-- Usage: @$(deriveUnboxWith ["a", "c"] ''CustomDataType)@
--
-- @
-- instance (Unbox a, Unbox c) => Unbox (CustomDataType a b c) where
-- ...
-- @
--
deriveUnboxWith :: [String] -> Name -> Q [Dec]
deriveUnboxWith vars name = do
    dt <- reifyDataType name
    let preds = map (unboxPred . VarT) (fmap mkName vars)
        headTy = appsT (ConT name) (map VarT (dtTvs dt))
        cons = dtCons dt
    deriveUnboxInternal preds headTy cons

    where

    unboxPred ty =
#if MIN_VERSION_template_haskell(2,10,0)
        AppT (ConT ''Unbox) ty
#else
        ClassP ''Unbox [ty]
#endif
