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
    (
    -- Deriving
      deriveSerialize
    , deriveSerializeWith

    -- Utilities
    , module Streamly.Internal.Data.Serialize.TH.Bottom
    -- ** Common
    , module Streamly.Internal.Data.Serialize.TH.Common
    -- ** RecHeader
    , module Streamly.Internal.Data.Serialize.TH.RecHeader
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Data.List (foldl')
import Data.Word (Word16, Word32, Word64, Word8)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Streamly.Internal.Data.Serialize.Type

import Streamly.Internal.Data.Unbox.TH
    ( DataCon(..)
    , DataType(..)
    , reifyDataType
    )

import qualified Streamly.Internal.Data.Serialize.TH.RecHeader as RecHeader

import Streamly.Internal.Data.Serialize.TH.Bottom
import Streamly.Internal.Data.Serialize.TH.Common
import Streamly.Internal.Data.Serialize.TH.RecHeader

--------------------------------------------------------------------------------
-- Domain specific helpers
--------------------------------------------------------------------------------

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

getNameBaseLen :: Name -> Word8
getNameBaseLen cname =
    let x = length (nameBase cname)
     in if x > 63
        then error "Max Constructor Len: 63 characters"
        else fromIntegral x

conEncLen :: Name -> Word8
conEncLen cname = getNameBaseLen cname + 1

--------------------------------------------------------------------------------
-- Size
--------------------------------------------------------------------------------

mkSizeOfExpr :: Bool -> Bool -> TypeOfType -> Q Exp
mkSizeOfExpr True False tyOfTy =
    case tyOfTy of
        UnitType cname ->
            lamE
                [varP _acc, wildP]
                [|$(varE _acc) + $(litIntegral (conEncLen cname))|]
        TheType con ->
            lamE
                [varP _acc, varP _x]
                (caseE (varE _x) [matchCons (varE _acc) con])
        MultiType constructors -> sizeOfHeadDt constructors

    where

    sizeOfFields acc fields =
        foldl' exprGetSize acc $ zip [0..] fields

    matchCons acc (SimpleDataCon cname fields) =
        let a = litIntegral (conEncLen cname)
            b = sizeOfFields acc (map snd fields)
            expr = [|$(a) + $(b)|]
         in matchConstructor cname (length fields) expr

    sizeOfHeadDt cons =
        let acc = [|$(varE _acc)|]
         in lamE
                [varP _acc, varP _x]
                (caseE (varE _x) (fmap (matchCons acc) cons))

mkSizeOfExpr False False tyOfTy =
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

mkSizeOfExpr False True (TheType con) = RecHeader.mkRecSizeOfExpr con

mkSizeOfExpr _ _ _ = errorUnimplemented

mkSizeDec :: SerializeConfig -> Type -> [DataCon] -> Q [Dec]
mkSizeDec (SerializeConfig {..}) headTy cons = do
    -- INLINE on sizeOf actually worsens some benchmarks, and improves none
    sizeOfMethod <-
        mkSizeOfExpr
            cfgConstructorTagAsString
            cfgRecordSyntaxWithHeader
            (typeOfType headTy cons)
    pure
        ( maybe
            []
            (\x -> [PragmaD (InlineP 'size x FunLike AllPhases)])
            cfgInlineSize
         ++ [FunD 'size [Clause [] (NormalB sizeOfMethod) []]]
        )

--------------------------------------------------------------------------------
-- Peek
--------------------------------------------------------------------------------

mkDeserializeExpr :: Bool -> Bool -> Type -> TypeOfType -> Q Exp
mkDeserializeExpr True False headTy tyOfTy =
    case tyOfTy of
        UnitType cname -> deserializeConsExpr [SimpleDataCon cname []]
        TheType con -> deserializeConsExpr [con]
        MultiType cons -> deserializeConsExpr cons

  where

    deserializeConsExpr cons = do
        conLen <- newName "conLen"
        off1 <- newName "off1"
        [|do ($(varP off1), $(varP conLen) :: Word8) <-
                 deserialize
                     $(varE _initialOffset)
                     $(varE _arr)
                     $(varE _endOffset)
             $(multiIfE (map (guardCon conLen off1) cons ++ [catchAll]))|]

    catchAll =
        normalGE
            [|True|]
            [|error
               ("Found invalid tag while peeking (" ++
                   $(lift (pprint headTy)) ++ ")")|]

    guardCon conLen off con@(SimpleDataCon cname _) = do
        let lenCname = getNameBaseLen cname
            tag = map c2w (nameBase cname)
        normalGE
            [|($(litIntegral lenCname) == $(varE conLen))
                   && $(xorCmp tag off _arr)|]
            [|let $(varP (makeI 0)) = $(varE off) + $(litIntegral lenCname)
               in $(mkDeserializeExprOne 'deserialize con)|]

mkDeserializeExpr False False headTy tyOfTy =
    case tyOfTy of
        -- Unit constructor
        UnitType cname ->
            [|pure ($(varE _initialOffset) + 1, $(conE cname))|]
        -- Product type
        TheType con ->
            letE
                [valD (varP (mkName "i0")) (normalB (varE _initialOffset)) []]
                (mkDeserializeExprOne 'deserialize con)
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
        match
            (litP (IntegerL i))
            (normalB (mkDeserializeExprOne 'deserialize con)) []
    peekErr =
        match
            wildP
            (normalB
                -- XXX Print the tag
                 [|error
                       ("Found invalid tag while peeking (" ++
                        $(lift (pprint headTy)) ++ ")")|])
            []

mkDeserializeExpr False True _ (TheType con@(SimpleDataCon _ fields)) = do
    deserializeWithKeys <- newName "deserializeWithKeys"
    updateFunc <- newName "updateFunc"
    updateFuncDec <- RecHeader.conUpdateFuncDec updateFunc fields
    deserializeWithKeysDec <-
        RecHeader.mkDeserializeKeysDec deserializeWithKeys updateFunc con
    letE
        (pure <$> (deserializeWithKeysDec ++ updateFuncDec))
        (RecHeader.mkRecDeserializeExpr
             _initialOffset
             _endOffset
             deserializeWithKeys
             con)

mkDeserializeExpr _ _ _ _ = errorUnimplemented

mkDeserializeDec :: SerializeConfig -> Type -> [DataCon] -> Q [Dec]
mkDeserializeDec (SerializeConfig {..}) headTy cons = do
    peekMethod <-
        mkDeserializeExpr
            cfgConstructorTagAsString
            cfgRecordSyntaxWithHeader
            headTy
            (typeOfType headTy cons)
    pure
        ( maybe
            []
            (\x -> [PragmaD (InlineP 'deserialize x FunLike AllPhases)])
            cfgInlineDeserialize
         ++
            [ FunD
              'deserialize
              [ Clause
                    (if isUnitType cons && not cfgConstructorTagAsString
                         then [VarP _initialOffset, WildP, WildP]
                         else [VarP _initialOffset, VarP _arr, VarP _endOffset])
                    (NormalB peekMethod)
                    []
              ]
            ]
        )

--------------------------------------------------------------------------------
-- Poke
--------------------------------------------------------------------------------

mkSerializeExprTag :: Name -> Int -> Q Exp
mkSerializeExprTag tagType tagVal =
    [|serialize
          $(varE _initialOffset)
          $(varE _arr)
          $((sigE (litE (IntegerL (fromIntegral tagVal))) (conT tagType)))|]

mkSerializeExpr :: Bool -> Bool -> TypeOfType -> Q Exp
mkSerializeExpr True False tyOfTy =
    case tyOfTy of
        -- Unit type
        UnitType cname ->
            caseE
                (varE _val)
                [serializeDataCon (SimpleDataCon cname [])]
        -- Product type
        (TheType con) ->
            caseE
                (varE _val)
                [serializeDataCon con]
        -- Sum type
        (MultiType cons) ->
            caseE
                (varE _val)
                (map serializeDataCon cons)

    where

    serializeDataCon (SimpleDataCon cname fields) = do
        let tagLen8 = getNameBaseLen cname
            conEnc = tagLen8 : map c2w (nameBase cname)
        matchConstructor
            cname
            (length fields)
            (doE [ bindS
                       (varP (mkName "i0"))
                       (serializeW8List _initialOffset _arr conEnc)
                 , noBindS (mkSerializeExprFields 'serialize fields)
                 ])

mkSerializeExpr False False tyOfTy =
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
                           (mkSerializeExprFields 'serialize fields)
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
                                   , noBindS
                                         (mkSerializeExprFields
                                              'serialize
                                              fields)
                                   ]))
                     (zip [0 ..] cons))

mkSerializeExpr False True (TheType con) =
    RecHeader.mkRecSerializeExpr _initialOffset con

mkSerializeExpr _ _ _ = errorUnimplemented

mkSerializeDec :: SerializeConfig -> Type -> [DataCon] -> Q [Dec]
mkSerializeDec (SerializeConfig {..}) headTy cons = do
    pokeMethod <-
        mkSerializeExpr
            cfgConstructorTagAsString
            cfgRecordSyntaxWithHeader
            (typeOfType headTy cons)
    pure
        ( maybe
            []
            (\x -> [PragmaD (InlineP 'serialize x FunLike AllPhases)])
            cfgInlineSerialize
         ++
            [FunD
                  'serialize
                  [ Clause
                        (if isUnitType cons && not cfgConstructorTagAsString
                             then [VarP _initialOffset, WildP, WildP]
                             else [VarP _initialOffset, VarP _arr, VarP _val])
                        (NormalB pokeMethod)
                        []
                  ]
            ]
        )

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
--       serializeConfig
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
       SerializeConfig -> Type -> [DataCon] -> ([Dec] -> Q [Dec]) -> Q [Dec]
deriveSerializeInternal conf headTy cons next = do
    sizeDec <- mkSizeDec conf headTy cons
    peekDec <- mkDeserializeDec conf headTy cons
    pokeDec <- mkSerializeDec conf headTy cons
    let methods = concat [sizeDec, peekDec, pokeDec]
    next methods

-- | @deriveSerializeWith config instance-dec@ generates a template Haskell
-- splice consisting of a declaration of a 'Serialize' instance. @instance-dec@
-- is a template Haskell declaration splice consisting of a standard Haskell
-- instance declaration without the type class methods. The type class methods
-- for the given instance are generated according to the supplied @config@
-- parameter.
--
-- Usage:
--
-- @
-- \$(deriveSerializeWith
--       serializeConfig
--       [d|instance Serialize a => Serialize (Maybe a)|])
-- @
deriveSerializeWith :: SerializeConfig -> Q [Dec] -> Q [Dec]
deriveSerializeWith conf mDecs = do
    dec <- mDecs
    case dec of
        [inst@(InstanceD _ _ headTyWC [])] -> do
            let headTy = unwrap dec headTyWC
            dt <- reifyDataType (getMainTypeName dec headTy)
            let cons = dtCons dt
            deriveSerializeInternal conf headTy cons (next inst)
        _ -> errorUnsupported (errorMessage dec)
  where

    next (InstanceD mo preds headTyWC []) methods =
        pure [InstanceD mo preds headTyWC methods]
    next dec _ = errorUnsupported (errorMessage dec)

    errorMessage dec =
        error $ unlines
            [ "Error: deriveSerializeWith:"
            , ""
            , ">> " ++ pprint dec
            , ""
            , "The above is not a valid instance declaration."
            , "Any haskell instance declaration without a body is valid."
            , ""
            , "Examples:"
            , "instance Serialize (Proxy a)"
            , "instance Serialize a => Serialize (Identity a)"
            , "instance Serialize (TableT Identity)"
            ]

    unwrap _ (AppT (ConT _) r) = r
    unwrap dec _ = errorMessage dec

    getMainTypeName dec = go

        where

        go (ConT nm) = nm
        go (AppT l _) = go l
        go _ = errorMessage dec

-- | Given a 'Serialize' instance declaration splice without the methods,
-- generate a full instance declaration including all the type class methods.
--
-- >>> deriveSerialize = deriveSerializeWith serializeConfig
--
-- Usage:
--
-- @
-- \$(deriveSerialize
--       [d|instance Serialize a => Serialize (Maybe a)|])
-- @
deriveSerialize :: Q [Dec] -> Q [Dec]
deriveSerialize = deriveSerializeWith serializeConfig
