{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Internal.Data.Serialize.TH.RecHeader
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Serialize.TH.RecHeader
    ( mkSerializeExpr
    , mkDeserializeExpr
    , mkSizeOfExpr
    , conUpdateFuncExpr
    , mkDeserializeKeysExpr
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Control.Monad (void)
import Data.List (foldl')
import Data.Word (Word16, Word32, Word8)
import Data.Maybe (fromJust)
import Language.Haskell.TH
import Streamly.Internal.Data.Serialize.Type (Serialize(..))
import Data.Foldable (foldlM)
import Streamly.Internal.Data.Unbox (MutableByteArray)
import Data.Proxy (Proxy(..))

import qualified Streamly.Internal.Data.Unbox as Unbox

import Streamly.Internal.Data.Serialize.TH.Bottom
import Streamly.Internal.Data.Serialize.TH.Common

--------------------------------------------------------------------------------
-- Notes
--------------------------------------------------------------------------------

-- Encoding the header length is required to check if we want to compare the
-- header. We should not compare the headers if the lengths of the headers are
-- not equal as this might lead to a runtime error.

--------------------------------------------------------------------------------
-- Compact lists
--------------------------------------------------------------------------------

-- Like haskell list but the maximum length of the list is 255
newtype CompactList a =
    CompactList
        { unCompactList :: [a]
        }

-- We use 'Word8' to encode the length, hence the maximim number of elements in
-- the list is 255.
instance forall a. Serialize a => Serialize (CompactList a) where

    -- {-# INLINE size #-}
    size acc (CompactList xs) =
        foldl' size (acc + (Unbox.sizeOf (Proxy :: Proxy Word8))) xs

    -- Inlining this causes large compilation times for tests
    {-# INLINABLE deserialize #-}
    deserialize off arr sz = do
        (off1, len8) <- deserialize off arr sz :: IO (Int, Word8)
        let len = w8_int len8
            peekList f o i | i >= 3 = do
              -- Unfold the loop three times
              (o1, x1) <- deserialize o arr sz
              (o2, x2) <- deserialize o1 arr sz
              (o3, x3) <- deserialize o2 arr sz
              peekList (f . (\xs -> x1:x2:x3:xs)) o3 (i - 3)
            peekList f o 0 = pure (o, f [])
            peekList f o i = do
              (o1, x) <- deserialize o arr sz
              peekList (f . (x:)) o1 (i - 1)
        (nextOff, lst) <- peekList id off1 len
        pure (nextOff, CompactList lst)

    -- Inlining this causes large compilation times for tests
    {-# INLINABLE serialize #-}
    serialize off arr (CompactList val) = do
        void $ serialize off arr (int_w8 (length val) :: Word8)
        let off1 = off + Unbox.sizeOf (Proxy :: Proxy Word8)
        let pokeList o [] = pure o
            pokeList o (x:xs) = do
              o1 <- serialize o arr x
              pokeList o1 xs
        pokeList off1 val

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

fieldToNameBase :: Field -> String
fieldToNameBase = nameBase . fromJust . fst

isMaybeType :: Type -> Bool
isMaybeType (AppT (ConT m) _) = m == ''Maybe
isMaybeType _ = False

--------------------------------------------------------------------------------
-- Size
--------------------------------------------------------------------------------

-- We add 4 here because we use 'serializeWithSize' for serializing.
exprGetSize :: Q Exp -> (Int, Type) -> Q Exp
exprGetSize acc (i, _) =
    [|size $(acc) $(varE (mkFieldName i)) + 4|]

sizeOfHeader :: SimpleDataCon -> Int
sizeOfHeader (SimpleDataCon _ fields) =
    sizeForFinalOff + sizeForHeaderLength + sizeForNumFields
        + sum (map ((+ sizeForFieldLen) . length . fieldToNameBase) fields)

    where

    sizeForFinalOff = 4
    sizeForHeaderLength = 2 -- Max header length is (255 * (255 + 1) + 1) and
                            -- hence 2 bytes is enough to store it.
    sizeForNumFields = 1 -- At max 255 fields in the record constructor
    sizeForFieldLen = 1  -- At max 255 letters in the key

mkSizeOfExpr :: SimpleDataCon -> Q Exp
mkSizeOfExpr con = do
    n_acc <- newName "acc"
    n_x <- newName "x"
    (lamE
         [varP n_acc, varP n_x]
         [|$(litIntegral hlen) +
            $(caseE (varE n_x) [matchCons (varE n_acc) con])|])

    where

    hlen = sizeOfHeader con
    sizeOfFields acc fields = foldl' exprGetSize acc $ zip [0 ..] fields
    matchCons acc (SimpleDataCon cname fields) =
        let expr = sizeOfFields acc (map snd fields)
         in matchConstructor cname (length fields) expr

--------------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------------

headerValue :: SimpleDataCon -> [Word8]
headerValue (SimpleDataCon _ fields) =
    int_w8 numFields : concat (fmap lengthPrependedFieldEncoding fields)

    where

    -- Error out if the number of fields or the length of key is >= 256. We use
    -- Word8 for encoding the info and hence the max value is 255.
    numFields =
        let lenFields = length fields
         in if lenFields <= 255
            then lenFields
            else errorUnsupported
    lengthPrependedFieldEncoding field =
        let fEnc =
                let fEnc_ = map c2w (fieldToNameBase field)
                    lenFEnc = length fEnc_
                 in if lenFEnc <= 255
                    then fEnc_
                    else errorUnsupported
         in (int_w8 (length fEnc)) : fEnc

--------------------------------------------------------------------------------
-- Peek
--------------------------------------------------------------------------------

-- Encoding the size is required if we want to skip the field.
-- We encode the size as 'Word32' hence there is a 4 bytes increase in size.
{-# INLINE serializeWithSize #-}
serializeWithSize :: Serialize a => Int -> MutableByteArray -> a -> IO Int
serializeWithSize off arr val = do
    off1 <- serialize (off + 4) arr val
    Unbox.pokeByteIndex off arr (int_w32 (off1 - off - 4) :: Word32)
    pure off1

mkSerializeExpr :: Name -> SimpleDataCon -> Q Exp
mkSerializeExpr initialOffset (con@(SimpleDataCon cname fields)) = do
    afterHLen <- newName "afterHLen"
    [|do $(varP afterHLen) <-
             serialize
                 ($(varE initialOffset) + 4)
                 $(varE _arr)
                 ($(litIntegral hlen) :: Word16)
         $(varP (makeI 0)) <- $(serializeW8List afterHLen _arr hval)
         let $(openConstructor cname (length fields)) = $(varE _val)
         finalOff <- $(mkSerializeExprFields 'serializeWithSize fields)
         Unbox.pokeByteIndex
             $(varE initialOffset)
             $(varE _arr)
             ((fromIntegral :: Int -> Word32)
                  (finalOff - $(varE initialOffset)))
         pure finalOff|]

    where

    hval = headerValue con
    hlen = length hval

--------------------------------------------------------------------------------
-- Poke
--------------------------------------------------------------------------------

{-# INLINE deserializeWithSize #-}
deserializeWithSize ::
       Serialize a => Int -> MutableByteArray -> Int -> IO (Int, a)
deserializeWithSize off arr endOff = deserialize (off + 4) arr endOff

conUpdateFuncExpr :: Name -> Type -> [Field] -> Q [Dec]
conUpdateFuncExpr funcName headTy fields = do
    prevRec <- newName "prevRec"
    curOff <- newName "curOff"
    endOff <- newName "endOff"
    arr <- newName "arr"
    key <- newName "key"
    method <-
        (caseE
             (varE key)
             (concat
                  [ map (matchField arr endOff (prevRec, curOff)) fnames
                  , [ match
                          wildP
                          (normalB
                               [|do (valOff, valLen :: Word32) <-
                                        deserialize
                                            $(varE curOff)
                                            $(varE arr)
                                            $(varE endOff)
                                    pure
                                        ( $(varE prevRec)
                                        , valOff + w32_int valLen)|])
                          []
                    ]
                  ]))
    pure
        [ PragmaD (InlineP funcName NoInline FunLike AllPhases)
        , FunD
              funcName
              [ Clause
                    [ VarP arr
                    , VarP endOff
                    , TupP [VarP prevRec, VarP curOff]
                    , VarP key
                    ]
                    (NormalB method)
                    []
              ]
        ]

    where

    fnames = fmap (fromJust . fst) fields
    matchField :: Name -> Name -> (Name, Name) -> Name -> Q Match
    matchField arr endOff (prevRec, currOff) fname = do
        val <- newName "val"
        newOff <- newName "newOff"
        match
            (litP (StringL (nameBase fname)))
            (normalB
                 [|do ($(varP newOff), $(varP val)) <-
                          deserializeWithSize
                              $(varE currOff)
                              $(varE arr)
                              $(varE endOff)
                      pure
                          ( $(recUpdE (varE prevRec) [pure (fname, VarE val)])
                                :: $(pure headTy)
                          , $(varE newOff))|])
            []

mkDeserializeKeysExpr ::
       Name -> Name -> Name -> Name -> Name -> SimpleDataCon -> Q Exp
mkDeserializeKeysExpr
    hOff finalOff arr endOff updateFunc (SimpleDataCon cname fields) = do

    [|do (dataOff, hlist :: CompactList (CompactList Word8)) <-
             deserialize $(varE hOff) $(varE arr) $(varE endOff)
         let keys = wListToString . unCompactList <$> unCompactList hlist
         (finalRec, _) <-
             foldlM
                 ($(varE updateFunc) $(varE arr) $(varE endOff))
                 ($(emptyConExpr), dataOff)
                 keys
         pure ($(varE finalOff), finalRec)|]

    where

    emptyTy k ty =
        if isMaybeType ty
            then [|Nothing|]
            else [|error $(litE (StringL (nameBase k ++ " is not found.")))|]
    fieldToEmptyRecC (Just name, ty) = (name, ) <$> emptyTy name ty
    fieldToEmptyRecC _ = errorUnsupported
    emptyConExpr = recConE cname (fmap fieldToEmptyRecC fields)

mkDeserializeExpr :: Name -> Name -> Name -> SimpleDataCon -> Q Exp
mkDeserializeExpr initialOff endOff deserializeWithKeys con = do
    hOff <- newName "hOff"
    let  sizeForFinalOff = 4     -- Word32
         sizeForHeaderLength = 2 -- Word16
         sizePreData = sizeForFinalOff + sizeForHeaderLength + hlen
    [|do (hlenOff, encLen :: Word32) <-
             deserialize $(varE initialOff) $(varE _arr) $(varE endOff)
         ($(varP hOff), hlen1 :: Word16) <-
             deserialize hlenOff $(varE _arr) $(varE endOff)
         if hlen1 == $(litIntegral hlen)
             then if $(xorCmp hval hOff _arr)
                      then do
                          let $(varP (makeI 0)) =
                                  $(varE initialOff) +
                                  $(litIntegral sizePreData)
                          $(mkDeserializeExprOne 'deserializeWithSize con)
                      else $(varE deserializeWithKeys)
                               $(varE hOff)
                               ($(varE initialOff) + w32_int encLen)
             else $(varE deserializeWithKeys)
                      $(varE hOff)
                      ($(varE initialOff) + w32_int encLen)|]

    where

    hval = headerValue con
    hlen = length hval
