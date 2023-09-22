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
    , conUpdateFuncDec
    , mkDeserializeKeysDec
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

-- Compatibility Algorithm
-- =======================
--
-- The algorithm is written without any low level implementation details. See
-- the code for any low level implementation details.
--
-- Serialization:
-- --------------
--
-- To serialize the data,
--
-- * Get the list of keys for the record as @keyList@.
-- * Serialize the @keyList@.
-- * Serialize the @fields@ one-by-one after serializing the @keyList@.
--
-- Deserialization:
-- ----------------
--
-- To deserialize the data to type @T@,
--
-- __Checking for type match__:
--
-- * Get the list of keys for type @T@ as @targetKeyList@.
-- * Get the list of keys encoded as @encodedKeyList@.
-- * If @targetKeyList == encodedKeyList@ see the __Type Match__ section else
--   see the __No Type Match__ section.
--
-- __Type Match__:
--
-- * Decode the fields one-by-one and construct the type @T@ in the end.
--
-- __No Type Match__:
--
-- * Decode the list of keys encoded into @encodedKeyList@.
-- * Get the list of keys for type @T@ as @targetKeyList@.
-- * Loop through @encodedKeyList@ and start deserializing the encoded data.
-- * If the key is present in @encodedKeyList@ and not in @targetKeyList@
--   then skip parsing the corresponding value.
-- * If the key is present in @targetKeyList@ and not in @encodedKeyList@
--   then set the value for that key as @Nothing@.
-- * If the key is present in both @encodedKeyList@ and in @targetKeyList@
--   parse the value.
-- * Construct @T@ after parsing all the data.

-- Developer Notes
-- ===============
--
-- * Record update syntax is not robust across language extensions and common
--   record plugins (like record-dot-processor, large-records, etc.).

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
                     "Number of fields in the record should be <= 255."
    lengthPrependedFieldEncoding field =
        let fEnc =
                let fEnc_ = map c2w (fieldToNameBase field)
                    lenFEnc = length fEnc_
                 in if lenFEnc <= 255
                    then fEnc_
                    else
                        errorUnsupported
                            "Length of any key should be <= 255."
         in (int_w8 (length fEnc)) : fEnc

--------------------------------------------------------------------------------
-- Peek
--------------------------------------------------------------------------------

-- Encoding the size is required if we want to skip the field without knowing
-- its type. We encode the size as 'Word32' hence there is a 4 bytes increase
-- in size.
{-# INLINE serializeWithSize #-}
serializeWithSize :: Serialize a => Int -> MutableByteArray -> a -> IO Int
serializeWithSize off arr val = do
    off1 <- serialize (off + 4) arr val
    Unbox.pokeByteIndex off arr (int_w32 (off1 - off - 4) :: Word32)
    pure off1

mkSerializeExpr :: Name -> SimpleDataCon -> Q Exp
mkSerializeExpr initialOffset (con@(SimpleDataCon cname fields)) = do
    afterHLen <- newName "afterHLen"
    -- Encoding the header length is required.
    -- We first compare the header length encoded and the current header
    -- length. Only if the header lengths match, we compare the headers.
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

conUpdateFuncDec :: Name -> [Field] -> Q [Dec]
conUpdateFuncDec funcName fields = do
    prevAcc <- newName "prevAcc"
    curOff <- newName "curOff"
    endOff <- newName "endOff"
    arr <- newName "arr"
    key <- newName "key"
    method <-
        (caseE
             (varE key)
             (concat
                  [ map (matchField arr endOff (prevAcc, curOff)) fnames
                  , [ match
                          wildP
                          (normalB
                               [|do (valOff, valLen :: Word32) <-
                                        deserialize
                                            $(varE curOff)
                                            $(varE arr)
                                            $(varE endOff)
                                    pure
                                        ( $(varE prevAcc)
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
                    , TupP [VarP prevAcc, VarP curOff]
                    , VarP key
                    ]
                    (NormalB method)
                    []
              ]
        ]

    where

    fnames = fmap (fromJust . fst) fields
    matchField :: Name -> Name -> (Name, Name) -> Name -> Q Match
    matchField arr endOff (acc, currOff) fname = do
        let fnameLit = StringL (nameBase fname)
        match
            (litP fnameLit)
            (normalB
                 [|do (valOff, valLen :: Word32) <-
                        deserialize
                            $(varE currOff)
                            $(varE arr)
                            $(varE endOff)
                      pure
                          ( ($(litE fnameLit), $(varE currOff)) : $(varE acc)
                          , valOff + w32_int valLen)|])
            []

mkDeserializeKeysDec :: Name -> Name -> SimpleDataCon -> Q [Dec]
mkDeserializeKeysDec funcName updateFunc (SimpleDataCon cname fields) = do
    hOff <- newName "hOff"
    finalOff <- newName "finalOff"
    arr <- newName "arr"
    endOff <- newName "endOff"
    kvEncoded <- newName "kvEncoded"
    finalRec <- newName "finalRec"
    let deserializeFieldExpr (Just name, ty) = do
            let nameLit = litE (StringL (nameBase name))
            [|case lookup $(nameLit) $(varE kvEncoded) of
                  Nothing -> $(emptyTy name ty)
                  Just off -> do
                      val <- deserializeWithSize off $(varE arr) $(varE endOff)
                      pure $ snd val|]
        deserializeFieldExpr _ =
            errorUnsupported "The datatype should use record syntax."
    method <-
        [|do (dataOff, hlist :: CompactList (CompactList Word8)) <-
                 deserialize $(varE hOff) $(varE arr) $(varE endOff)
             let keys = wListToString . unCompactList <$> unCompactList hlist
             ($(varP kvEncoded), _) <-
                 foldlM
                     ($(varE updateFunc) $(varE arr) $(varE endOff))
                     ([], dataOff)
                     keys
             $(varP finalRec) <-
                 $(foldl
                       (\acc i ->
                            [|$(acc) <*>
                              $(deserializeFieldExpr i)|])
                       [|pure $(conE cname)|]
                       fields)
             pure ($(varE finalOff), $(varE finalRec))|]
    pure
        [ PragmaD (InlineP funcName NoInline FunLike AllPhases)
        , FunD
              funcName
              [ Clause
                    [ VarP hOff
                    , VarP finalOff
                    , VarP arr
                    , VarP endOff
                    ]
                    (NormalB method)
                    []
              ]
        ]

    where

    emptyTy k ty =
        if isMaybeType ty
            then [|pure Nothing|]
            else [|error $(litE (StringL (nameBase k ++ " is not found.")))|]


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
         if (hlen1 == $(litIntegral hlen)) && $(xorCmp hval hOff _arr)
         then do
             let $(varP (makeI 0)) =
                     $(varE initialOff) +
                     $(litIntegral sizePreData)
             $(mkDeserializeExprOne 'deserializeWithSize con)
         else $(varE deserializeWithKeys)
                  $(varE hOff)
                  ($(varE initialOff) + w32_int encLen)
                  $(varE _arr)
                  $(varE endOff)|]

    where

    hval = headerValue con
    hlen = length hval
