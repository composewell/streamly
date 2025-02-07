{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Streamly.Internal.Data.Serialize.TH.Bottom
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Serialize.TH.Bottom
    (
    -- ** Config
      SerializeConfig(..)
    , serializeConfig
    , inlineAddSizeTo
    , inlineSerializeAt
    , inlineDeserializeAt
    , encodeConstrNames
    , encodeRecordFields

    -- ** Other Utilities
    , TypeOfType(..)
    , typeOfType
    , SimpleDataCon(..)
    , simplifyDataCon
    , Field
    , mkFieldName
    , isUnitType
    , isRecordSyntax
    , c2w
    , wListToString
    , xorCmp
    , serializeW8List
    , litIntegral
    , litProxy
    , matchConstructor
    , openConstructor
    , makeI
    , makeN
    , makeA
    , int_w8
    , int_w32
    , w32_int
    , w8_int
    , _acc
    , _arr
    , _endOffset
    , _initialOffset
    , _x
    , _tag
    , _val
    , errorUnsupported
    , errorUnimplemented
    ) where

import Data.Maybe (isJust)
import Data.Char (chr, ord)
import Data.List (foldl')
import Data.Foldable (length)
import Data.Word (Word16, Word32, Word64, Word8)
import Data.Bits (Bits, (.|.), shiftL, zeroBits, xor)
import Streamly.Internal.System.IO (unsafeInlineIO)
import Streamly.Internal.Data.Unbox (Unbox)
import Data.Proxy (Proxy)

import Language.Haskell.TH
import Streamly.Internal.Data.Serialize.Type

import qualified Streamly.Internal.Data.Unbox as Unbox

import Streamly.Internal.Data.Unbox.TH (DataCon(..))
import Prelude hiding (Foldable(..))

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

-- NOTE: 'Nothing' is not eqvivalant to 'Just Inlinable'. Ie. Having no inline
-- specific pragma and having an Inlinable pragma are different. Having an
-- Inlinable pragma makes GHC put the code in the interface file whereas having
-- no inline specific pragma let's GHC decide whether to put the code in
-- interface file or not.

-- | Configuration to control how the 'Serialize' instance is generated. The
-- configuration is opaque and is modified by composing config modifier
-- functions, for example:
--
-- >>> (inlineSerializeAt (Just NoInline)) . (inlineSerializeAt (Just Inlinable))
--
-- The default configuration settings are:
--
-- * 'inlineAddSizeTo' Nothing
-- * 'inlineSerializeAt' (Just Inline)
-- * 'inlineDeserializeAt' (Just Inline)
--
-- The following experimental options are also available:
--
-- * 'encodeConstrNames' False
-- * 'encodeRecordFields' False
--
data SerializeConfig =
    SerializeConfig
        { cfgInlineSize :: Maybe Inline
        , cfgInlineSerialize :: Maybe Inline
        , cfgInlineDeserialize :: Maybe Inline
        , cfgConstructorTagAsString :: Bool
        , cfgRecordSyntaxWithHeader :: Bool
        }

-- | How should we inline the 'addSizeTo' function? The default is 'Nothing'
-- which means left to the compiler. Forcing inline on @addSizeTo@ function
-- actually worsens some benchmarks and improves none.
inlineAddSizeTo :: Maybe Inline -> SerializeConfig -> SerializeConfig
inlineAddSizeTo v cfg = cfg {cfgInlineSize = v}

-- XXX Should we make the default Inlinable instead?

-- | How should we inline the 'serialize' function? The default 'Just Inline'.
-- However, aggressive inlining can bloat the code and increase in compilation
-- times when there are big functions and too many nesting levels so you can
-- change it accordingly. A 'Nothing' value leaves the decision to the
-- compiler.
inlineSerializeAt :: Maybe Inline -> SerializeConfig -> SerializeConfig
inlineSerializeAt v cfg = cfg {cfgInlineSerialize = v}

-- XXX Should we make the default Inlinable instead?

-- | How should we inline the 'deserialize' function? See guidelines in
-- 'inlineSerializeAt'.
inlineDeserializeAt :: Maybe Inline -> SerializeConfig -> SerializeConfig
inlineDeserializeAt v cfg = cfg {cfgInlineDeserialize = v}

-- | __Experimental__
--
-- In sum types, use Latin-1 encoded original constructor names rather than
-- binary values to identify constructors. This option is not applicable to
-- product types.
--
-- This option enables the following behavior:
--
-- * __Reordering__: Order of the fields can be changed without affecting
-- serialization.
-- * __Addition__: If a field is added in the new version, the old version of
-- the data type can still be deserialized by the new version. The new value
-- would never occur in the old one.
-- * __Deletion__: If a field is deleted in the new version, deserialization
-- of the old version will result in an error. TBD: We can possibly designate a
-- catch-all case to handle this scenario.
--
-- Note that if you change a type, change the semantics of a type, or delete a
-- field and add a new field with the same name, deserialization of old data
-- may result in silent unexpected behavior.
--
-- This option has to be the same on both encoding and decoding side.
--
-- The default is 'False'.
--
encodeConstrNames :: Bool -> SerializeConfig -> SerializeConfig
encodeConstrNames v cfg = cfg {cfgConstructorTagAsString = v}

-- XXX We can deserialize each field to Either, so if there is a
-- deserialization error in any field it can handled independently. Also, a
-- unique type/version identifier of the field (based on the versions of the
-- packages, full module name space + type identifier) can be serialized along
-- with the value for stricter compatibility, semantics checking. Or we can
-- store a type hash.

-- | __Experimental__
--
-- In explicit record types, use Latin-1 encoded record field names rather than
-- binary values to identify the record fields. Note that this option is not
-- applicable to sum types. Also, it does not work on a product type which is
-- not a record, because there are no field names to begin with.
--
-- This option enables the following behavior:
--
-- * __Reordering__: Order of the fields can be changed without affecting
-- serialization.
-- * __Addition__: If a 'Maybe' type field is added in the new version, the old
-- version of the data type can still be deserialized by the new version, the
-- field value in the older version is assumed to be 'Nothing'. If any other
-- type of field is added, deserialization of the older version results in an
-- error but only when that field is actually accessed in the deserialized
-- record.
-- * __Deletion__: If a field is deleted in the new version and it is
-- encountered in a previously serialized version then the field is discarded.
--
-- This option has to be the same on both encoding and decoding side.
--
-- There is a constant performance overhead proportional to the total length of
-- the record field names and the number of record fields.
--
-- The default is 'False'.
--
encodeRecordFields :: Bool -> SerializeConfig -> SerializeConfig
encodeRecordFields v cfg = cfg {cfgRecordSyntaxWithHeader = v}

serializeConfig :: SerializeConfig
serializeConfig =
    SerializeConfig
        { cfgInlineSize = Nothing
        , cfgInlineSerialize = Just Inline
        , cfgInlineDeserialize = Just Inline
        , cfgConstructorTagAsString = False
        , cfgRecordSyntaxWithHeader = False
        }

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

makeN :: Int -> Name
makeN i = mkName $ "n" ++ show i

makeA :: Int -> Name
makeA i = mkName $ "a" ++ show i

--------------------------------------------------------------------------------
-- Domain specific helpers
--------------------------------------------------------------------------------

openConstructor :: Name -> Int -> Q Pat
openConstructor cname numFields =
    conP cname (map varP (map mkFieldName [0 .. (numFields - 1)]))

matchConstructor :: Name -> Int -> Q Exp -> Q Match
matchConstructor cname numFields exp0 =
    match (openConstructor cname numFields) (normalB exp0) []

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

isRecordSyntax :: SimpleDataCon -> Bool
isRecordSyntax (SimpleDataCon _ fields) = and (isJust . fst <$> fields)

--------------------------------------------------------------------------------
-- Type casting
--------------------------------------------------------------------------------

int_w8 :: Int -> Word8
int_w8 = fromIntegral

int_w32 :: Int -> Word32
int_w32 = fromIntegral

w8_w16 :: Word8 -> Word16
w8_w16 = fromIntegral

w8_w32 :: Word8 -> Word32
w8_w32 = fromIntegral

w8_w64 :: Word8 -> Word64
w8_w64 = fromIntegral

w8_int :: Word8 -> Int
w8_int = fromIntegral

w32_int :: Word32 -> Int
w32_int = fromIntegral

c2w :: Char -> Word8
c2w = fromIntegral . ord

wListToString :: [Word8] -> String
wListToString = fmap (chr . fromIntegral)

--------------------------------------------------------------------------------
-- Bit manipulation
--------------------------------------------------------------------------------

shiftAdd :: Bits a => (b -> a) -> [b] -> a
shiftAdd conv xs =
    foldl' (.|.) zeroBits $
    map (\(j, x) -> shiftL x (j * 8)) $ zip [0 ..] $ map conv xs

-- Note: This only works in little endian machines
-- TODO:
-- Instead of generating this via TH can't we write it directly in Haskell and
-- use that? Creating one comparison function for each deserialization may be
-- too much code and may not be necessary.
-- Benchmark both the implementations and check.
xorCmp :: [Word8] -> Name -> Name -> Q Exp
xorCmp tag off arr =
    case tagLen of
        x | x < 2 -> [|$(go8 0) == zeroBits|]
        x | x < 4 -> [|$(go16 0) == zeroBits|]
        x | x < 8 -> [|$(go32 0) == zeroBits|]
        _ -> [|$(go64 0) == zeroBits|]
  where
    tagLen = length tag
    go8 i | i >= tagLen = [|zeroBits|]
    go8 i = do
        let wIntegral = litIntegral i
        [|xor (unsafeInlineIO
                   (Unbox.peekAt
                        ($(varE off) + $(litIntegral i))
                        $(varE arr)))
              ($(wIntegral) :: Word8) .|.
          $(go8 (i + 1))|]
    go16 i
        | i >= tagLen = [|zeroBits|]
    go16 i
        | tagLen - i < 2 = go16 (tagLen - 2)
    go16 i = do
        let wIntegral =
                litIntegral
                    (shiftAdd w8_w16 [tag !! i, tag !! (i + 1)] :: Word16)
        [|xor (unsafeInlineIO
                   (Unbox.peekAt
                        ($(varE off) + $(litIntegral i))
                        $(varE arr)))
              ($(wIntegral) :: Word16) .|.
          $(go16 (i + 2))|]
    go32 i
        | i >= tagLen = [|zeroBits|]
    go32 i
        | tagLen - i < 4 = go32 (tagLen - 4)
    go32 i = do
        let wIntegral =
                litIntegral
                    (shiftAdd
                         w8_w32
                         [ tag !! i
                         , tag !! (i + 1)
                         , tag !! (i + 2)
                         , tag !! (i + 3)
                         ] :: Word32)
        [|xor (unsafeInlineIO
                   (Unbox.peekAt
                        ($(varE off) + $(litIntegral i))
                        $(varE arr)))
              ($(wIntegral) :: Word32) .|.
          $(go32 (i + 4))|]
    go64 i
        | i >= tagLen = [|zeroBits|]
    go64 i
        | tagLen - i < 8 = go64 (tagLen - 8)
    go64 i = do
        let wIntegral =
                litIntegral
                    (shiftAdd
                         w8_w64
                         [ tag !! i
                         , tag !! (i + 1)
                         , tag !! (i + 2)
                         , tag !! (i + 3)
                         , tag !! (i + 4)
                         , tag !! (i + 5)
                         , tag !! (i + 6)
                         , tag !! (i + 7)
                         ])
        [|xor (unsafeInlineIO
                   (Unbox.peekAt
                        ($(varE off) + $(litIntegral i))
                        $(varE arr)))
              ($(wIntegral) :: Word64) .|.
          $(go64 (i + 8))|]

--------------------------------------------------------------------------------
-- Primitive serialization
--------------------------------------------------------------------------------

-- TODO:
-- Will this be too much of a code bloat?
-- Loop with the loop body unrolled?
-- Serialize this in batches similar to batch comparision in xorCmp?
serializeW8List :: Name -> Name -> [Word8] -> Q Exp
serializeW8List off arr w8List = do
    [|let $(varP (makeN 0)) = $(varE off)
       in $(doE (fmap makeBind [0 .. (lenW8List - 1)] ++
                 [noBindS ([|pure $(varE (makeN lenW8List))|])]))|]

    where

    lenW8List = length w8List
    makeBind i =
        bindS
            (varP (makeN (i + 1)))
            [|$(varE 'serializeAt)
                  $(varE (makeN i))
                  $(varE arr)
                  ($(litIntegral (w8List !! i)) :: Word8)|]

--------------------------------------------------------------------------------
-- TH Helpers
--------------------------------------------------------------------------------

litIntegral :: Integral a => a -> Q Exp
litIntegral = litE . IntegerL . fromIntegral

litProxy :: Unbox a => Proxy a -> Q Exp
litProxy = litE . IntegerL . fromIntegral . Unbox.sizeOf

--------------------------------------------------------------------------------
-- Error codes
--------------------------------------------------------------------------------

errorUnsupported :: String -> a
errorUnsupported err =
    error
        $ unlines
              [ "Unsupported"
              , "==========="
              , "This is improper use of the library."
              , "This case is unsupported."
              , "Please contact the developer if this case is of interest."
              , ""
              , "Message"
              , "-------"
              , err
              ]

errorUnimplemented :: a
errorUnimplemented =
    error
        $ unlines
              [ "Unimplemented"
              , "============="
              , "Please contact the developer if this case is of interest."
              ]
