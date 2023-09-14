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
    ( Config(..)
    , defaultConfig
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
import Data.Word (Word16, Word32, Word64, Word8)
import Data.Bits (Bits, (.|.), shiftL, zeroBits, xor)
import Streamly.Internal.System.IO (unsafeInlineIO)
import Streamly.Internal.Data.Unbox (Unbox)
import Data.Proxy (Proxy)

import Language.Haskell.TH
import Streamly.Internal.Data.Serialize.Type

import qualified Streamly.Internal.Data.Unbox as Unbox

import Streamly.Internal.Data.Unbox.TH (DataCon(..))

--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

-- | Config to control how the 'Serialize' instance is generated.
data Config =
    Config
        { inlineSize :: Inline
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
          -- serialize the data. Multiple constructors are not supported with
          -- @recordSyntaxWithHeader@ enabled.
          --
          -- __Performance Notes:__
          --
          -- There is a constant regression proportional to
          -- @sum (map length keyList) + length keyList@ where @keyList@ is the
          -- list of keys of that record as strings.
          --
          -- As an example, @keyList@ of,
          -- @
          -- data RecordType = RecordType { field0 :: Int, field2 :: Char }
          -- @
          -- is @["field0", "field1"]@
        }

defaultConfig :: Config
defaultConfig =
    Config
        { inlineSize = Inline
        , inlineSerialize = Inline
        , inlineDeserialize = Inline
        , constructorTagAsString = False
        , recordSyntaxWithHeader = False
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
                   (Unbox.peekByteIndex
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
                   (Unbox.peekByteIndex
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
                   (Unbox.peekByteIndex
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
                   (Unbox.peekByteIndex
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
            [|$(varE 'serialize)
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

errorUnsupported :: a
errorUnsupported =
    error
        $ unlines
              [ "Unsupported."
              , "There is improper use of the library."
              , "This case is unsupported."
              , "Please contact the developer if this case is of interest."
              ]

errorUnimplemented :: a
errorUnimplemented =
    error
        $ unlines
              [ "Unimplemented."
              , "Please contact the developer if this case is of interest."
              ]
