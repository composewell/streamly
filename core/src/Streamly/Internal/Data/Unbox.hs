{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- Must come after TypeFamilies, otherwise it is re-enabled.
-- MonoLocalBinds enabled by TypeFamilies causes perf regressions in general.
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{- HLINT ignore -}

-- |
-- Module      : Streamly.Internal.Data.Unbox
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Unbox
    (
    -- ** Unbox type class
      Unbox(..)

    -- ** Peek and poke utilities
    , BoundedPtr (..)
    -- Peek
    , Peeker (..)
    , read
    , readUnsafe
    , skipByte
    , runPeeker
    -- Poke
    , pokeBoundedPtrUnsafe
    , pokeBoundedPtr

    -- ** Generic Deriving
    , PeekRep(..)
    , PokeRep(..)
    , SizeOfRep(..)
    , genericSizeOf
    , genericPeekByteIndex
    , genericPokeByteIndex
    ) where

#include "MachDeps.h"
#include "ArrayMacros.h"

import Control.Monad (void, when)
import Data.Complex (Complex((:+)))
import Data.Functor ((<&>))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Foreign.Ptr (IntPtr(..), WordPtr(..))
import GHC.Base (IO(..))
import GHC.Fingerprint.Type (Fingerprint(..))
import GHC.Int (Int16(..), Int32(..), Int64(..), Int8(..))
import GHC.Real (Ratio(..))
import GHC.Stable (StablePtr(..))
import GHC.Word (Word16(..), Word32(..), Word64(..), Word8(..))
#if MIN_VERSION_base(4,15,0)
import GHC.RTS.Flags (IoSubSystem(..))
#endif
import Streamly.Internal.Data.Builder (Builder (..))

import GHC.Generics
import GHC.Exts
import GHC.TypeLits
import Prelude hiding (read)

import Streamly.Internal.Data.MutByteArray.Type (MutByteArray(..))
#ifdef DEBUG
import Streamly.Internal.Data.MutByteArray.Type (sizeOfMutableByteArray)
#endif

--------------------------------------------------------------------------------
-- The Unbox type class
--------------------------------------------------------------------------------

-- XXX generate error if the size is < 1

-- = Design notes =
--
-- == Fixed length data types ==
--
-- The main goal of the Unbox type class is to be used in arrays. Invariants
-- for the sizeOf value required for use in arrays:
--
-- * size is independent of the value, it is determined by the type only. So
-- that we can store values of the same type in fixed length array cells.
-- * recursive data types cannot be fixed length, therefore, cannot be
-- serialized using this type class.
-- * size cannot be zero. So that the length of an array storing the element
-- and the number of elements can be related.
--
-- Note, for general serializable types the size cannot be fixed e.g. we may
-- want to serialize a list. This type class can be considered a special case
-- of a more general serialization type class.
--
-- == Stream vs Array ==
--
-- In theory we could convert a type to and from a byte stream and use that
-- to implement boxing, unboxing. But composing a stream from parts of the
-- structure is much more inefficient than writing them to a memory location.
-- However, it should be possible to efficiently parse a Haskell type from an
-- array using chunk folds.
--
-- Also, this type class allows each primitive type to have its own specific
-- efficient implementation to read and write the type to the mutable byte
-- array using special GHC byte array operations. For example, see the Unbox
-- instances of Char, Int, Word types.
--
-- == MutableByteArray vs ForeignPtr ==
--
-- The Unbox typeclass uses MutableByteArray but could use ForeignPtr or
-- any other representation of memory. We could make this a multiparameter type
-- class if necessary.
--
-- If the type class would have to support reading and writing to a Ptr as well,
-- basically what Storable does. We will also need to touch the anchoring ptr at
-- the right points which is prone to errors. However, it should be simple to
-- implement unmanaged/read-only memory arrays by allowing a Ptr type in
-- ArrayContents, though it would require all instances to support reading from
-- a Ptr.
--
-- == Byte Offset vs Element Index ==
--
-- There is a reason for using byte offset instead of element index in read and
-- write operations in the type class. If we use element index, slicing of the
-- array becomes rigid. We can only slice the array at addresses that are
-- aligned with the type, therefore, we cannot slice at misaligned location and
-- then cast the slice into another type which does not necessarily align with
-- the original type.
--
-- == Alignment ==
--
-- As a side note, there seem to be no performance advantage of alignment
-- anymore, see
-- https://lemire.me/blog/2012/05/31/data-alignment-for-speed-myth-or-reality/
--
-- = Unboxed Records =
--
-- Unboxed types can be treated as unboxed records. We can provide a more
-- convenient API to access different parts from the Unboxed representation
-- without having to unbox the entire type. The type can have nested parts
-- therefore, we will need a general way (some sort of lenses) to address the
-- parts.
--
-- = Lazy Boxing =
--
-- When converting an unboxed representation to a boxed representation we can
-- use lazy construction. Each constructor of the constructed computation may
-- just hold a lazy computation to actually construct it on demand. This could
-- be useful for larger structures where we may need only small parts of it.
--
-- Same thing can be done for serialize type class as well but it will require
-- size fields at each nesting level, aggregating the size upwards.

-- | The 'Unbox' type class provides operations for serialization (unboxing)
-- and deserialization (boxing) of fixed-length, non-recursive Haskell data
-- types to and from their byte stream representation.
--
-- Unbox uses fixed size encoding, therefore, size is independent of the value,
-- it must be determined solely by the type. This restriction makes types with
-- 'Unbox' instances suitable for storing in arrays. Note that sum types may
-- have multiple constructors of different sizes, the size of a sum type is
-- computed as the maximum required by any constructor.
--
-- The 'peekAt' operation reads as many bytes from the mutable byte
-- array as the @size@ of the data type and builds a Haskell data type from
-- these bytes. 'pokeAt' operation converts a Haskell data type to its
-- binary representation which consists of @size@ bytes and then stores
-- these bytes into the mutable byte array. These operations do not check the
-- bounds of the array, the user of the type class is expected to check the
-- bounds before peeking or poking.
--
-- IMPORTANT: The serialized data's byte ordering remains the same as the host
-- machine's byte order. Therefore, it can not be deserialized from host
-- machines with a different byte ordering.
--
-- Instances can be derived via Generics, Template Haskell, or written
-- manually. Note that the data type must be non-recursive. WARNING! Generic
-- and Template Haskell deriving, both hang for recursive data types. Deriving
-- via Generics is more convenient but Template Haskell should be preferred
-- over Generics for the following reasons:
--
-- 1. Instances derived via Template Haskell provide better and more reliable
-- performance.
-- 2. Generic deriving allows only 256 fields or constructor tags whereas
-- template Haskell has no limit.
--
-- Here is an example, for deriving an instance of this type class using
-- generics:
--
-- >>> import GHC.Generics (Generic)
-- >>> :{
-- data Object = Object
--     { _int0 :: Int
--     , _int1 :: Int
--     } deriving Generic
-- :}
--
-- >>> import Streamly.Data.MutByteArray (Unbox(..))
-- >>> instance Unbox Object
--
-- To derive the instance via Template Haskell:
--
-- @
-- import Streamly.Data.MutByteArray (deriveUnbox)
-- \$(deriveUnbox [d|instance Unbox Object|])
-- @
--
-- See 'Streamly.Data.MutByteArray.deriveUnbox' for more information on deriving
-- using Template Haskell.
--
-- If you want to write the instance manually:
--
-- >>> :{
-- instance Unbox Object where
--     sizeOf _ = 16
--     peekAt i arr = do
--        -- Check the array bounds
--         x0 <- peekAt i arr
--         x1 <- peekAt (i + 8) arr
--         return $ Object x0 x1
--     pokeAt i arr (Object x0 x1) = do
--        -- Check the array bounds
--         pokeAt i arr x0
--         pokeAt (i + 8) arr x1
-- :}
--
class Unbox a where
    -- | Get the size. Size cannot be zero, should be at least 1 byte.
    sizeOf :: Proxy a -> Int

    {-# INLINE sizeOf #-}
    default sizeOf :: (SizeOfRep (Rep a)) => Proxy a -> Int
    sizeOf = genericSizeOf

    -- | @peekAt byte-offset array@ reads an element of type @a@ from the
    -- given byte offset in the array.
    --
    -- IMPORTANT: The implementation of this interface may not check the bounds
    -- of the array, the caller must not assume that.
    peekAt :: Int -> MutByteArray -> IO a

    {-# INLINE peekAt #-}
    default peekAt :: (Generic a, PeekRep (Rep a)) =>
         Int -> MutByteArray -> IO a
    peekAt i arr = genericPeekByteIndex arr i

    peekByteIndex :: Int -> MutByteArray -> IO a
    peekByteIndex = peekAt

    -- | @pokeAt byte-offset array@ writes an element of type @a@ to the
    -- given byte offset in the array.
    --
    -- IMPORTANT: The implementation of this interface may not check the bounds
    -- of the array, the caller must not assume that.
    pokeAt :: Int -> MutByteArray -> a -> IO ()

    pokeByteIndex :: Int -> MutByteArray -> a -> IO ()
    pokeByteIndex = pokeAt

    {-# INLINE pokeAt #-}
    default pokeAt :: (Generic a, PokeRep (Rep a)) =>
        Int -> MutByteArray -> a -> IO ()
    pokeAt i arr = genericPokeByteIndex arr i

{-# DEPRECATED peekByteIndex "Use peekAt." #-}
{-# DEPRECATED pokeByteIndex "Use pokeAt." #-}

-- _size is the length from array start to the last accessed byte.
{-# INLINE checkBounds #-}
checkBounds :: String -> Int -> MutByteArray -> IO ()
checkBounds _label _size _arr = do
#ifdef DEBUG
    sz <- sizeOfMutableByteArray _arr
    if (_size > sz)
    then error
        $ _label
            ++ ": accessing array at offset = "
            ++ show (_size - 1)
            ++ " max valid offset = " ++ show (sz - 1)
    else return ()
#else
    return ()
#endif

#define DERIVE_UNBOXED(_type, _constructor, _readArray, _writeArray, _sizeOf) \
instance Unbox _type where {                                                  \
; {-# INLINE peekAt #-}                                                       \
; peekAt off@(I# n) arr@(MutByteArray mbarr) =                                \
    checkBounds "peek" (off + sizeOf (Proxy :: Proxy _type)) arr              \
    >> (IO $ \s ->                                                            \
      case _readArray mbarr n s of                                            \
          { (# s1, i #) -> (# s1, _constructor i #) })                        \
; {-# INLINE pokeAt #-}                                                       \
; pokeAt off@(I# n) arr@(MutByteArray mbarr) (_constructor val) =             \
    checkBounds "poke" (off + sizeOf (Proxy :: Proxy _type)) arr              \
    >> (IO $ \s -> (# _writeArray mbarr n val s, () #))                       \
; {-# INLINE sizeOf #-}                                                       \
; sizeOf _ = _sizeOf                                                          \
}

#define DERIVE_WRAPPED_UNBOX(_constraint, _type, _constructor, _innerType)    \
instance _constraint Unbox _type where                                        \
; {-# INLINE peekAt #-}                                                       \
; peekAt i arr =                                                              \
    checkBounds "peek" (i + sizeOf (Proxy :: Proxy _type)) arr                \
    >> _constructor <$> peekAt i arr                                          \
; {-# INLINE pokeAt #-}                                                       \
; pokeAt i arr (_constructor a) =                                             \
    checkBounds "poke" (i + sizeOf (Proxy :: Proxy _type)) arr                \
    >> pokeAt i arr a                                                         \
; {-# INLINE sizeOf #-}                                                       \
; sizeOf _ = SIZE_OF(_innerType)

#define DERIVE_BINARY_UNBOX(_constraint, _type, _constructor, _innerType)     \
instance _constraint Unbox _type where {                                      \
; {-# INLINE peekAt #-}                                                       \
; peekAt i arr =                                                              \
      checkBounds "peek" (i + sizeOf (Proxy :: Proxy _type)) arr >>           \
      peekAt i arr >>=                                                        \
        (\p1 -> peekAt (i + SIZE_OF(_innerType)) arr <&> _constructor p1)     \
; {-# INLINE pokeAt #-}                                                       \
; pokeAt i arr (_constructor p1 p2) =                                         \
      checkBounds "poke" (i + sizeOf (Proxy :: Proxy _type)) arr >>           \
      pokeAt i arr p1 >>                                                      \
        pokeAt (i + SIZE_OF(_innerType)) arr p2                               \
; {-# INLINE sizeOf #-}                                                       \
; sizeOf _ = 2 * SIZE_OF(_innerType)                                          \
}

-------------------------------------------------------------------------------
-- Unbox instances for primitive types
-------------------------------------------------------------------------------

DERIVE_UNBOXED( Char
              , C#
              , readWord8ArrayAsWideChar#
              , writeWord8ArrayAsWideChar#
              , SIZEOF_HSCHAR)

DERIVE_UNBOXED( Int8
              , I8#
              , readInt8Array#
              , writeInt8Array#
              , 1)

DERIVE_UNBOXED( Int16
              , I16#
              , readWord8ArrayAsInt16#
              , writeWord8ArrayAsInt16#
              , 2)

DERIVE_UNBOXED( Int32
              , I32#
              , readWord8ArrayAsInt32#
              , writeWord8ArrayAsInt32#
              , 4)

DERIVE_UNBOXED( Int
              , I#
              , readWord8ArrayAsInt#
              , writeWord8ArrayAsInt#
              , SIZEOF_HSINT)

DERIVE_UNBOXED( Int64
              , I64#
              , readWord8ArrayAsInt64#
              , writeWord8ArrayAsInt64#
              , 8)

DERIVE_UNBOXED( Word
              , W#
              , readWord8ArrayAsWord#
              , writeWord8ArrayAsWord#
              , SIZEOF_HSWORD)

DERIVE_UNBOXED( Word8
              , W8#
              , readWord8Array#
              , writeWord8Array#
              , 1)

DERIVE_UNBOXED( Word16
              , W16#
              , readWord8ArrayAsWord16#
              , writeWord8ArrayAsWord16#
              , 2)

DERIVE_UNBOXED( Word32
              , W32#
              , readWord8ArrayAsWord32#
              , writeWord8ArrayAsWord32#
              , 4)

DERIVE_UNBOXED( Word64
              , W64#
              , readWord8ArrayAsWord64#
              , writeWord8ArrayAsWord64#
              , 8)

DERIVE_UNBOXED( Double
              , D#
              , readWord8ArrayAsDouble#
              , writeWord8ArrayAsDouble#
              , SIZEOF_HSDOUBLE)

DERIVE_UNBOXED( Float
              , F#
              , readWord8ArrayAsFloat#
              , writeWord8ArrayAsFloat#
              , SIZEOF_HSFLOAT)

-------------------------------------------------------------------------------
-- Unbox instances for derived types
-------------------------------------------------------------------------------

DERIVE_UNBOXED( (StablePtr a)
              , StablePtr
              , readWord8ArrayAsStablePtr#
              , writeWord8ArrayAsStablePtr#
              , SIZEOF_HSSTABLEPTR)

DERIVE_UNBOXED( (Ptr a)
              , Ptr
              , readWord8ArrayAsAddr#
              , writeWord8ArrayAsAddr#
              , SIZEOF_HSPTR)

DERIVE_UNBOXED( (FunPtr a)
              , FunPtr
              , readWord8ArrayAsAddr#
              , writeWord8ArrayAsAddr#
              , SIZEOF_HSFUNPTR)

DERIVE_WRAPPED_UNBOX(,IntPtr,IntPtr,Int)
DERIVE_WRAPPED_UNBOX(,WordPtr,WordPtr,Word)
DERIVE_WRAPPED_UNBOX(Unbox a =>,(Identity a),Identity,a)
#if MIN_VERSION_base(4,14,0)
DERIVE_WRAPPED_UNBOX(Unbox a =>,(Down a),Down,a)
#endif
DERIVE_WRAPPED_UNBOX(Unbox a =>,(Const a b),Const,a)
DERIVE_BINARY_UNBOX(forall a. Unbox a =>,(Complex a),(:+),a)
DERIVE_BINARY_UNBOX(forall a. Unbox a =>,(Ratio a),(:%),a)
DERIVE_BINARY_UNBOX(,Fingerprint,Fingerprint,Word64)

instance Unbox () where

    {-# INLINE peekAt #-}
    peekAt i arr =
      checkBounds "peek ()" (i + sizeOf (Proxy :: Proxy ())) arr >> return ()

    {-# INLINE pokeAt #-}
    pokeAt i arr _ =
      checkBounds "poke ()" (i + sizeOf (Proxy :: Proxy ())) arr >> return ()

    {-# INLINE sizeOf #-}
    sizeOf _ = 1

#if MIN_VERSION_base(4,15,0)
instance Unbox IoSubSystem where

    {-# INLINE peekAt #-}
    peekAt i arr =
        checkBounds
            "peek IoSubSystem" (i + sizeOf (Proxy :: Proxy IoSubSystem)) arr
        >> toEnum <$> peekAt i arr

    {-# INLINE pokeAt #-}
    pokeAt i arr a =
        checkBounds
            "poke IoSubSystem" (i + sizeOf (Proxy :: Proxy IoSubSystem)) arr
        >> pokeAt i arr (fromEnum a)

    {-# INLINE sizeOf #-}
    sizeOf _ = sizeOf (Proxy :: Proxy Int)
#endif

instance Unbox Bool where

    {-# INLINE peekAt #-}
    peekAt i arr = do
        checkBounds "peek Bool" (i + sizeOf (Proxy :: Proxy Bool)) arr
        res <- peekAt i arr
        return $ res /= (0 :: Int8)

    {-# INLINE pokeAt #-}
    pokeAt i arr a =
        checkBounds "poke Bool" (i + sizeOf (Proxy :: Proxy Bool)) arr
        >> if a
           then pokeAt i arr (1 :: Int8)
           else pokeAt i arr (0 :: Int8)

    {-# INLINE sizeOf #-}
    sizeOf _ = 1

--------------------------------------------------------------------------------
-- Generic deriving
--------------------------------------------------------------------------------

-- Utilities to build or parse a type safely and easily.

-- XXX Use Array instead.

-- | A location inside a mutable byte array with the bound of the array. Is it
-- cheaper to just get the bound using the size of the array whenever needed?
data BoundedPtr =
    BoundedPtr
        MutByteArray          -- byte array
        Int                       -- current pos
        Int                       -- position after end

--------------------------------------------------------------------------------
-- Peeker monad
--------------------------------------------------------------------------------

-- | Chains peek functions that pass the current position to the next function
newtype Peeker a = Peeker (Builder BoundedPtr IO a)
    deriving (Functor, Applicative, Monad)

{-# INLINE readUnsafe #-}
readUnsafe :: Unbox a => Peeker a
readUnsafe = Peeker (Builder step)

    where

    {-# INLINE step #-}
    step :: forall a. Unbox a => BoundedPtr -> IO (a, BoundedPtr)
    step (BoundedPtr arr pos end) = do
        let next = pos + sizeOf (Proxy :: Proxy a)
#ifdef DEBUG
        when (next > end)
            $ error $ "readUnsafe: reading beyond limit. next = "
                ++ show next
                ++ " end = " ++ show end
#endif
        r <- peekAt pos arr
        return (r, BoundedPtr arr next end)

{-# INLINE read #-}
read :: Unbox a => Peeker a
read = Peeker (Builder step)

    where

    {-# INLINE step #-}
    step :: forall a. Unbox a => BoundedPtr -> IO (a, BoundedPtr)
    step (BoundedPtr arr pos end) = do
        let next = pos + sizeOf (Proxy :: Proxy a)
        when (next > end)
            $ error $ "read: reading beyond limit. next = "
                ++ show next
                ++ " end = " ++ show end
        r <- peekAt pos arr
        return (r, BoundedPtr arr next end)

{-# INLINE skipByte #-}
skipByte :: Peeker ()
skipByte = Peeker (Builder step)

    where

    {-# INLINE step #-}
    step :: BoundedPtr -> IO ((), BoundedPtr)
    step (BoundedPtr arr pos end) = do
        let next = pos + 1
#ifdef DEBUG
        when (next > end)
            $ error $ "skipByte: reading beyond limit. next = "
                ++ show next
                ++ " end = " ++ show end
#endif
        return ((), BoundedPtr arr next end)

{-# INLINE runPeeker #-}
runPeeker :: Peeker a -> BoundedPtr -> IO a
runPeeker (Peeker (Builder f)) ptr = fmap fst (f ptr)

--------------------------------------------------------------------------------
-- Poke utilities
--------------------------------------------------------------------------------

-- XXX Use MutArray instead of BoundedPtr.

-- XXX Using a Poker monad may be useful when we have to compute the size to be
-- poked as we go and then poke the size at a previous location. For variable
-- sized object serialization we may also want to reallocate the array and
-- return the newly allocated array in the output.

-- Does not check writing beyond bound.
{-# INLINE pokeBoundedPtrUnsafe #-}
pokeBoundedPtrUnsafe :: forall a. Unbox a => a -> BoundedPtr -> IO BoundedPtr
pokeBoundedPtrUnsafe a (BoundedPtr arr pos end) = do
    let next = pos + sizeOf (Proxy :: Proxy a)
#ifdef DEBUG
    when (next > end)
        $ error $ "pokeBoundedPtrUnsafe: reading beyond limit. next = "
            ++ show next
            ++ " end = " ++ show end
#endif
    pokeAt pos arr a
    return (BoundedPtr arr next end)

{-# INLINE pokeBoundedPtr #-}
pokeBoundedPtr :: forall a. Unbox a => a -> BoundedPtr -> IO BoundedPtr
pokeBoundedPtr a (BoundedPtr arr pos end) = do
    let next = pos + sizeOf (Proxy :: Proxy a)
    when (next > end) $ error "pokeBoundedPtr writing beyond limit"
    pokeAt pos arr a
    return (BoundedPtr arr next end)

--------------------------------------------------------------------------------
-- Check the number of constructors in a sum type
--------------------------------------------------------------------------------

-- Count the constructors of a sum type.
type family SumArity (a :: Type -> Type) :: Nat where
    SumArity (C1 _ _) = 1
    -- Requires UndecidableInstances
    SumArity (f :+: g) = SumArity f + SumArity g

type family TypeErrorMessage (a :: Symbol) :: Constraint where
    TypeErrorMessage a = TypeError ('Text a)

type family ArityCheck (b :: Bool) :: Constraint where
    ArityCheck 'True = ()
    ArityCheck 'False = TypeErrorMessage
        "Generic Unbox deriving does not support > 256 constructors."

-- Type constraint to restrict the sum type arity so that the constructor tag
-- can fit in a single byte.
-- Note that Arity starts from 1 and constructor tags start from 0. So if max
-- arity is 256 then max constructor tag would be 255.
-- XXX Use variable length encoding to support more than 256 constructors.
type MaxArity256 n = ArityCheck (n <=? 256)

--------------------------------------------------------------------------------
-- Generic Deriving of Unbox instance
--------------------------------------------------------------------------------

-- Unbox uses fixed size encoding, therefore, when a (sum) type has multiple
-- constructors, the size of the type is computed as the maximum required by
-- any constructor. Therefore, size is independent of the value, it can be
-- determined solely by the type.

-- | Implementation of sizeOf that works on the generic representation of an
-- ADT.
class SizeOfRep (f :: Type -> Type) where
    sizeOfRep :: f x -> Int

-- Meta information wrapper, go inside
instance SizeOfRep f => SizeOfRep (M1 i c f) where
    {-# INLINE sizeOfRep #-}
    sizeOfRep _ = sizeOfRep (undefined :: f x)

-- Primitive type "a".
instance Unbox a => SizeOfRep (K1 i a) where
    {-# INLINE sizeOfRep #-}
    sizeOfRep _ = sizeOf (Proxy :: Proxy a)

-- Void: data type without constructors. Values of this type cannot exist,
-- therefore the size is undefined. We should never be serializing structures
-- with elements of this type.
instance SizeOfRep V1 where
    {-# INLINE sizeOfRep #-}
    sizeOfRep = error "sizeOfRep: a value of a Void type must not exist"

-- Note that when a sum type has many unit constructors only a single byte is
-- required to encode the type as only the constructor tag is stored.
instance SizeOfRep U1 where
    {-# INLINE sizeOfRep #-}
    sizeOfRep _ = 0

-- Product type
instance (SizeOfRep f, SizeOfRep g) => SizeOfRep (f :*: g) where
    {-# INLINE sizeOfRep #-}
    sizeOfRep _ = sizeOfRep (undefined :: f x) + sizeOfRep (undefined :: g x)

-------------------------------------------------------------------------------

class SizeOfRepSum (f :: Type -> Type) where
    sizeOfRepSum :: f x -> Int

-- Constructor
instance SizeOfRep a => SizeOfRepSum (C1 c a) where
    {-# INLINE sizeOfRepSum #-}
    sizeOfRepSum = sizeOfRep

instance (SizeOfRepSum f, SizeOfRepSum g) => SizeOfRepSum (f :+: g) where
    {-# INLINE sizeOfRepSum #-}
    sizeOfRepSum _ =
        max (sizeOfRepSum (undefined :: f x)) (sizeOfRepSum (undefined :: g x))

-------------------------------------------------------------------------------

instance (MaxArity256 (SumArity (f :+: g)), SizeOfRepSum f, SizeOfRepSum g) =>
    SizeOfRep (f :+: g) where

    -- The size of a sum type is the max of any of the constructor size.
    -- sizeOfRepSum type class operation is used here instead of sizeOfRep so
    -- that we account the constructor index byte only for the first time and
    -- avoid including it for the subsequent sum constructors.
    {-# INLINE sizeOfRep #-}
    sizeOfRep _ =
        -- One byte for the constructor id and then the constructor value.
        sizeOf (Proxy :: Proxy Word8) +
            max (sizeOfRepSum (undefined :: f x))
                (sizeOfRepSum (undefined :: g x))

-- Unit: constructors without arguments.
-- Theoretically the size can be 0, but we use 1 to simplify the implementation
-- of an array of unit type elements. With a non-zero size we can count the number
-- of elements in the array based on the size of the array. Otherwise we will
-- have to store a virtual length in the array, but keep the physical size of
-- the array as 0. Or we will have to make a special handling for zero sized
-- elements to make the size as 1. Or we can disallow arrays with elements
-- having size 0.
--
-- Some examples:
--
-- data B = B -- one byte
-- data A = A B -- one byte
-- data X = X1 | X2 -- one byte (constructor tag only)
--
{-# INLINE genericSizeOf #-}
genericSizeOf :: forall a. (SizeOfRep (Rep a)) => Proxy a -> Int
genericSizeOf _ =
    let s = sizeOfRep (undefined :: Rep a x)
      in if s == 0 then 1 else s

--------------------------------------------------------------------------------
-- Generic poke
--------------------------------------------------------------------------------

class PokeRep (f :: Type -> Type) where
    pokeRep :: f a -> BoundedPtr -> IO BoundedPtr

instance PokeRep f => PokeRep (M1 i c f) where
    {-# INLINE pokeRep #-}
    pokeRep f = pokeRep (unM1 f)

instance Unbox a => PokeRep (K1 i a) where
    {-# INLINE pokeRep #-}
    pokeRep a = pokeBoundedPtrUnsafe (unK1 a)

instance PokeRep V1 where
    {-# INLINE pokeRep #-}
    pokeRep = error "pokeRep: a value of a Void type should not exist"

instance PokeRep U1 where
    {-# INLINE pokeRep #-}
    pokeRep _ x = pure x

instance (PokeRep f, PokeRep g) => PokeRep (f :*: g) where
    {-# INLINE pokeRep #-}
    pokeRep (f :*: g) ptr = pokeRep f ptr >>= pokeRep g

-------------------------------------------------------------------------------

class KnownNat n => PokeRepSum (n :: Nat) (f :: Type -> Type) where
    -- "n" is the constructor tag to be poked.
    pokeRepSum :: Proxy n -> f a -> BoundedPtr -> IO BoundedPtr

instance (KnownNat n, PokeRep a) => PokeRepSum n (C1 c a) where
    {-# INLINE pokeRepSum #-}
    pokeRepSum _ x ptr = do
        let tag = fromInteger (natVal (Proxy :: Proxy n)) :: Word8
        pokeBoundedPtrUnsafe tag ptr >>= pokeRep x

instance (PokeRepSum n f, PokeRepSum (n + SumArity f) g)
         => PokeRepSum n (f :+: g) where
    {-# INLINE pokeRepSum #-}
    pokeRepSum _ (L1 x) ptr =
        pokeRepSum (Proxy :: Proxy n) x ptr
    pokeRepSum _ (R1 x) ptr =
        pokeRepSum (Proxy :: Proxy (n + SumArity f)) x ptr

-------------------------------------------------------------------------------

instance (MaxArity256 (SumArity (f :+: g)), PokeRepSum 0 (f :+: g)) =>
    PokeRep (f :+: g) where

    {-# INLINE pokeRep #-}
    pokeRep = pokeRepSum (Proxy :: Proxy 0)

{-# INLINE genericPokeObject #-}
genericPokeObject :: (Generic a, PokeRep (Rep a)) =>
    a -> BoundedPtr -> IO BoundedPtr
genericPokeObject a = pokeRep (from a)

genericPokeObj :: (Generic a, PokeRep (Rep a)) => a -> BoundedPtr -> IO ()
genericPokeObj a ptr = void $ genericPokeObject a ptr

{-# INLINE genericPokeByteIndex #-}
genericPokeByteIndex :: (Generic a, PokeRep (Rep a)) =>
    MutByteArray -> Int -> a -> IO ()
genericPokeByteIndex arr index x = do
    -- XXX Should we use unsafe poke?
#ifdef DEBUG
    end <- sizeOfMutableByteArray arr
    genericPokeObj x (BoundedPtr arr index end)
#else
    genericPokeObj x (BoundedPtr arr index undefined)
#endif

--------------------------------------------------------------------------------
-- Generic peek
--------------------------------------------------------------------------------

class PeekRep (f :: Type -> Type) where
    -- Like pokeRep, we can use the following signature instead of using Peeker
    -- peekRep :: BoundedPtr -> IO (BoundedPtr, f a)
    peekRep :: Peeker (f x)

instance PeekRep f => PeekRep (M1 i c f) where
    {-# INLINE peekRep #-}
    peekRep = fmap M1 peekRep

instance Unbox a => PeekRep (K1 i a) where
    {-# INLINE peekRep #-}
    peekRep = fmap K1 readUnsafe

instance PeekRep V1 where
    {-# INLINE peekRep #-}
    peekRep = error "peekRep: a value of a Void type should not exist"

instance PeekRep U1 where
    {-# INLINE peekRep #-}
    peekRep = pure U1

instance (PeekRep f, PeekRep g) => PeekRep (f :*: g) where
    {-# INLINE peekRep #-}
    peekRep = (:*:) <$> peekRep <*> peekRep

-------------------------------------------------------------------------------

class KnownNat n => PeekRepSum (n :: Nat) (f :: Type -> Type) where
    -- "n" is the constructor tag to be matched.
    peekRepSum :: Proxy n -> Word8 -> Peeker (f a)

instance (KnownNat n, PeekRep a) => PeekRepSum n (C1 c a) where
    {-# INLINE peekRepSum #-}
    peekRepSum _ _ = peekRep
    {-
    -- These error checks are expensive, to avoid these
    -- we validate the max value of the tag in peekRep.
    -- XXX Add tests to cover all cases
    peekRepSum _ tag
        | tag == curTag = peekRep
        | tag > curTag =
            error $ "Unbox instance peek: Constructor tag index "
                ++ show tag ++ " out of range, max tag index is "
                ++ show curTag
        | otherwise = error "peekRepSum: bug"

        where

        curTag = fromInteger (natVal (Proxy :: Proxy n))
    -}

instance (PeekRepSum n f, PeekRepSum (n + SumArity f) g)
         => PeekRepSum n (f :+: g) where
    {-# INLINE peekRepSum #-}
    peekRepSum curProxy tag
        | tag < firstRightTag =
            L1 <$> peekRepSum curProxy tag
        | otherwise =
            R1 <$> peekRepSum (Proxy :: Proxy (n + SumArity f)) tag

        where

        firstRightTag = fromInteger (natVal (Proxy :: Proxy (n + SumArity f)))

-------------------------------------------------------------------------------

instance ( MaxArity256 (SumArity (f :+: g))
         , KnownNat (SumArity (f :+: g))
         , PeekRepSum 0 (f :+: g))
         => PeekRep (f :+: g) where
    {-# INLINE peekRep #-}
    peekRep = do
        tag :: Word8 <- readUnsafe
        -- XXX test with 256 and more constructors
        let arity :: Int =
                fromInteger (natVal (Proxy :: Proxy (SumArity (f :+: g))))
        when (fromIntegral tag >= arity)
            $ error $ "peek: Tag " ++ show tag
                ++ " is greater than the max tag " ++ show (arity - 1)
                ++ " for the data type"
        peekRepSum (Proxy :: Proxy 0) tag -- DataKinds

{-# INLINE genericPeeker #-}
genericPeeker :: (Generic a, PeekRep (Rep a)) => Peeker a
genericPeeker = to <$> peekRep

{-# INLINE genericPeekBoundedPtr #-}
genericPeekBoundedPtr :: (Generic a, PeekRep (Rep a)) => BoundedPtr -> IO a
genericPeekBoundedPtr = runPeeker genericPeeker

{-# INLINE genericPeekByteIndex #-}
genericPeekByteIndex :: (Generic a, PeekRep (Rep a)) =>
    MutByteArray -> Int -> IO a
genericPeekByteIndex arr index = do
    -- XXX Should we use unsafe peek?
#ifdef DEBUG
    end <- sizeOfMutableByteArray arr
    genericPeekBoundedPtr (BoundedPtr arr index end)
#else
    genericPeekBoundedPtr (BoundedPtr arr index undefined)
#endif
