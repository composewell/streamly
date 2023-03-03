{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | TODO: Implement TH based instance derivation for better performance.

module Streamly.Internal.Data.Unboxed
    ( Unbox(..)
    , peekWith
    , pokeWith
    , MutableByteArray(..)
    , touch
    , getMutableByteArray#
    , pin
    , unpin
    , newUnpinnedBytes
    , newPinnedBytes
    , newAlignedPinnedBytes
    , nil

    -- * Type Parser and Builder
    , BoundedPtr (..)

    , Peeker (..)
    , read
    , readUnsafe
    , skipByte
    , runPeeker

    , pokeBoundedPtrUnsafe
    , pokeBoundedPtr

    -- * Generic Unbox instances
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
import System.IO.Unsafe (unsafePerformIO)

import GHC.Generics
import GHC.Exts
import GHC.TypeLits
import Prelude hiding (read)

--------------------------------------------------------------------------------
-- The ArrayContents type
--------------------------------------------------------------------------------

-- XXX can use UnliftedNewtypes
data MutableByteArray = MutableByteArray (MutableByteArray# RealWorld)

{-# INLINE getMutableByteArray# #-}
getMutableByteArray# :: MutableByteArray -> MutableByteArray# RealWorld
getMutableByteArray# (MutableByteArray mbarr) = mbarr

{-# INLINE touch #-}
touch :: MutableByteArray -> IO ()
touch (MutableByteArray contents) =
    IO $ \s -> case touch# contents s of s' -> (# s', () #)

-- | Return the size of the array in bytes.
{-# INLINE sizeOfMutableByteArray #-}
sizeOfMutableByteArray :: MutableByteArray -> IO Int
sizeOfMutableByteArray (MutableByteArray arr) =
    IO $ \s ->
        case getSizeofMutableByteArray# arr s of
            (# s1, i #) -> (# s1, I# i #)

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

{-# NOINLINE nil #-}
nil :: MutableByteArray
nil = unsafePerformIO $ newUnpinnedBytes 0

{-# INLINE newUnpinnedBytes #-}
newUnpinnedBytes :: Int -> IO MutableByteArray
newUnpinnedBytes nbytes | nbytes < 0 =
  errorWithoutStackTrace "newUnpinnedBytes: size must be >= 0"
newUnpinnedBytes (I# nbytes) = IO $ \s ->
    case newByteArray# nbytes s of
        (# s', mbarr# #) ->
           let c = MutableByteArray mbarr#
            in (# s', c #)

{-# INLINE newPinnedBytes #-}
newPinnedBytes :: Int -> IO MutableByteArray
newPinnedBytes nbytes | nbytes < 0 =
  errorWithoutStackTrace "newPinnedBytes: size must be >= 0"
newPinnedBytes (I# nbytes) = IO $ \s ->
    case newPinnedByteArray# nbytes s of
        (# s', mbarr# #) ->
           let c = MutableByteArray mbarr#
            in (# s', c #)

{-# INLINE newAlignedPinnedBytes #-}
newAlignedPinnedBytes :: Int -> Int -> IO MutableByteArray
newAlignedPinnedBytes nbytes _align | nbytes < 0 =
  errorWithoutStackTrace "newAlignedPinnedBytes: size must be >= 0"
newAlignedPinnedBytes (I# nbytes) (I# align) = IO $ \s ->
    case newAlignedPinnedByteArray# nbytes align s of
        (# s', mbarr# #) ->
           let c = MutableByteArray mbarr#
            in (# s', c #)

-------------------------------------------------------------------------------
-- Pinning & Unpinning
-------------------------------------------------------------------------------

{-# INLINE isPinned #-}
isPinned :: MutableByteArray -> Bool
isPinned (MutableByteArray arr#) =
    let pinnedInt = I# (isMutableByteArrayPinned# arr#)
     in pinnedInt == 1


{-# INLINE cloneMutableArrayWith# #-}
cloneMutableArrayWith#
    :: (Int# -> State# RealWorld -> (# State# RealWorld
                                     , MutableByteArray# RealWorld #))
    -> MutableByteArray# RealWorld
    -> State# RealWorld
    -> (# State# RealWorld, MutableByteArray# RealWorld #)
cloneMutableArrayWith# alloc# arr# s# =
    case getSizeofMutableByteArray# arr# s# of
        (# s1#, i# #) ->
            case alloc# i# s1# of
                (# s2#, arr1# #) ->
                    case copyMutableByteArray# arr# 0# arr1# 0# i# s2# of
                        s3# -> (# s3#, arr1# #)

{-# INLINE pin #-}
pin :: MutableByteArray -> IO MutableByteArray
pin arr@(MutableByteArray marr#) =
    if isPinned arr
    then return arr
    else IO
             $ \s# ->
                   case cloneMutableArrayWith# newPinnedByteArray# marr# s# of
                       (# s1#, marr1# #) -> (# s1#, MutableByteArray marr1# #)

{-# INLINE unpin #-}
unpin :: MutableByteArray -> IO MutableByteArray
unpin arr@(MutableByteArray marr#) =
    if not (isPinned arr)
    then return arr
    else IO
             $ \s# ->
                   case cloneMutableArrayWith# newByteArray# marr# s# of
                       (# s1#, marr1# #) -> (# s1#, MutableByteArray marr1# #)

--------------------------------------------------------------------------------
-- The Unbox type class
--------------------------------------------------------------------------------

-- XXX generate error if the size is < 1

-- In theory we could convert a type to and from a byte stream and use that
-- to implement boxing, unboxing. But that would be inefficient. This type
-- class allows each primitive type to have its own specific efficient
-- implementation to read and write the type to memory.
--
-- This is a typeclass that uses MutableByteArray but could use ForeignPtr or
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
-- There is a reason for using byte offset instead of element index in read and
-- write operations in the type class. If we use element index slicing of the
-- array becomes rigid. We can only slice the array at addresses that are
-- aligned with the type, therefore, we cannot slice at misaligned location and
-- then cast the slice into another type which does not ncessarily align with
-- the original type.
--
-- As a side note, there seem to be no performance advantage of alignment
-- anymore, see
-- https://lemire.me/blog/2012/05/31/data-alignment-for-speed-myth-or-reality/
--

-- The main goal of the Unbox type class is to be used in arrays. Invariants
-- for the sizeOf value required for use in arrays:
--
-- * size is independent of the value, it is determined by the type only. So
-- that we can store values of the same type in fixed length array cells.
-- * size cannot be zero. So that the length of an array storing the element
-- and the number of elements can be related.
--
-- Note, for general serializable types the size cannot be fixed e.g. we may
-- want to serialize a list. This type class can be considered a special case
-- of a more general serialization type class.

-- | A type implementing the 'Unbox' interface supplies operations for reading
-- and writing the type from and to a mutable byte array (an unboxed
-- representation of the type) in memory. The read operation 'peekByteIndex'
-- deserializes the boxed type from the mutable byte array. The write operation
-- 'pokeByteIndex' serializes the boxed type to the mutable byte array.
--
-- Instances can be derived via 'Generic'. Note that the data type must be
-- non-recursive. Here is an example, for deriving an instance of this type
-- class.
--
-- >>> import GHC.Generics (Generic)
-- >>> :{
-- data Object = Object
--     { _int0 :: Int
--     , _int1 :: Int
--     } deriving Generic
-- :}
--
-- WARNING! Generic deriving hangs for recursive data types.
--
-- >>> import Streamly.Data.Array (Unbox(..))
-- >>> instance Unbox Object
--
-- If you want to write the instance manually:
--
-- >>> :{
-- instance Unbox Object where
--     sizeOf _ = 16
--     peekByteIndex i arr = do
--         x0 <- peekByteIndex i arr
--         x1 <- peekByteIndex (i + 8) arr
--         return $ Object x0 x1
--     pokeByteIndex i arr (Object x0 x1) = do
--         pokeByteIndex i arr x0
--         pokeByteIndex (i + 8) arr x1
-- :}
--
class Unbox a where
    -- | Get the size. Size cannot be zero.
    sizeOf :: Proxy a -> Int

    default sizeOf :: (SizeOfRep (Rep a)) => Proxy a -> Int
    sizeOf = genericSizeOf

    -- | Read an element of type "a" from a MutableByteArray given the byte
    -- index.
    --
    -- IMPORTANT: The implementation of this interface may not check the bounds
    -- of the array, the caller must not assume that.
    peekByteIndex :: Int -> MutableByteArray -> IO a

    default peekByteIndex :: (Generic a, PeekRep (Rep a)) =>
         Int -> MutableByteArray -> IO a
    peekByteIndex i arr = genericPeekByteIndex arr i

    -- | Write an element of type "a" to a MutableByteArray given the byte
    -- index.
    --
    -- IMPORTANT: The implementation of this interface may not check the bounds
    -- of the array, the caller must not assume that.
    pokeByteIndex :: Int -> MutableByteArray -> a -> IO ()

    default pokeByteIndex :: (Generic a, PokeRep (Rep a)) =>
        Int -> MutableByteArray -> a -> IO ()
    pokeByteIndex i arr = genericPokeByteIndex arr i

#define DERIVE_UNBOXED(_type, _constructor, _readArray, _writeArray, _sizeOf) \
instance Unbox _type where {                                         \
; {-# INLINE peekByteIndex #-}                                       \
; peekByteIndex (I# n) (MutableByteArray mbarr) = IO $ \s ->         \
      case _readArray mbarr n s of                                   \
          { (# s1, i #) -> (# s1, _constructor i #) }                \
; {-# INLINE pokeByteIndex #-}                                       \
; pokeByteIndex (I# n) (MutableByteArray mbarr) (_constructor val) = \
        IO $ \s -> (# _writeArray mbarr n val s, () #)               \
; {-# INLINE sizeOf #-}                                              \
; sizeOf _ = _sizeOf                                                 \
}

#define DERIVE_WRAPPED_UNBOX(_constraint, _type, _constructor, _innerType)    \
instance _constraint Unbox _type where                                        \
; {-# INLINE peekByteIndex #-}                                                \
; peekByteIndex i arr = _constructor <$> peekByteIndex i arr                  \
; {-# INLINE pokeByteIndex #-}                                                \
; pokeByteIndex i arr (_constructor a) = pokeByteIndex i arr a                \
; {-# INLINE sizeOf #-}                                                       \
; sizeOf _ = SIZE_OF(_innerType)

#define DERIVE_BINARY_UNBOX(_constraint, _type, _constructor, _innerType) \
instance _constraint Unbox _type where {                                  \
; {-# INLINE peekByteIndex #-}                                            \
; peekByteIndex i arr =                                                   \
      peekByteIndex i arr >>=                                             \
        (\p1 -> peekByteIndex (i + SIZE_OF(_innerType)) arr               \
            <&> _constructor p1)                                          \
; {-# INLINE pokeByteIndex #-}                                            \
; pokeByteIndex i arr (_constructor p1 p2) =                              \
      pokeByteIndex i arr p1 >>                                           \
        pokeByteIndex (i + SIZE_OF(_innerType)) arr p2                    \
; {-# INLINE sizeOf #-}                                                   \
; sizeOf _ = 2 * SIZE_OF(_innerType)                                      \
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

    {-# INLINE peekByteIndex #-}
    peekByteIndex _ _ = return ()

    {-# INLINE pokeByteIndex #-}
    pokeByteIndex _ _ _ = return ()

    {-# INLINE sizeOf #-}
    sizeOf _ = 1

#if MIN_VERSION_base(4,15,0)
instance Unbox IoSubSystem where

    {-# INLINE peekByteIndex #-}
    peekByteIndex i arr = toEnum <$> peekByteIndex i arr

    {-# INLINE pokeByteIndex #-}
    pokeByteIndex i arr a = pokeByteIndex i arr (fromEnum a)

    {-# INLINE sizeOf #-}
    sizeOf _ = sizeOf (Proxy :: Proxy Int)
#endif

instance Unbox Bool where

    {-# INLINE peekByteIndex #-}
    peekByteIndex i arr = do
        res <- peekByteIndex i arr
        return $ res /= (0 :: Int8)

    {-# INLINE pokeByteIndex #-}
    pokeByteIndex i arr a =
        if a
        then pokeByteIndex i arr (1 :: Int8)
        else pokeByteIndex i arr (0 :: Int8)

    {-# INLINE sizeOf #-}
    sizeOf _ = 1

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

{-# INLINE peekWith #-}
peekWith :: Unbox a => MutableByteArray -> Int -> IO a
peekWith arr i = peekByteIndex i arr

{-# INLINE pokeWith #-}
pokeWith :: Unbox a => MutableByteArray -> Int -> a -> IO ()
pokeWith arr i = pokeByteIndex i arr

--------------------------------------------------------------------------------
-- Generic deriving
--------------------------------------------------------------------------------

-- Utilities to build or parse a type safely and easily.

-- | A location inside a mutable byte array with the bound of the array. Is it
-- cheaper to just get the bound using the size of the array whenever needed?
data BoundedPtr =
    BoundedPtr
        MutableByteArray          -- byte array
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
    step :: forall a. Unbox a => BoundedPtr -> IO (BoundedPtr, a)
    step (BoundedPtr arr pos end) = do
        let next = pos + sizeOf (Proxy :: Proxy a)
        r <- peekByteIndex pos arr
        return (BoundedPtr arr next end, r)

{-# INLINE read #-}
read :: Unbox a => Peeker a
read = Peeker (Builder step)

    where

    {-# INLINE step #-}
    step :: forall a. Unbox a => BoundedPtr -> IO (BoundedPtr, a)
    step (BoundedPtr arr pos end) = do
        let next = pos + sizeOf (Proxy :: Proxy a)
        when (next > end) $ error "peekObject reading beyond limit"
        r <- peekByteIndex pos arr
        return (BoundedPtr arr next end, r)

{-# INLINE skipByte #-}
skipByte :: Peeker ()
skipByte = Peeker (Builder step)

    where

    {-# INLINE step #-}
    step :: BoundedPtr -> IO (BoundedPtr, ())
    step (BoundedPtr arr pos end) = do
        let next = pos + 1
        when (next > end)
            $ error $ "skipByte: reading beyond limit. next = "
                ++ show next
                ++ " end = " ++ show end
        return (BoundedPtr arr next end, ())

{-# INLINE runPeeker #-}
runPeeker :: Peeker a -> BoundedPtr -> IO a
runPeeker (Peeker (Builder f)) ptr = fmap snd (f ptr)

--------------------------------------------------------------------------------
-- Poke utilities
--------------------------------------------------------------------------------

-- XXX Using a Poker monad may be useful when we have to compute the size to be
-- poked as we go and then poke the size at a previous location. For variable
-- sized object serialization we may also want to reallocate the array and
-- return the newly allocated array in the output.

-- Does not check writing beyond bound.
{-# INLINE pokeBoundedPtrUnsafe #-}
pokeBoundedPtrUnsafe :: forall a. Unbox a => a -> BoundedPtr -> IO BoundedPtr
pokeBoundedPtrUnsafe a (BoundedPtr arr pos end) = do
    let next = pos + sizeOf (Proxy :: Proxy a)
    pokeByteIndex pos arr a
    return (BoundedPtr arr next end)

{-# INLINE pokeBoundedPtr #-}
pokeBoundedPtr :: forall a. Unbox a => a -> BoundedPtr -> IO BoundedPtr
pokeBoundedPtr a (BoundedPtr arr pos end) = do
    let next = pos + sizeOf (Proxy :: Proxy a)
    when (next > end) $ error "pokeBoundedPtr writing beyond limit"
    pokeByteIndex pos arr a
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
type MaxArity256 n = ArityCheck (n <=? 255)

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
    -- that we add the constructor index byte only for the first time and avoid
    -- including it for the subsequent sum constructors.
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
    pokeRep a = pokeBoundedPtr (unK1 a)

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
        pokeBoundedPtr (fromInteger (natVal (Proxy :: Proxy n)) :: Word8) ptr
            >>= pokeRep x

instance (KnownNat n, PokeRepSum n f, PokeRepSum (n + SumArity f) g)
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
    MutableByteArray -> Int -> a -> IO ()
genericPokeByteIndex arr index x = do
    -- XXX Should we use unsafe poke?
    end <- sizeOfMutableByteArray arr
    genericPokeObj x (BoundedPtr arr index end)

--------------------------------------------------------------------------------
-- Generic peek
--------------------------------------------------------------------------------

class PeekRep (f :: Type -> Type) where
    peekRep :: Peeker (f x)

instance PeekRep f => PeekRep (M1 i c f) where
    {-# INLINE peekRep #-}
    peekRep = fmap M1 peekRep

instance Unbox a => PeekRep (K1 i a) where
    {-# INLINE peekRep #-}
    peekRep = fmap K1 read

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
    peekRepSum _ tag
        | tag == curTag = peekRep
        | tag > curTag =
            error $ "Unbox instance peek: Constructor tag index "
                ++ show tag ++ " out of range, max tag index is "
                ++ show curTag
        | otherwise = error "peekRepSum: bug"

        where

        curTag = fromInteger (natVal (Proxy :: Proxy n))

instance (KnownNat n, PeekRepSum n f, PeekRepSum (n + SumArity f) g)
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

instance (MaxArity256 (SumArity (f :+: g)), PeekRepSum 0 (f :+: g))
         => PeekRep (f :+: g) where
    {-# INLINE peekRep #-}
    peekRep = do
        tag <- read
        peekRepSum (Proxy :: Proxy 0) tag

{-# INLINE genericPeeker #-}
genericPeeker :: (Generic a, PeekRep (Rep a)) => Peeker a
genericPeeker = to <$> peekRep

{-# INLINE genericPeekBoundedPtr #-}
genericPeekBoundedPtr :: (Generic a, PeekRep (Rep a)) => BoundedPtr -> IO a
genericPeekBoundedPtr = runPeeker genericPeeker

{-# INLINE genericPeekByteIndex #-}
genericPeekByteIndex :: (Generic a, PeekRep (Rep a)) =>
    MutableByteArray -> Int -> IO a
genericPeekByteIndex arr index = do
    -- XXX Should we use unsafe peek?
    end <- sizeOfMutableByteArray arr
    genericPeekBoundedPtr (BoundedPtr arr index end)
