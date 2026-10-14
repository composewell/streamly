-- |
-- Module      : Streamly.Data.MutByteArray
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- This module implements a low level byte Array type 'MutByteArray', along
-- with type classes 'Unbox' and 'Serialize' for fast binary serialization and
-- deserialization of Haskell values. Serialization, deserialization
-- performance is similar to, and in some cases many times better than the
-- store package. Conceptually, the 'Serialize' type class works in the same
-- way as store.
--
-- == Fast serialization with schema
--
-- Serialize instances are configurable to use constructor names (see
-- 'Streamly.Internal.Data.MutByteArray.encodeConstrNames'), record field names (see
-- 'Streamly.Internal.Data.MutByteArray.encodeRecordFields') instead of binary
-- encoded values. This is an experimental feature which allows JSON like
-- properties with faster speed. For example, you can change the order of
-- constructors or record fields without affecting serialized value.
--
-- == Serialization with Array and MutArray
--
-- Higher level unboxed array modules "Streamly.Data.Array" and
-- "Streamly.Data.MutArray" are built on top of this module. Unboxed arrays are
-- essentially serialized Haskell values. Array modules provide higher level
-- serialization routines like 'Streamly.Internal.Data.Array.pinnedSerialize'
-- and 'Streamly.Internal.Data.Array.deserialize' in the
-- "Streamly.Internal.Data.Array" module.
--
-- == Mutable Byte Array
--
-- 'MutByteArray' is a primitive mutable array in the IO monad. 'Unbox' and
-- 'Serialize' type classes use this primitive array to serialize data to and
-- deserialize it from. This array is used to build higher level unboxed
-- array types 'Streamly.Data.MutArray.MutArray' and 'Streamly.Data.Array.Array'.
--
-- == Using with FFI
--
-- For using an array with "safe" FFI functions or OS interfaces the array must
-- be pinned using 'pin' and then the array pointer can be accessed using
-- 'unsafeAsPtr'.
--
-- For using with "unsafe" FFI functions, the array can remain unpinned. The
-- safe way to do that is to directly pass the underlying 'MutableByteArray#
-- RealWorld' (using 'getMutByteArray#') to the FFI function wherever a pointer
-- to the array is required, it translates to the memory address of the payload
-- of the array.
--
-- For more details, see the FFI section in the GHC user guide. Here is a
-- relevant excerpt from the GHC manual:
--
-- GHC, since version 8.4, guarantees that garbage collection will never occur
-- during an unsafe call, even in the bytecode interpreter, and further
-- guarantees that unsafe calls will be performed in the calling thread. Making
-- it safe to pass heap-allocated objects to unsafe functions.

-- == Serialization using Unbox
--
-- The 'Unbox' type class is simple and used to serialize non-recursive fixed
-- size data types. This type class is primarily used to implement unboxed
-- arrays. Unboxed arrays are just a sequence of serialized fixed length
-- Haskell data types. Instances of this type class can be derived using
-- 'Generic' or template haskell based deriving functions provided in this
-- module.
--
-- Writing a data type to an array using the array creation routines in
-- "Streamly.Data.Array" or "Streamly.Data.MutArray" (e.g. @writeN@ or
-- @fromListN@), serializes the type to the array. Similarly, reading the data
-- type from the array deserializes it. You can also serialize and deserialize
-- directly to and from a 'MutByteArray', using the type class methods.
--
-- == Serialization using Serialize
--
-- The 'Serialize' type class is a superset of the 'Unbox' type class, it can
-- serialize variable length data types as well e.g. Haskell lists. Use
-- 'deriveSerialize' to derive the instances of the type class automatically
-- and then use the type class methods to serialize and deserialize to and from
-- a 'MutByteArray'.
--
-- See 'Streamly.Internal.Data.Array.pinnedSerialize' and
-- 'Streamly.Internal.Data.Array.deserialize' for 'Array' type based
-- serialization.
--
-- == Comparing serialized values
--
-- In some cases, 'Unbox' does not write every byte of a value's @sizeOf@
-- bytes. The bytes that are not written retain old values held in the memory.
-- Such cases are listed below:
--
-- * the single byte of @()@ and of a type with one constructor and no fields,
-- * the bytes after the fields of a sum type constructor that is smaller than
--   the largest constructor.
--
-- Therefore, the same value may be stored as different bytes, and comparing
-- the bytes of values stored using 'Unbox' is not reliable.
--
-- 'Serialize' writes every byte, a value is always serialized to the same
-- bytes, and comparing serialized bytes is reliable. A type with one
-- constructor and no fields, including @()@ and 'Data.Proxy.Proxy', is
-- serialized as a single 0 byte. The exception is 'Streamly.Data.Array.Array':
-- its serialization contains the bytes of its elements as stored by 'Unbox',
-- so the serialization of an @Array a@ is the same for the same value only if
-- 'Unbox' writes every byte of @a@.
--
module Streamly.Data.MutByteArray
    (

    -- * Mutable Byte Array
    -- | The standard way to read from or write to a 'MutByteArray' is by using
    -- the 'Unbox' or 'Serialize' type class methods.
      MutByteArray
    , isPinned
    , pin
    , unpin
    , new
    , pinnedNew

    -- * Unbox
    , Unbox(..)
    , deriveUnbox

    -- * Serialize
    , Serialize(..)

    -- ** Instance Config
    , SerializeConfig
    , inlineAddSizeTo
    , inlineSerializeAt
    , inlineDeserializeAt

    -- ** Instance Deriving
    , deriveSerialize
    , deriveSerializeWith
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Internal.Data.MutByteArray
