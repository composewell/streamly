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
--
-- == Choosing between Unbox and Serialize
--
-- 'Unbox' stores Haskell values in memory in unboxed form, for example as the
-- elements of the unboxed arrays in "Streamly.Data.Array" and
-- "Streamly.Data.MutArray". It is designed for speed, for data written and
-- read by the same program on the same machine:
--
-- * Only non-recursive types of a fixed size are supported, the size is
--   determined by the type, not by the value.
-- * 'peekAt' and 'pokeAt' do not check the bounds of the array, the caller
--   checks them.
-- * 'Int' and 'Word' use the native machine word size, e.g. 4 bytes on the
--   JavaScript backend and 8 bytes on 64-bit platforms.
-- * Bytes that carry no information are not written, see the section
--   Comparing serialized values below.
--
-- 'Serialize' converts Haskell values to bytes for storing in files or
-- sending to another program or machine. It may be slower than 'Unbox':
--
-- * Recursive and variable length types are supported, e.g. lists. The
--   length of a variable length value is stored with the value.
-- * 'deserializeAt' checks the bounds of the input buffer and fails on
--   invalid lengths.
-- * 'Int' and 'Word' are serialized as 8 bytes on all platforms.
-- * Every byte is written, the same value is always serialized to the same
--   bytes, except for 'Streamly.Data.Array.Array', see the section Comparing
--   serialized values below.
-- * Constructors and record fields can optionally be encoded by name, so
--   that constructors or record fields can be reordered without changing the
--   meaning of serialized data. This is experimental, see the section Fast
--   serialization with schema above.
--
-- Both use the byte order of the host machine. Data written on a machine with
-- one byte order cannot be read on a machine with the other.
--
-- 'Unbox' can be used for storing or transferring data if both ends use the
-- same byte order and word size, and the unwritten bytes are acceptable.
--
-- == Serialization using Unbox
--
-- Instances of 'Unbox' can be derived using 'Generic' or the Template Haskell
-- function 'deriveUnbox' provided in this module.
--
-- Writing a data type to an array using the array creation routines in
-- "Streamly.Data.Array" or "Streamly.Data.MutArray" (e.g. @writeN@ or
-- @fromListN@), serializes the type to the array. Similarly, reading the data
-- type from the array deserializes it. You can also serialize and deserialize
-- directly to and from a 'MutByteArray', using the type class methods.
--
-- == Serialization using Serialize
--
-- Use 'deriveSerialize' to derive the instances of the type class
-- automatically and then use the type class methods to serialize and
-- deserialize to and from a 'MutByteArray'.
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
