-- |
-- Module      : Streamly.Data.Serialize
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--
-- Fast binary serialization and deserialization of Haskell values to and from
-- arrays. This module provides two type classes for serialization, 'Unbox' and
-- 'Serialize'. The speed is similar to, and in some cases many times faster
-- than the store package. Conceptually, the 'Serialize' type class works in
-- the same way as store.
--
-- == Using Unbox
--
-- The 'Unbox' type class is simple and used to serialize non-recursive fixed
-- size data types. This type class is primarily used to implement unboxed
-- arrays. Unboxed arrays are just serialization of fixed length Haskell data
-- types. Instances of this type class can be derived using 'Generic' or
-- template haskell based deriving functions provided in this module. Read the
-- type class documentation for more details.
--
-- Writing a data type to an array using the array creation routines in
-- "Streamly.Data.Array" or "Streamly.Data.MutArray", serializes the type to
-- the array. Similarly, reading the data type from the array deserializes it.
--
-- There are no encode/decode routines provided to serialize or deserialize
-- using this type class but you can write those easily. Just create a mutable
-- byte array and use 'pokeByteIndex' to serialize a type, and use
-- 'peekByteIndex' to deserialize it.
--
-- == Using Serialize
--
-- The 'Serialize' type class is a superset of the 'Unbox' type class, it can
-- serialize variable length data types as well e.g. Haskell lists. Use
-- 'deriveSerialize' to derive the instances of the type class automatically
-- and then use 'pinnedEncode', 'decode' to serialize and deserialize the type
-- respectively. You can also create the array yourself and use the type class
-- functions directly.
--
module Streamly.Data.Serialize
    (

    -- * MutableByteArray
      MutByteArray
    , isPinned
    , pin
    , unpin
    , newByteArray
    , pinnedNewByteArray

    -- * Unbox
    , Unbox(..)
    , deriveUnbox

    -- * Serialize
    , Serialize(..)

    -- Deriving instances
    , SerializeConfig
    , serializeConfig
    , inlineSize
    , inlineSerialize
    , inlineDeserialize

    , deriveSerialize
    , deriveSerializeWith

    -- Encoding and Decoding
    -- , encode
    , pinnedEncode
    , decode
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Streamly.Internal.Data.Serialize
