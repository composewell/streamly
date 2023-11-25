-- |
-- Module      : Streamly.Data.MutByteArray
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
-- == Mutable Byte Array
--
-- 'MutByteArray' is a primitive mutable array in the IO monad. 'Unbox' and
-- 'Serialize' type classes use this primitive array to serialize data to and
-- deserialize it from. This array can be wrapped into higher level unboxed
-- array types 'Streamly.Data.MutArray.MutArray' and 'Streamly.Data.Array.Array'.
--
-- == Using Unbox
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
-- == Using Serialize
--
-- The 'Serialize' type class is a superset of the 'Unbox' type class, it can
-- serialize variable length data types as well e.g. Haskell lists. Use
-- 'deriveSerialize' to derive the instances of the type class automatically
-- and then use 'pinnedEncode', 'decode' to serialize and deserialize the type
-- to and from an 'Array' type. You can also serialize and deserialize directly
-- to and from a 'MutByteArray', using the type class methods.
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

import Streamly.Internal.Data.MutByteArray
