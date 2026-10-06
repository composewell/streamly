-- This is required as all the instances in this module are orphan instances.
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Streamly.Internal.Data.MutByteArray
-- Copyright   : (c) 2023 Composewell Technologies
-- License     : BSD3-3-Clause
-- Maintainer  : streamly@composewell.com
-- Portability : GHC
--

module Streamly.Internal.Data.MutByteArray
    (
    -- * MutByteArray
      module Streamly.Internal.Data.MutByteArray.Type
    -- * Unbox
    , module Streamly.Internal.Data.Unbox
    , module Streamly.Internal.Data.Unbox.TH
    -- * Serialize
    , module Streamly.Internal.Data.Serialize.Type
    -- * Serialize TH
    , module Streamly.Internal.Data.Serialize.TH
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- MachDeps.h includes ghcautoconf.h that defines WORDS_BIGENDIAN for big
-- endian systems.
#include "MachDeps.h"

import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Streamly.Internal.Data.Array (Array(..))
import GHC.Base (assert)
import GHC.Exts (ByteArray#, Int(..), sizeofByteArray#, unsafeCoerce#)
import GHC.Word (Word8)

#if __GLASGOW_HASKELL__ >= 900
#if !defined(WORDS_BIGENDIAN)
import GHC.Exts ((-#), int2Word#)
import GHC.Num.Integer (integerFromByteArray)
#endif
import GHC.Num.Integer (Integer(..))
#else
#if !defined(WORDS_BIGENDIAN)
import GHC.Exts ((-#), int2Word#)
import GHC.Integer.GMP.Internals (importIntegerFromByteArray)
#endif
import GHC.Integer.GMP.Internals (Integer(..), BigNat(..))
#endif

import Streamly.Internal.Data.MutByteArray.Type
import Streamly.Internal.Data.Serialize.TH
import Streamly.Internal.Data.Serialize.Type
import Streamly.Internal.Data.Unbox
import Streamly.Internal.Data.Unbox.TH

--------------------------------------------------------------------------------
-- Common instances
--------------------------------------------------------------------------------

-- Note
-- ====
--
-- Even a non-functional change such as changing the order of constructors will
-- change the instance derivation.
--
-- This will not pose a problem if both, encode, and decode are done by the same
-- version of the application. There *might* be a problem if version that
-- encodes differs from the version that decodes.
--
-- We need to add some compatibility tests using different versions of
-- dependencies.
--
-- Although such chages for the most basic types won't happen we need to detect
-- if it ever happens.
--
-- Should we worry about these kind of changes and this kind of compatibility?
-- This is a problem for all types of derivations that depend on the order of
-- constructors, for example, Enum.

-- Note on Windows build
-- =====================
--
-- On Windows, having template haskell splices here fail the build with the
-- following error:
--
-- @
-- addLibrarySearchPath: C:\...  (Win32 error 3): The system cannot find the path specified.
-- @
--
-- The error might be irrelavant but having these splices triggers it. We should
-- either fix the problem or avoid the use to template haskell splices in this
-- file.
--
-- Similar issue: https://github.com/haskell/cabal/issues/4741

-- $(Serialize.deriveSerialize ''Maybe)
instance Serialize a => Serialize (Maybe a) where

    {-# INLINE addSizeTo #-}
    addSizeTo acc x =
        case x of
            Nothing -> acc + 1
            Just field0 -> addSizeTo (acc + 1) field0

    {-# INLINE deserializeAt #-}
    deserializeAt initialOffset arr endOffset = do
        (i0, tag) <- deserializeAt initialOffset arr endOffset
        case tag :: Word8 of
            0 -> pure (i0, Nothing)
            1 -> do (i1, a0) <- deserializeAt i0 arr endOffset
                    pure (i1, Just a0)
            _ -> error "Found invalid tag while peeking (Maybe a)"

    {-# INLINE serializeAt #-}
    serializeAt initialOffset arr val =
        case val of
            Nothing -> serializeAt initialOffset arr (0 :: Word8)
            Just field0 -> do
                i0 <- serializeAt initialOffset arr (1 :: Word8)
                serializeAt i0 arr field0

-- $(Serialize.deriveSerialize ''Either)
instance (Serialize a, Serialize b) => Serialize (Either a b) where

    {-# INLINE addSizeTo #-}
    addSizeTo acc x =
        case x of
            Left field0 -> addSizeTo (acc + 1) field0
            Right field0 -> addSizeTo (acc + 1) field0

    {-# INLINE deserializeAt #-}
    deserializeAt initialOffset arr endOffset = do
        (i0, tag) <- deserializeAt initialOffset arr endOffset
        case tag :: Word8 of
            0 -> do (i1, a0) <- deserializeAt i0 arr endOffset
                    pure (i1, Left a0)
            1 -> do (i1, a0) <- deserializeAt i0 arr endOffset
                    pure (i1, Right a0)
            _ -> error "Found invalid tag while peeking (Either a b)"

    {-# INLINE serializeAt #-}
    serializeAt initialOffset arr val =
        case val of
            Left field0 -> do
                i0 <- serializeAt initialOffset arr (0 :: Word8)
                serializeAt i0 arr field0
            Right field0 -> do
                i0 <- serializeAt initialOffset arr (1 :: Word8)
                serializeAt i0 arr field0

instance Serialize (Proxy a) where

    {-# INLINE addSizeTo #-}
    addSizeTo acc _ = acc + 1

    {-# INLINE deserializeAt #-}
    deserializeAt initialOffset _ _ = pure (initialOffset + 1, Proxy)

    {-# INLINE serializeAt #-}
    serializeAt initialOffset _ _ = pure (initialOffset + 1)

--------------------------------------------------------------------------------
-- Integer
--------------------------------------------------------------------------------

-- A small Integer is serialized as Int64 so that it can be deserialized on
-- platforms with a different word size, e.g. the JavaScript backend has 32-bit
-- words.
--
-- A large Integer is serialized as the words of its BigNat, as they are in
-- memory. The tag records the word size of the platform that serialized it:
-- LIP and LIN have 64-bit words, LIP32 and LIN32 have 32-bit words. The
-- words are used as they are if the word size is the same as the host's,
-- otherwise they are converted.
data LiftedInteger
    = LIS Int64
    | LIP (Array Word)
    | LIN (Array Word)
    | LIP32 (Array Word)
    | LIN32 (Array Word)

-- $(Serialize.deriveSerialize ''LiftedInteger)
instance Serialize LiftedInteger where

    {-# INLINE addSizeTo #-}
    addSizeTo acc x =
        case x of
            LIS field0 -> addSizeTo (acc + 1) field0
            LIP field0 -> addSizeTo (acc + 1) field0
            LIN field0 -> addSizeTo (acc + 1) field0
            LIP32 field0 -> addSizeTo (acc + 1) field0
            LIN32 field0 -> addSizeTo (acc + 1) field0

    {-# INLINE deserializeAt #-}
    deserializeAt initialOffset arr endOffset = do
        (i0, tag) <- deserializeAt initialOffset arr endOffset
        case tag :: Word8 of
            0 -> do (i1, a0) <- deserializeAt i0 arr endOffset
                    pure (i1, LIS a0)
            1 -> do (i1, a0) <- deserializeAt i0 arr endOffset
                    pure (i1, LIP a0)
            2 -> do (i1, a0) <- deserializeAt i0 arr endOffset
                    pure (i1, LIN a0)
            3 -> do (i1, a0) <- deserializeAt i0 arr endOffset
                    pure (i1, LIP32 a0)
            4 -> do (i1, a0) <- deserializeAt i0 arr endOffset
                    pure (i1, LIN32 a0)
            _ -> error "Found invalid tag while peeking (LiftedInteger)"

    {-# INLINE serializeAt #-}
    serializeAt initialOffset arr val =
        case val of
            LIS field0 -> do
                i0 <- serializeAt initialOffset arr (0 :: Word8)
                serializeAt i0 arr field0
            LIP field0 -> do
                i0 <- serializeAt initialOffset arr (1 :: Word8)
                serializeAt i0 arr field0
            LIN field0 -> do
                i0 <- serializeAt initialOffset arr (2 :: Word8)
                serializeAt i0 arr field0
            LIP32 field0 -> do
                i0 <- serializeAt initialOffset arr (3 :: Word8)
                serializeAt i0 arr field0
            LIN32 field0 -> do
                i0 <- serializeAt initialOffset arr (4 :: Word8)
                serializeAt i0 arr field0

-- | The constructors for a large Integer with the host's word size.
{-# INLINE liftPositive #-}
{-# INLINE liftNegative #-}
liftPositive, liftNegative :: Array Word -> LiftedInteger
#if WORD_SIZE_IN_BITS == 64
liftPositive = LIP
liftNegative = LIN
#else
liftPositive = LIP32
liftNegative = LIN32
#endif

-- | Build a positive Integer from the words of a BigNat serialized on a
-- platform with a different word size.
{-# INLINE fromForeignWords #-}
fromForeignWords :: Array Word -> Integer
#if !defined(WORDS_BIGENDIAN)
-- The words of a BigNat, least significant first, are a little endian
-- base-256 number on a little endian host. Building the Integer from it
-- removes the high zero words and makes a small Integer if the value fits in
-- an Int.
#if __GLASGOW_HASKELL__ >= 900
fromForeignWords (Array (MutByteArray x) (I# start) (I# end)) =
    integerFromByteArray
        (int2Word# (end -# start)) (unsafeCoerce# x) (int2Word# start) 0#
#else
fromForeignWords (Array (MutByteArray x) (I# start) (I# end)) =
    importIntegerFromByteArray
        (unsafeCoerce# x) (int2Word# start) (int2Word# (end -# start)) 0#
#endif
#else
-- On a big endian host the words are least significant first but the bytes
-- of each word are most significant first, the bytes are not a base-256
-- number.
fromForeignWords _ =
    error $ "Deserializing an Integer serialized on a big endian platform "
        ++ "with a different word size is not supported"
#endif

#if __GLASGOW_HASKELL__ >= 900

{-# INLINE liftInteger #-}
liftInteger :: Integer -> LiftedInteger
liftInteger (IS x) = LIS (fromIntegral (I# x))
liftInteger (IP x) =
    liftPositive
        (Array (MutByteArray (unsafeCoerce# x)) 0 (I# (sizeofByteArray# x)))
liftInteger (IN x) =
    liftNegative
        (Array (MutByteArray (unsafeCoerce# x)) 0 (I# (sizeofByteArray# x)))

-- | Build a positive Integer from the words of a BigNat serialized on a
-- platform with the same word size. The words are used as the BigNat, the
-- BigNat size is the size of the byte array, therefore, the array must not be
-- a slice.
{-# INLINE fromHostWords #-}
fromHostWords :: Array Word -> Integer
fromHostWords (Array (MutByteArray x) start end) =
    let ba = unsafeCoerce# x :: ByteArray#
     in assert (start == 0 && end == I# (sizeofByteArray# ba)) (IP ba)

#else

{-# INLINE liftInteger #-}
liftInteger :: Integer -> LiftedInteger
liftInteger (S# x) = LIS (fromIntegral (I# x))
liftInteger (Jp# (BN# x)) =
    liftPositive
        (Array (MutByteArray (unsafeCoerce# x)) 0 (I# (sizeofByteArray# x)))
liftInteger (Jn# (BN# x)) =
    liftNegative
        (Array (MutByteArray (unsafeCoerce# x)) 0 (I# (sizeofByteArray# x)))

-- | See fromHostWords for GHC 9.0 and later.
{-# INLINE fromHostWords #-}
fromHostWords :: Array Word -> Integer
fromHostWords (Array (MutByteArray x) start end) =
    let ba = unsafeCoerce# x :: ByteArray#
     in assert (start == 0 && end == I# (sizeofByteArray# ba)) (Jp# (BN# ba))

#endif

{-# INLINE unliftInteger #-}
unliftInteger :: LiftedInteger -> Integer
unliftInteger (LIS x) = fromIntegral x
#if WORD_SIZE_IN_BITS == 64
unliftInteger (LIP arr) = fromHostWords arr
unliftInteger (LIN arr) = negate (fromHostWords arr)
unliftInteger (LIP32 arr) = fromForeignWords arr
unliftInteger (LIN32 arr) = negate (fromForeignWords arr)
#else
unliftInteger (LIP arr) = fromForeignWords arr
unliftInteger (LIN arr) = negate (fromForeignWords arr)
unliftInteger (LIP32 arr) = fromHostWords arr
unliftInteger (LIN32 arr) = negate (fromHostWords arr)
#endif

instance Serialize Integer where
    {-# INLINE addSizeTo #-}
    addSizeTo i a = addSizeTo i (liftInteger a)

    {-# INLINE deserializeAt #-}
    deserializeAt off arr end =
        fmap unliftInteger <$> deserializeAt off arr end

    {-# INLINE serializeAt #-}
    serializeAt off arr val = serializeAt off arr (liftInteger val)
