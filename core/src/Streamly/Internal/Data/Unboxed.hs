{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UnboxedTuples #-}

module Streamly.Internal.Data.Unboxed
    ( Storable
    , alignment
    , peek
    , poke
    , sizeOf
    ) where

#ifdef USE_STORABLE
import Foreign.Storable (Storable(..))
#else
import Data.Int (Int8)
import Data.Primitive.Types (Prim(..), sizeOf, alignment)
import GHC.Base (IO(..))
import GHC.Ptr (Ptr(..))
#endif

#ifndef USE_STORABLE
type Storable = Prim

{-# INLINE peek #-}
peek :: Prim a => Ptr a -> IO a
peek (Ptr addr#) = IO $ \s# -> readOffAddr# addr# 0# s#

{-# INLINE poke #-}
poke :: Prim a => Ptr a -> a -> IO ()
poke (Ptr addr#) a = IO $ \s# -> (# writeOffAddr# addr# 0# a s#, () #)

-- | Orphan Prim instance of Bool implemented using Int8
instance Prim Bool where
    sizeOf# _ = sizeOf# (undefined :: Int8)
    alignment# _ = alignment# (undefined :: Int8)
    indexByteArray# arr# i# = indexByteArray# arr# i# /= (0 :: Int8)
    readByteArray# arr# i# s# =
        case readByteArray# arr# i# s# of
            (# s1#, i :: Int8 #) -> (# s1#, i /= 0 #)
    writeByteArray# arr# i# a s# =
        case a of
            True -> writeByteArray# arr# i# (1 :: Int8) s#
            False -> writeByteArray# arr# i# (0 :: Int8) s#
    setByteArray# arr# off# len# a s# =
        case a of
            True -> setByteArray# arr# off# len# (1 :: Int8) s#
            False -> setByteArray# arr# off# len# (0 :: Int8) s#
    indexOffAddr# addr# i# = indexOffAddr# addr# i# /= (0 :: Int8)
    readOffAddr# addr# i# s# =
        case readOffAddr# addr# i# s# of
            (# s1#, i :: Int8 #) -> (# s1#, i /= 0 #)
    writeOffAddr# addr# i# a s# =
        case a of
            True -> writeOffAddr# addr# i# (1 :: Int8) s#
            False -> writeOffAddr# addr# i# (0 :: Int8) s#
    setOffAddr# addr# off# len# a s# =
        case a of
            True -> setOffAddr# addr# off# len# (1 :: Int8) s#
            False -> setOffAddr# addr# off# len# (0 :: Int8) s#
#endif
