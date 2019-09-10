{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Streamly.Memory.List
-- Copyright   : (c) 2018 Composewell Technologies
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com, pranaysashank@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Memory.List where

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..), poke)
import GHC.ForeignPtr (mallocPlainForeignPtr, newForeignPtr_)
import GHC.IO (unsafePerformIO)

import qualified Streamly.Streams.StreamD.Type as D

data List a
    = Null -- XXX Do I really need this, isn't a nullPtr enough??
    | List
          { listHead :: {-# UNPACK #-}!(ForeignPtr (Node a)) -- first address
          }

data Node a =
    Node
        { nodeValue :: a
        , nextNode :: {-# UNPACK #-}!(Ptr (Node a))
        }
    deriving (Eq, Ord, Show)

instance Storable a => Storable (Node a) where
    sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: Ptr (Node a))
    alignment _ = alignment (undefined :: a)
    peek ptr = do
        nv <- peekByteOff ptr 0
        nn <- peekByteOff ptr (sizeOf (undefined :: a))
        return $ Node nv nn
    poke ptr (Node nv nn) = do
        pokeByteOff ptr 0 nv
        pokeByteOff ptr (sizeOf nv) nn

newList :: List a
newList = Null

consIO ::
       forall a. Storable a
    => a
    -> List a
    -> IO (List a)
consIO a Null = do
    nodefp <- mallocPlainForeignPtr
    withForeignPtr nodefp (flip poke (Node a nullPtr))
    return $ List nodefp
consIO a (List fp) = do
    nodefp <- mallocPlainForeignPtr
    withForeignPtr nodefp (flip poke (Node a (unsafeForeignPtrToPtr fp)))
    return $ List nodefp

snocIO ::
       forall a. Storable a
    => a
    -> List a
    -> IO (List a)
snocIO a Null = do
    nodefp <- mallocPlainForeignPtr
    withForeignPtr nodefp (flip poke (Node a nullPtr))
    return $ List nodefp
snocIO a (List fp) = do
    ptr <- withForeignPtr fp go
    fptr <- newForeignPtr_ ptr
    return $ List fptr
  where
    go :: Ptr (Node a) -> IO (Ptr (Node a))
    go ptr
        | ptr == nullPtr = do
            nodefp <- mallocPlainForeignPtr
            withForeignPtr nodefp (flip poke (Node a nullPtr))
            return $ unsafeForeignPtrToPtr nodefp
        | otherwise = do
            node <- peek ptr
            nextPtr <- go (nextNode node)
            poke ptr (node {nextNode = nextPtr})
            return ptr

toListIO ::
       forall a. Storable a
    => List a
    -> IO [a]
toListIO Null = return []
toListIO (List fp) = withForeignPtr fp go
  where
    go :: Ptr (Node a) -> IO [a]
    go ptr
        | ptr == nullPtr = return []
        | otherwise = do
            node <- peek ptr
            xs <- go (nextNode node)
            return $ nodeValue node : xs

(!!) ::
       forall a. Storable a
    => List a
    -> Int
    -> Maybe a
_ !! i | i < 0 = Nothing
Null !! _ = Nothing
(List fp) !! i = unsafePerformIO (withForeignPtr fp (go i))
  where
    go :: Int -> Ptr (Node a) -> IO (Maybe a)
    go n ptr
        | ptr == nullPtr = return Nothing
        | otherwise = do
            node <- peek ptr
            if n == 0
                then return $ Just (nodeValue node)
                else go (n - 1) (nextNode node)

headIO ::
         forall a. Storable a
    => List a
    -> IO (Maybe a)
headIO Null = return Nothing
headIO (List fp) =
    withForeignPtr fp $ \ptr -> do
        node <- peek ptr
        return $ Just (nodeValue node)

head ::
       forall a. Storable a
    => List a
    -> Maybe a
head = unsafePerformIO . headIO

insertBy ::
       forall a. Storable a
    => (a -> a -> Ordering)
    -> a
    -> List a
    -> List a
insertBy _ x Null = cons x Null
insertBy cmp x (List fp) =
    unsafePerformIO $
    withForeignPtr fp (\ptr -> go ptr >>= newForeignPtr_ >>= return . List)
  where
    go :: Ptr (Node a) -> IO (Ptr (Node a))
    go ptr
        | ptr == nullPtr = do
            nodefp <- mallocPlainForeignPtr
            withForeignPtr nodefp (flip poke (Node x nullPtr))
            return $ unsafeForeignPtrToPtr nodefp
        | otherwise = do
            node <- peek ptr
            case cmp x (nodeValue node) of
                GT -> do
                    nextPtr <- go (nextNode node)
                    poke ptr (node {nextNode = nextPtr})
                    return ptr
                _ -> do
                    nodefp <- mallocPlainForeignPtr
                    withForeignPtr nodefp (flip poke (Node x ptr))
                    return $ unsafeForeignPtrToPtr nodefp

insert :: forall a . (Storable a, Ord a) => a -> List a -> List a
insert = insertBy compare

fromStreamD :: (Monad m, Storable a) => D.Stream m a -> m (List a)
fromStreamD = D.foldr cons newList

cons ::
       forall a. Storable a
    => a
    -> List a
    -> List a
cons x l = unsafePerformIO (consIO x l)

snoc ::
       forall a. Storable a
    => a
    -> List a
    -> List a
snoc x l = unsafePerformIO (snocIO x l)

toList ::
       forall a. Storable a
    => List a
    -> [a]
toList l = unsafePerformIO (toListIO l)
