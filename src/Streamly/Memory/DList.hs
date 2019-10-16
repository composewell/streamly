{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Streamly.Memory.List
--
-- License     : BSD3
-- Stability   : experimental
-- Portability : GHC
module Streamly.Memory.DList where

import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import GHC.Base (nullAddr#)
import GHC.Exts (IsList(..))
import GHC.ForeignPtr (ForeignPtr(..), newForeignPtr_)
import GHC.IO (unsafePerformIO)
import Streamly.Internal.Data.Fold.Types (Fold(..))
import qualified Streamly.Streams.StreamD.Type as D
import qualified Streamly.Streams.StreamK as K

write :: forall m a. (MonadIO m, Storable a) => Fold m a (DList a)
write = Fold step initial extract
  where
    step l x = liftIO $ consIO x l  
    initial = return empty 
    extract = liftIO . return 
    
-- read :: (Monad m, K.IsStream t, Storable a) => DList a -> t m a
-- read = D.fromStreamD . toStreamD

-- Is it OK to use unsafePerformIO?
-- TODO: Make D.fromList
-- TODO: Make IsList instance
-- TODO: Make IsString instance

-- Implementation only works for Int.
-- Only made for checking performance benchmarks.
-- We could potentially use backpack is required.

nullForeignPtr :: ForeignPtr a
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr")

-- Only head (or) tail needs to be ForeignPtr

data DList a = DList
  { listHead :: {-# UNPACK #-}!(ForeignPtr (Ptr (Node a))) -- First address
  , listLast :: {-# UNPACK #-}!(ForeignPtr (Ptr (Node a))) -- Last address
  , lengthDL :: {-# UNPACK #-}!Int -- ~ Int#
  } deriving (Show)

data Node a = Node
  { nodeValue :: a
  , prevNode :: {-# UNPACK #-}!(Ptr (Node a))
  , nextNode :: {-# UNPACK #-}!(Ptr (Node a))
  } deriving (Show)

instance Eq a => Eq (Node a) where
  x == y = nodeValue x == nodeValue y


instance Storable a => Storable (Node a) where
  sizeOf _ = sizeOf (undefined :: a) + 2 * sizeOf (undefined :: Ptr (Node a))
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    nv <- peekByteOff ptr 0
    np <- peekByteOff ptr (sizeOf (undefined :: a))
    nn <-
      peekByteOff
        ptr
        (sizeOf (undefined :: a) + sizeOf (undefined :: Ptr (Node a)))
    return $ Node nv np nn
  poke ptr (Node nv np nn) = do
    pokeByteOff ptr 0 nv
    pokeByteOff ptr (sizeOf nv) np
    pokeByteOff ptr (sizeOf nv + sizeOf np) nn

empty :: DList a
empty = DList nullForeignPtr nullForeignPtr 0

isEmpty :: DList a -> Bool
isEmpty = (==) 0 . lengthDL

singletonIO :: Storable a => a -> IO (DList a)
singletonIO a = do
  p <- malloc
  p `poke` Node a nullPtr nullPtr
  pph <- malloc
  ppt <- malloc
  pph `poke` p
  ppt `poke` p
  fph <- newForeignPtr pph (freeDLP pph)
  fpt <- newForeignPtr_ ppt
  return $ DList fph fpt 1

-- Is it safe to write functions like
-- "insertAfterIO :: a -> Ptr (Node a) -> IO (ForeignPtr a)" and
-- "insertBeforeIO :: a -> Ptr (Node a) -> IO (ForeignPtr a)" ?

consIO :: Storable a => a -> DList a -> IO (DList a)
consIO a dl
  | isEmpty dl = singletonIO a
  | otherwise = do
    let (DList fph fpl l) = dl
    p <- malloc
    ph <- withForeignPtr fph peek
    Node ha _ hn <- peek ph
    p `poke` Node a nullPtr ph 
    ph `poke` Node ha p hn
    withForeignPtr fph (`poke` p)
    return $ DList fph fpl (l + 1)

snocIO :: Storable a => DList a -> a -> IO (DList a)
snocIO dl a
  | isEmpty dl = singletonIO a
  | otherwise = do
    let (DList fph fpl l) = dl
    p <- malloc
    pl <- withForeignPtr fpl peek
    Node ta tp _ <- peek pl
    p `poke` Node a pl nullPtr
    pl `poke` Node ta tp p
    withForeignPtr fpl (`poke` p)
    return $ DList fph fpl (l + 1)

foldlIO :: Storable a => (b -> a -> b) -> b -> DList a -> IO b
foldlIO f z (DList hfp _ _) = do
  hp <- withForeignPtr hfp peek
  if hp == nullPtr
     then return z
     else go z =<< peek hp 
  where
    go b (Node a _ nx)
      | nx == nullPtr = return $ f b a 
      | otherwise = go (f b a) =<< peek nx 

foldrIO :: Storable a => (a -> b -> b) -> b -> DList a -> IO b
foldrIO f z (DList _ tfp _) = do
  tp <- withForeignPtr tfp peek
  if tp == nullPtr
     then return z
     else go z =<< peek tp 
  where
    go b (Node a px _)
      | px == nullPtr = return $ f a b 
      | otherwise = go (f a b) =<< peek px 

toListIO :: Storable a => DList a -> IO [a]
toListIO = foldrIO (:) []

fromListIO :: Storable a => [a] -> IO (DList a)
fromListIO [] = return empty
fromListIO (x:xs) = consIO x =<< ioxs
  where ioxs = fromListIO xs

instance Storable a => IsList (DList a) where
    type (Item (DList a)) = a
    fromList = unsafePerformIO . fromListIO
    toList = unsafePerformIO . toListIO
    -- TODO: Make efficiently fromListN

freeDLP :: Storable a => Ptr (Ptr (Node a)) -> IO ()
freeDLP dlp
  | dlp == nullPtr = return ()
  | otherwise = go =<< peek dlp
    where
      go p
        | p == nullPtr = return ()
        | otherwise = do
            np <- nextNode <$> peek p
            free p
            go np

  {-
headIO :: Storable a => DList a -> IO (Maybe a)
headIO dl
  | dl == empty = return Nothing
  | otherwise =
    withForeignPtr (listHead dl) $ peek >=> return . Just . nodeValue

tailIO :: Storable a => DList a -> IO (Maybe (DList a))
tailIO dl
  | dl == empty = return Nothing
  | otherwise =
    withForeignPtr (listHead dl) $ \ptr -> do
      (Node a _ hn) <- peek =<< nextNode <$> peek ptr
      fp <- mallocPlainForeignPtr
      withForeignPtr fp (`poke` Node a nullPtr hn)
      return . Just $  DList fp (listLast dl) (lengthDL dl - 1)
-}
  {-

ml :: Int -> IO (DList Int)
ml 0 = return empty
ml i = do
  x <- ml (i - 1)
  consIO i x

test :: IO ()
test = do
  ml 10000
  ml 10000
  return ()
-}

        {-
sliceIO :: Storable a => Int -> Int -> DList a -> IO (Maybe (DList a))
sliceIO f t dl
  | f > t = Nothing
  | lengthDL dl < t + 1 = Nothing
  | otherwise = 
    where
      nH = withForeignPtr (listHead dl) $ \ptr -> do
             (Node a _ hn) <- peek =<< nextNode <$> peek ptr
             fp <- mallocPlainForeignPtr
             withForeignPtr fp (`poke` Node a nullPtr hn)
             return . Just $  DList fp (listLast dl) (lengthDL dl - 1)

-}



