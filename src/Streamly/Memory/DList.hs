{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Streamly.Memory.DList where

import Data.String (IsString(..))
import Data.Functor.Identity (runIdentity)
import Control.DeepSeq (NFData(..))
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import GHC.Base (nullAddr#)
import GHC.Generics (Generic)
import GHC.Exts (IsList(..), realWorld#)
import GHC.ForeignPtr (ForeignPtr(..), newForeignPtr_)
import GHC.IO (IO(IO), unsafePerformIO)
import Streamly.Internal.Data.Fold.Types (Fold(..))
import qualified Streamly.Streams.StreamD.Type as D
import qualified Streamly.Streams.StreamK as K
import Text.Read (readPrec, readListPrec, readListPrecDefault)

unsafeInlineIO :: IO a -> a
unsafeInlineIO (IO m) = case m realWorld# of (# _, r #) -> r

write :: forall m a. (MonadIO m, Storable a) => Fold m a (DList a)
write = Fold step initial extract
  where
    step l x = liftIO $ consIO x l  
    initial = return empty 
    extract = return 
    
writeN :: forall m a. (MonadIO m, Storable a) => a -> Int -> Fold m a (DList a)
writeN a n = Fold step initial extract
  where
    step l x = liftIO $ consIO x l  
    initial = liftIO . fromListIO . replicate n $ a
    extract = return 

foldl' :: Storable a => (b -> a -> b) -> b -> DList a -> b
foldl' f z a = runIdentity $ D.foldl' f z $ toStreamD a

read :: (Monad m, K.IsStream t, Storable a) => DList a -> t m a
read = D.fromStreamD . toStreamD

readRev :: (Monad m, K.IsStream t, Storable a) => DList a -> t m a
readRev = D.fromStreamD . toStreamDRev

toStreamK :: (K.IsStream t, Storable a) => DList a -> t m a
toStreamK = unsafeInlineIO . foldrIO K.cons K.nil

toStreamD :: (Monad m, Storable a) => DList a -> D.Stream m a
toStreamD dl =
    let p = unsafeInlineIO $ withForeignPtr (listHead dl) peek 
    in D.Stream step p
    where
    step _ p | p == nullPtr = return D.Stop
    step _ p =
        let x = unsafePerformIO $ peek p
          in return $ D.Yield (nodeValue x) (nextNode x)

toStreamDRev :: (Monad m, Storable a) => DList a -> D.Stream m a
toStreamDRev dl =
    let p = unsafeInlineIO $ withForeignPtr (listLast dl) peek 
    in D.Stream step p
    where
    step _ p | p == nullPtr = return D.Stop
    step _ p =
        let x = unsafePerformIO $ peek p
          in return $ D.Yield (nodeValue x) (prevNode x)

nullForeignPtr :: ForeignPtr a
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr")

data DList a = DList
  { listHead :: {-# UNPACK #-}!(ForeignPtr (Ptr (Node a))) -- First address
  , listLast :: {-# UNPACK #-}!(ForeignPtr (Ptr (Node a))) -- Last address
  } deriving (Eq)

instance (Storable a, Show a) => Show (DList a) where
  show = show . toList

instance (Storable a, Ord a) => Ord (DList a) where
  compare x y = unsafeInlineIO $ do
    xl <- toListIO x
    yl <- toListIO y
    return $ compare xl yl

instance (Storable a, NFData a) => NFData (DList a) where
    rnf = foldl' (\_ x -> rnf x) ()

instance (Storable a, Read a, Show a) => Read (DList a) where
    readPrec = fromList <$> readPrec
    readListPrec = readListPrecDefault

data Node a = Node
  { nodeValue :: a
  , prevNode :: {-# UNPACK #-}!(Ptr (Node a))
  , nextNode :: {-# UNPACK #-}!(Ptr (Node a))
  } deriving (Show, Generic, NFData)

instance Eq a => Eq (Node a) where
  x == y = nodeValue x == nodeValue y

instance Ord a => Ord (Node a) where
  compare x y = compare (nodeValue x) (nodeValue y)

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
empty = DList nullForeignPtr nullForeignPtr

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
  return $ DList fph fpt

consIO :: Storable a => a -> DList a -> IO (DList a)
consIO a dl
  | listHead dl == nullForeignPtr = singletonIO a
  | otherwise = do
    let fph = listHead dl
    p <- malloc
    ph <- withForeignPtr fph peek
    p `poke` Node a nullPtr ph 
    pokeByteOff ph (sizeOf a) p
    withForeignPtr fph (`poke` p)
    return dl

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

foldrIO :: Storable a => (a -> b -> b) -> b -> DList a -> IO b
foldrIO f z dl = do
  tp <- withForeignPtr (listLast dl) peek
  if tp == nullPtr
     then return z
     else do
       r <- go z =<< peek tp 
       touchForeignPtr (listHead dl)
       return r
  where
    go b (Node a px _)
      | px == nullPtr = return $ f a b 
      | otherwise = go (f a b) =<< peek px 

foldlIO :: Storable a => (b -> a -> b) -> b -> DList a -> IO b
foldlIO f z dl = do
  hp <- withForeignPtr (listHead dl) peek
  if hp == nullPtr
     then return z
     else do
       r <- go z =<< peek hp 
       touchForeignPtr (listHead dl)
       return r
  where
    go b (Node a _ nx)
      | nx == nullPtr = return $ f b a 
      | otherwise = go (f b a) =<< peek nx 

toListIO :: Storable a => DList a -> IO [a]
toListIO = foldrIO (:) []

toListRevIO :: Storable a => DList a -> IO [a]
toListRevIO = foldlIO (flip (:)) []

fromListIO :: Storable a => [a] -> IO (DList a)
fromListIO [] = return empty
fromListIO (x:xs) = consIO x =<< ioxs
  where ioxs = fromListIO xs

instance Storable a => IsList (DList a) where
    type (Item (DList a)) = a
    fromList = unsafeInlineIO . fromListIO
    toList = unsafeInlineIO . toListIO

instance (a ~ Char) => IsString (DList a) where
    fromString = unsafeInlineIO . fromListIO

