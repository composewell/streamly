-- MOVE THIS TO A DIFFERENT LOCATION

import GHC.Exts

import Control.Monad.Primitive
import Data.Primitive.Types

import Prelude hiding (length, unlines)

import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.SVar (adaptState)
import Control.Monad (when)
import Streamly.Internal.Data.Strict (Tuple'(..))


import qualified Streamly.Internal.Data.Stream.StreamD.Type as D

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

-- This is not supposed to be used
data Array s a = Array (MutableByteArray# s)

{-# INLINE unsafeCopy #-}
unsafeCopy ::
       forall m a. (PrimMonad m, Prim a)
    => Array (PrimState m) a -- ^ destination array
    -> Int -- ^ offset into destination array
    -> Array (PrimState m) a -- ^ source array
    -> Int -- ^ offset into source array
    -> Int -- ^ number of elements to copy
    -> m ()
unsafeCopy (Array dst#) (I# doff#) (Array src#) (I# soff#) (I# n#)
  = primitive_ (copyMutableByteArray#
      src#
      (soff# *# (sizeOf# (undefined :: a)))
      dst#
      (doff# *# (sizeOf# (undefined :: a)))
      (n# *# (sizeOf# (undefined :: a)))
    )

{-# INLINE length #-}
length ::
       forall s a. Prim a
    => Array s a
    -> Int
length (Array arr#) =
    I# (quotInt# (sizeofMutableByteArray# arr#) (sizeOf# (undefined :: a)))

{-# INLINE spliceTwo #-}
spliceTwo ::
       forall m a. (PrimMonad m, Prim a)
    => Array (PrimState m) a
    -> Array (PrimState m) a
    -> m (Array (PrimState m) a)
spliceTwo a1 a2 = do
    a3 <- resizeArray a1 (l1 + l2)
    unsafeCopy a2 0 a3 l1 l2
    return a3
  where
    l1 = length a1
    l2 = length a2

{-# INLINE writeArray #-}
writeArray ::
       forall m a. (PrimMonad m, Prim a)
    => Array (PrimState m) a -- ^ array
    -> Int -- ^ index
    -> a -- ^ element
    -> m ()
writeArray (Array arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

{-# INLINE shrinkArray #-}
shrinkArray ::
       forall m a. (PrimMonad m, Prim a)
    => Array (PrimState m) a
    -> Int -- ^ new size
    -> m ()
shrinkArray (Array arr#) (I# n#)
  = primitive_ (shrinkMutableByteArray# arr# (n# *# sizeOf# (undefined :: a)))

{-# INLINE_NORMAL write #-}
write ::
       forall m a. (PrimMonad m, Prim a)
    => Fold m a (Array (PrimState m) a)
write = Fold step initial extract
  where
    initial = do
        marr <- newArray 0
        return (marr, 0, 0)
    step (marr, i, capacity) x
        | i == capacity =
            let newCapacity = max (capacity * 2) 1
             in do newMarr <- resizeArray marr newCapacity
                   writeArray newMarr i x
                   return (newMarr, i + 1, newCapacity)
        | otherwise = do
            writeArray marr i x
            return (marr, i + 1, capacity)
    extract (marr, len, _) = shrinkArray marr len >> return marr

{-# INLINE_NORMAL writeN #-}
writeN ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> Fold m a (Array (PrimState m) a)
writeN limit = Fold step initial extract
  where
    initial = do
        marr <- newArray limit
        return (marr, 0)
    step (marr, i) x
        | i == limit = return (marr, i)
        | otherwise = do
            writeArray marr i x
            return (marr, i + 1)
    extract (marr, len) = shrinkArray marr len >> return marr



{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> D.Stream m a
    -> m (Array (PrimState m) a)
fromStreamDN limit str = do
    marr <- newArray (max limit 0)
    _ <-
        D.foldlM'
            (\i x -> i `seq` (writeArray marr i x) >> return (i + 1))
            0 $
        D.take limit str
    return marr

{-# INLINE runFold #-}
runFold :: (Monad m) => Fold m a b -> D.Stream m a -> m b
runFold (Fold step begin done) = D.foldlMx' step begin done

{-# INLINE fromStreamD #-}
fromStreamD ::
       forall m a. (PrimMonad m, Prim a)
    => D.Stream m a
    -> m (Array (PrimState m) a)
fromStreamD str = runFold write str

{-# INLINABLE fromListNM #-}
fromListNM ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> [a]
    -> m (Array (PrimState m) a)
fromListNM n xs = fromStreamDN n $ D.fromList xs

{-# INLINABLE fromListM #-}
fromListM ::
       forall m a. (PrimMonad m, Prim a)
    => [a]
    -> m (Array (PrimState m) a)
fromListM xs = fromStreamD $ D.fromList xs

data GroupState s t a
    = GroupStart s
    | GroupBuffer s (Array t a) Int
    | GroupYield (Array t a) s
    | GroupLastYield (Array t a) Int
    | GroupFinish

{-# INLINE_NORMAL fromStreamDArraysOf #-}
fromStreamDArraysOf ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> D.Stream m a
    -> D.Stream m (Array (PrimState m) a)
-- fromStreamDArraysOf n str = D.groupsOf n (writeN n) str
fromStreamDArraysOf n (D.Stream step state) = D.Stream step' (GroupStart state)
  where
    {-# INLINE_LATE step' #-}
    step' _ (GroupStart st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $
            "Streamly.Internal.Memory.Mutable.Array.Types.fromStreamDArraysOf: the size of " ++
            "arrays [" ++ show n ++ "] must be a natural number"
        arr <- newArray n
        return $ D.Skip (GroupBuffer st arr 0)
    step' gst (GroupBuffer st arr i)
        | i < n = do
            r <- step (adaptState gst) st
            case r of
                D.Yield x s -> do
                    writeArray arr i x
                    return $ D.Skip (GroupBuffer s arr (i + 1))
                D.Skip s -> return $ D.Skip (GroupBuffer s arr i)
                D.Stop -> return $ D.Skip (GroupLastYield arr i)
        | otherwise = return $ D.Skip (GroupYield arr st)
    step' _ (GroupYield arr st) = do
        nArr <- newArray n
        return $ D.Yield arr (GroupBuffer st nArr 0)
    step' _ (GroupLastYield arr i)
        | i == 0 = return D.Stop
        | otherwise = do
            shrinkArray arr i
            return $ D.Yield arr GroupFinish
    step' _ GroupFinish = return D.Stop

-- Check correctness
{-# INLINE unsafeIndexM #-}
unsafeIndexM ::
       forall m a. (PrimMonad m, Prim a)
    => Array (PrimState m) a
    -> Int
    -> m a
unsafeIndexM (Array arr#) (I# i#) =
    primitive (\s# -> case indexByteArray# (unsafeCoerce# arr#) i# of
                        a -> (# s#, a #))

{-# INLINE byteLength #-}
byteLength :: Array s a -> Int
byteLength (Array arr#) =
    I# (sizeofMutableByteArray# arr#)

data SpliceState s arr
    = SpliceInitial s
    | SpliceBuffering s arr
    | SpliceYielding arr (SpliceState s arr)
    | SpliceFinish

-- Int in bytes?
{-# INLINE_NORMAL packArraysChunksOf #-}
packArraysChunksOf ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> D.Stream m (Array (PrimState m) a)
    -> D.Stream m (Array (PrimState m) a)
packArraysChunksOf n (D.Stream step state) =
    D.Stream step' (SpliceInitial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (SpliceInitial st) = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Memory.Mutable.Array.Types.packArraysChunksOf: the size of "
                 ++ "arrays [" ++ show n ++ "] must be a natural number"
        r <- step gst st
        case r of
            D.Yield arr s -> return $
                let len = byteLength arr
                 in if len >= n
                    then D.Skip (SpliceYielding arr (SpliceInitial s))
                    else D.Skip (SpliceBuffering s arr)
            D.Skip s -> return $ D.Skip (SpliceInitial s)
            D.Stop -> return $ D.Stop

    step' gst (SpliceBuffering st buf) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                let len = byteLength buf + byteLength arr
                if len > n
                then return $
                    D.Skip (SpliceYielding buf (SpliceBuffering s arr))
                else do
                    buf' <- spliceTwo buf arr
                    return $ D.Skip (SpliceBuffering s buf')
            D.Skip s -> return $ D.Skip (SpliceBuffering s buf)
            D.Stop -> return $ D.Skip (SpliceYielding buf SpliceFinish)

    step' _ SpliceFinish = return D.Stop

    step' _ (SpliceYielding arr next) = return $ D.Yield arr next

{-# INLINE_NORMAL lpackArraysChunksOf #-}
lpackArraysChunksOf ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> Fold m (Array (PrimState m) a) ()
    -> Fold m (Array (PrimState m) a) ()
lpackArraysChunksOf n (Fold step1 initial1 extract1) =
    Fold step initial extract

    where

    initial = do
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Memory.Mutable.Array.Types.packArraysChunksOf: the size of "
                 ++ "arrays [" ++ show n ++ "] must be a natural number"
        r1 <- initial1
        return (Tuple' Nothing r1)

    extract (Tuple' Nothing r1) = extract1 r1
    extract (Tuple' (Just buf) r1) = do
        r <- step1 r1 buf
        extract1 r

    step (Tuple' Nothing r1) arr = do
            let len = byteLength arr
             in if len >= n
                then do
                    r <- step1 r1 arr
                    extract1 r
                    r1' <- initial1
                    return (Tuple' Nothing r1')
                else return (Tuple' (Just arr) r1)

    step (Tuple' (Just buf) r1) arr = do
            let len = byteLength buf + byteLength arr
            buf' <- spliceTwo buf arr

            if len >= n
            then do
                r <- step1 r1 buf'
                extract1 r
                r1' <- initial1
                return (Tuple' Nothing r1')
            else return (Tuple' (Just buf') r1)
