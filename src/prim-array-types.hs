import GHC.Exts hiding (toList, fromListN, fromList)

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.Types

import Prelude hiding (length, unlines, foldr)

import Data.Word (Word8)
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.SVar (adaptState)

import qualified Prelude as P
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K

#ifdef PINNED
import qualified Streamly.Internal.Data.Prim.Pinned.Mutable.Array.Types as MA
#else
import qualified Streamly.Internal.Data.Prim.Mutable.Array.Types as MA
#endif

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

data Array a = Array ByteArray#

{-# INLINE unsafeFreeze #-}
unsafeFreeze :: PrimMonad m => MA.Array (PrimState m) a -> m (Array a)
unsafeFreeze (MA.Array arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, Array arr'# #))

{-# INLINE unsafeThaw #-}
unsafeThaw :: PrimMonad m => Array a -> m (MA.Array (PrimState m) a)
unsafeThaw (Array arr#)
  = primitive (\s# -> (# s#, MA.Array (unsafeCoerce# arr#) #))

{-# INLINE length #-}
length :: forall a. Prim a => Array a -> Int
length (Array arr#) =
    I# (quotInt# (sizeofByteArray# arr#) (sizeOf# (undefined :: a)))

{-# INLINE spliceTwo #-}
spliceTwo :: (PrimMonad m, Prim a) => Array a -> Array a -> m (Array a)
spliceTwo a1 a2 = do
    a1' <- unsafeThaw a1
    a2' <- unsafeThaw a2
    MA.spliceTwo a1' a2' >>= unsafeFreeze

{-# INLINE_NORMAL write #-}
write :: (PrimMonad m, Prim a) => Fold m a (Array a)
write = FL.mapM unsafeFreeze MA.write

{-# INLINE_NORMAL writeN #-}
writeN :: (PrimMonad m, Prim a) => Int -> Fold m a (Array a)
writeN limit = FL.mapM unsafeFreeze (MA.writeN limit)

{-# INLINE_NORMAL fromStreamDN #-}
fromStreamDN ::
       (PrimMonad m, Prim a)
    => Int
    -> D.Stream m a
    -> m (Array a)
fromStreamDN limit str = MA.fromStreamDN limit str >>= unsafeFreeze

{-# INLINE fromStreamD #-}
fromStreamD ::
       (PrimMonad m, Prim a) => D.Stream m a -> m (Array a)
fromStreamD str = MA.fromStreamD str >>= unsafeFreeze

fromList :: Prim a => [a] -> Array a
fromList vs = fromListN (P.length vs) vs

fromListN :: forall a. Prim a => Int -> [a] -> Array a
fromListN len vs = runST run where
  run :: forall s. ST s (Array a)
  run = do
    arr <- MA.newArray len
    let go :: [a] -> Int -> ST s ()
        go [] !ix = if ix == len
          then return ()
          else error "fromListN" "list length less than specified size"
        go (a : as) !ix = if ix < len
          then do
            MA.writeArray arr ix a
            go as (ix + 1)
          else error "fromListN" "list length greater than specified size"
    go vs 0
    unsafeFreeze arr

{-# INLINE_NORMAL fromStreamDArraysOf #-}
fromStreamDArraysOf ::
       forall m a. (PrimMonad m, Prim a)
    => Int
    -> D.Stream m a
    -> D.Stream m (Array a)
fromStreamDArraysOf n str = D.mapM unsafeFreeze (MA.fromStreamDArraysOf n str)

-- Check correctness
{-# INLINE unsafeIndex #-}
unsafeIndex :: Prim a => Array a -> Int -> a
unsafeIndex (Array arr#) (I# i#) = indexByteArray# arr# i#

data FlattenState s a =
      OuterLoop s
    | InnerLoop s !(Array a) !Int !Int

{-# INLINE_NORMAL flattenArrays #-}
flattenArrays :: (PrimMonad m, Prim a) => D.Stream m (Array a) -> D.Stream m a
flattenArrays (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield arr s ->
                let len = length arr
                in if len == 0
                   then D.Skip (OuterLoop s)
                   else D.Skip (InnerLoop s arr len 0)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ len i) | i == len =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st arr len i) = do
        let x = unsafeIndex arr i
        return $ D.Yield x (InnerLoop st arr len (i + 1))

{-# INLINE_NORMAL flattenArraysRev #-}
flattenArraysRev ::
       (PrimMonad m, Prim a)
    => D.Stream m (Array a)
    -> D.Stream m a
flattenArraysRev (D.Stream step state) = D.Stream step' (OuterLoop state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield arr s ->
                let len = length arr
                in if len == 0
                   then D.Skip (OuterLoop s)
                   else D.Skip (InnerLoop s arr len (len - 1))
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ _ i) | i == -1 =
        return $ D.Skip $ OuterLoop st

    step' _ (InnerLoop st arr len i) = do
        let x = unsafeIndex arr i
        return $ D.Yield x (InnerLoop st arr len (i - 1))

{-# INLINE byteLength #-}
byteLength :: Array a -> Int
byteLength (Array arr#) = I# (sizeofByteArray# arr#)

{-# INLINE_NORMAL packArraysChunksOf #-}
packArraysChunksOf ::
       (PrimMonad m, Prim a)
    => Int
    -> D.Stream m (Array a)
    -> D.Stream m (Array a)
packArraysChunksOf n str =
    D.mapM unsafeFreeze $ MA.packArraysChunksOf n (D.mapM unsafeThaw str)

{-# INLINE_NORMAL lpackArraysChunksOf #-}
lpackArraysChunksOf ::
       (PrimMonad m, Prim a)
    => Int
    -> Fold m (Array a) ()
    -> Fold m (Array a) ()
lpackArraysChunksOf n fld =
    FL.lmapM unsafeThaw $ MA.lpackArraysChunksOf n (FL.lmapM unsafeFreeze fld)

{-# INLINE_NORMAL unlines #-}
unlines ::
       (PrimMonad m, Prim a)
    => a
    -> D.Stream m (Array a)
    -> D.Stream m a
unlines sep (D.Stream step state) = D.Stream step' (OuterLoop state)
    where
    {-# INLINE_LATE step' #-}
    step' gst (OuterLoop st) = do
        r <- step (adaptState gst) st
        return $ case r of
            D.Yield arr s ->
                let len = length arr
                in D.Skip (InnerLoop s arr len 0)
            D.Skip s -> D.Skip (OuterLoop s)
            D.Stop -> D.Stop

    step' _ (InnerLoop st _ len i) | i == len =
        return $ D.Yield sep $ OuterLoop st

    step' _ (InnerLoop st arr len i) = do
        let x = unsafeIndex arr i
        return $ D.Yield x (InnerLoop st arr len (i + 1))

{-# INLINE foldr #-}
foldr ::
       forall a b. Prim a
    => (a -> b -> b)
    -> b
    -> Array a
    -> b
foldr f z arr = go 0
  where
    !sz = length arr
    go !i
      | sz > i = f (unsafeIndex arr i) (go (i+1))
      | otherwise = z

-- | Strict right-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldr' #-}
foldr' ::
       forall a b. Prim a
    => (a -> b -> b)
    -> b
    -> Array a
    -> b
foldr' f z0 arr = go (length arr - 1) z0
  where
    go !i !acc
      | i < 0 = acc
      | otherwise = go (i - 1) (f (unsafeIndex arr i) acc)

-- | Strict left-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldl' #-}
foldl' ::
       forall a b. Prim a
    => (b -> a -> b)
    -> b
    -> Array a
    -> b
foldl' f z0 arr = go 0 z0
  where
    !sz = length arr
    go !i !acc
      | i < sz = go (i + 1) (f acc (unsafeIndex arr i))
      | otherwise = acc

-- | Strict left-associated fold over the elements of a 'PrimArray'.
{-# INLINE foldlM' #-}
foldlM' :: (Prim a, Monad m) => (b -> a -> m b) -> b -> Array a -> m b
foldlM' f z0 arr = go 0 z0
  where
    !sz = length arr
    go !i !acc1
      | i < sz = do
          acc2 <- f acc1 (unsafeIndex arr i)
          go (i + 1) acc2
      | otherwise = return acc1

-- Drops the separator byte
-- Inefficcient compared to Memory Array
{-# INLINE breakOn #-}
breakOn ::
       PrimMonad m
    => Word8
    -> Array Word8
    -> m (Array Word8, Maybe (Array Word8))
breakOn sep arr = do
    let loc = foldl' chk (Left 0) arr
    case loc of
        Left _ -> return (arr, Nothing)
        Right i -> do
            let nLen = len - i - 1
            nArr <- MA.newArray nLen
            mArr <- unsafeThaw arr
            MA.unsafeCopy nArr 0 mArr (i + 1) nLen
            MA.shrinkArray mArr i
            arr1 <- unsafeFreeze mArr
            arr2 <- unsafeFreeze nArr
            return (arr1, Just arr2)
  where
    len = length arr
    chk (Left i) a =
        if a == sep
        then if i /= len - 1
             then Right i
             else Left i
        else Left (i + 1)
    chk r _ = r

data SplitState s arr
    = Initial s
    | Buffering s arr
    | Splitting s arr
    | Yielding arr (SplitState s arr)
    | Finishing

-- | Split a stream of arrays on a given separator byte, dropping the separator
-- and coalescing all the arrays between two separators into a single array.
--
-- @since 0.7.0
{-# INLINE_NORMAL splitOn #-}
splitOn
    :: PrimMonad m
    => Word8
    -> D.Stream m (Array Word8)
    -> D.Stream m (Array Word8)
splitOn byte (D.Stream step state) = D.Stream step' (Initial state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (Initial st) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                (arr1, marr2) <- breakOn byte arr
                return $ case marr2 of
                    Nothing   -> D.Skip (Buffering s arr1)
                    Just arr2 -> D.Skip (Yielding arr1 (Splitting s arr2))
            D.Skip s -> return $ D.Skip (Initial s)
            D.Stop -> return $ D.Stop

    step' gst (Buffering st buf) = do
        r <- step gst st
        case r of
            D.Yield arr s -> do
                (arr1, marr2) <- breakOn byte arr
                buf' <- spliceTwo buf arr1
                return $ case marr2 of
                    Nothing -> D.Skip (Buffering s buf')
                    Just x -> D.Skip (Yielding buf' (Splitting s x))
            D.Skip s -> return $ D.Skip (Buffering s buf)
            D.Stop -> return $
                if byteLength buf == 0
                then D.Stop
                else D.Skip (Yielding buf Finishing)

    step' _ (Splitting st buf) = do
        (arr1, marr2) <- breakOn byte buf
        return $ case marr2 of
                Nothing -> D.Skip $ Buffering st arr1
                Just arr2 -> D.Skip $ Yielding arr1 (Splitting st arr2)

    step' _ (Yielding arr next) = return $ D.Yield arr next
    step' _ Finishing = return $ D.Stop

{-# INLINE_NORMAL toStreamD #-}
toStreamD :: (Prim a, Monad m) => Array a -> D.Stream m a
toStreamD arr = D.Stream step 0
  where
    {-# INLINE_LATE step #-}
    step _ i
        | i == length arr = return D.Stop
    step _ i = return $ D.Yield (unsafeIndex arr i) (i + 1)

{-# INLINE toStreamK #-}
toStreamK ::
       forall t m a. (K.IsStream t, Prim a)
    => Array a
    -> t m a
toStreamK arr = go 0

    where
    len = length arr
    go p | p == len = K.nil
         | otherwise =
        -- See Note in toStreamD.
        let !x = unsafeIndex arr p
        in x `K.cons` go (p + 1)


{-# INLINE_NORMAL toStreamDRev #-}
toStreamDRev :: (Prim a, Monad m) => Array a -> D.Stream m a
toStreamDRev arr = D.Stream step (length arr - 1)
  where
    {-# INLINE_LATE step #-}
    step _ i
        | i < 0 = return D.Stop
    step _ i = return $ D.Yield (unsafeIndex arr i) (i - 1)

{-# INLINE toStreamKRev #-}
toStreamKRev ::
       forall t m a. (K.IsStream t, Prim a)
    => Array a
    -> t m a
toStreamKRev arr = go (length arr - 1)

    where
    go p | p == -1 = K.nil
         | otherwise =
        let !x = unsafeIndex arr p
        in x `K.cons` go (p - 1)

{-# INLINE_LATE toListFB #-}
toListFB :: forall a b. Prim a => (a -> b -> b) -> b -> Array a -> b
toListFB c n arr = go 0
    where
    len = length arr
    go p | p == len = n
    go p =
        let !x = unsafeIndex arr p
        in c x (go (p + 1))

-- | Convert an 'Array' into a list.
--
-- @since 0.7.0
{-# INLINE toList #-}
toList :: Prim a => Array a -> [a]
toList s = build (\c n -> toListFB c n s)
