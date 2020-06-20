import GHC.Exts hiding (fromListN, fromList, toList)
import qualified GHC.Exts as Exts

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive.Types

#if __GLASGOW_HASKELL__ < 808
import Data.Semigroup (Semigroup(..))
#endif


import Prelude hiding (length, unlines, foldr)

import Data.Word (Word8)
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.SVar (adaptState)
import Text.Read (readPrec, readListPrec, readListPrecDefault)

import Control.DeepSeq (NFData(..))

import qualified Prelude as P
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.StreamD.Type as D
import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Data.Primitive.ByteArray as PB

-------------------------------------------------------------------------------
-- Array Data Type
-------------------------------------------------------------------------------

data Array a = Array ByteArray#

empty :: forall a. Prim a => Array a
empty = runST run where
  run :: forall s. ST s (Array a)
  run = do
    arr <- MA.newArray 0
    unsafeFreeze arr


mkChunkSizeKB :: Int -> Int
mkChunkSizeKB n = n * k
   where k = 1024

-- | Default maximum buffer size in bytes, for reading from and writing to IO
-- devices, the value is 32KB minus GHC allocation overhead, which is a few
-- bytes, so that the actual allocation is 32KB.
defaultChunkSize :: Int
defaultChunkSize = mkChunkSizeKB 32

sameByteArray :: ByteArray# -> ByteArray# -> Bool
sameByteArray ba1 ba2 =
    case reallyUnsafePtrEquality# (unsafeCoerce# ba1 :: ()) (unsafeCoerce# ba2 :: ()) of
#if __GLASGOW_HASKELL__ >= 708
      r -> isTrue# r
#else
      1# -> True
      _ -> False
#endif

-- | @since 0.6.4.0
instance (Eq a, Prim a) => Eq (Array a) where
  a1@(Array ba1#) == a2@(Array ba2#)
    | sameByteArray ba1# ba2# = True
    | sz1 /= sz2 = False
    | otherwise = loop (quot sz1 (sizeOf (undefined :: a)) - 1)
    where
    -- Here, we take the size in bytes, not in elements. We do this
    -- since it allows us to defer performing the division to
    -- calculate the size in elements.
    sz1 = PB.sizeofByteArray (PB.ByteArray ba1#)
    sz2 = PB.sizeofByteArray (PB.ByteArray ba2#)
    loop !i
      | i < 0 = True
      | otherwise = unsafeIndex a1 i == unsafeIndex a2 i && loop (i-1)
  {-# INLINE (==) #-}

-- | Lexicographic ordering. Subject to change between major versions.
--
--   @since 0.6.4.0
instance (Ord a, Prim a) => Ord (Array a) where
  compare a1@(Array ba1#) a2@(Array ba2#)
    | sameByteArray ba1# ba2# = EQ
    | otherwise = loop 0
    where
    sz1 = PB.sizeofByteArray (PB.ByteArray ba1#)
    sz2 = PB.sizeofByteArray (PB.ByteArray ba2#)
    sz = quot (min sz1 sz2) (sizeOf (undefined :: a))
    loop !i
      | i < sz = compare (unsafeIndex a1 i) (unsafeIndex a2 i) <> loop (i+1)
      | otherwise = compare sz1 sz2
  {-# INLINE compare #-}

instance Prim a => Semigroup (Array a) where
  a <> b = unsafeInlineIO (spliceTwo a b :: IO (Array a))

instance Prim a => Monoid (Array a) where
  mempty = empty
  mappend = (<>)

instance Prim a => NFData (Array a) where
    {-# INLINE rnf #-}
    rnf = foldl' (\_ _ -> ()) ()

-- | @since 0.6.4.0
instance (Show a, Prim a) => Show (Array a) where
  showsPrec p a = showParen (p > 10) $
    showString "fromListN " . shows (length a) . showString " "
      . shows (toList a)

instance (a ~ Char) => IsString (Array a) where
    {-# INLINE fromString #-}
    fromString = fromList

-- GHC versions 8.0 and below cannot derive IsList
instance Prim a => IsList (Array a) where
    type (Item (Array a)) = a
    {-# INLINE fromList #-}
    fromList = fromList
    {-# INLINE fromListN #-}
    fromListN = fromListN
    {-# INLINE toList #-}
    toList = toList

instance (Prim a, Read a, Show a) => Read (Array a) where
    {-# INLINE readPrec #-}
    readPrec = fromList <$> readPrec
    readListPrec = readListPrecDefault

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

{-# INLINE fromList #-}
fromList :: Prim a => [a] -> Array a
fromList vs = fromListN (P.length vs) vs

{-# INLINE fromListN #-}
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
breakOn sep arr =
    case loc of
        Left _ -> return (arr, Nothing)
        Right (Left _) -> do
            mArr <- unsafeThaw arr
            MA.shrinkArray mArr (len - 1)
            arr' <- unsafeFreeze mArr
            return (arr', Just empty)
        Right (Right i) -> do
            let nLen = len - i - 1
            nArr <- MA.newArray nLen
            mArr <- unsafeThaw arr
            MA.unsafeCopy nArr 0 mArr (i + 1) nLen
            MA.shrinkArray mArr i
            arr1 <- unsafeFreeze mArr
            arr2 <- unsafeFreeze nArr
            return (arr1, Just arr2)
  where
    loc = foldl' chk (Left 0) arr
    len = length arr
    chk (Left i) a =
        if a == sep
            then if i /= len - 1
                     then Right (Right i)
                     else Right (Left i)
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
