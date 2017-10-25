{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Asyncly.Prelude
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Asyncly.Prelude
    ( Streaming (..)
    , StreamT
    , InterleavedT
    , AsyncT
    , ParallelT
    , ZipStream
    , ZipAsync
    , serially
    , toListSerial
    , interleaved
    , toListInterleaved
    , asyncly
    , toListAsync
    , parallely
    , toListParallel
    )
where

import Control.Applicative (Alternative (..), liftA2)
import Control.Arrow (second)
import Data.Semigroup (Semigroup(..), cycle1)
import           Control.Monad               (MonadPlus(..))
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Error.Class   (MonadError(..))
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.Reader.Class  (MonadReader(..))
import           Control.Monad.State.Class   (MonadState(..))
import           Control.Monad.Trans.Class   (MonadTrans)
import Prelude hiding (drop, take, zipWith)
import Asyncly.AsyncT

------------------------------------------------------------------------------
-- Streams with different styles of product composition
------------------------------------------------------------------------------

newtype StreamT m a = StreamT {getStreamT :: Stream m a}
    deriving (Functor, Applicative, Monad, Semigroup, Monoid, Num, Fractional,
              Floating, MonadTrans, MonadIO, MonadThrow)

deriving instance MonadAsync m => Alternative (StreamT m)
deriving instance MonadAsync m => MonadPlus (StreamT m)
deriving instance (MonadBase b m, Monad m) => MonadBase b (StreamT m)
deriving instance MonadError e m => MonadError e (StreamT m)
deriving instance MonadReader r m => MonadReader r (StreamT m)
deriving instance MonadState s m => MonadState s (StreamT m)

-- XXX the applicative, functor and num instances may not be correct
newtype InterleavedT m a = InterleavedT {getInterleavedT :: Stream m a}
    deriving (Functor, Applicative, Semigroup, Monoid, Num, Fractional,
              Floating, MonadTrans, MonadIO, MonadThrow)

deriving instance MonadAsync m => Alternative (InterleavedT m)
deriving instance MonadAsync m => MonadPlus (InterleavedT m)
deriving instance (MonadBase b m, Monad m) => MonadBase b (InterleavedT m)
deriving instance MonadError e m => MonadError e (InterleavedT m)
deriving instance MonadReader r m => MonadReader r (InterleavedT m)
deriving instance MonadState s m => MonadState s (InterleavedT m)

newtype AsyncT m a = AsyncT {getAsyncT :: Stream m a}
    deriving (Functor, Applicative, Semigroup, Monoid, Num, Fractional,
              Floating, MonadTrans)

deriving instance MonadAsync m => MonadIO (AsyncT m)
deriving instance MonadAsync m => MonadThrow (AsyncT m)
deriving instance (MonadBase b m, MonadAsync m) => MonadBase b (AsyncT m)
deriving instance MonadAsync m => Alternative (AsyncT m)
deriving instance MonadAsync m => MonadPlus (AsyncT m)
deriving instance (MonadError e m, MonadAsync m) => MonadError e (AsyncT m)
deriving instance (MonadReader r m, MonadAsync m) => MonadReader r (AsyncT m)
deriving instance (MonadState s m, MonadAsync m) => MonadState s (AsyncT m)

newtype ParallelT m a = ParallelT {getParallelT :: Stream m a}
    deriving (Functor, Applicative, Semigroup, Monoid, Num, Fractional,
              Floating, MonadTrans)

deriving instance MonadAsync m => MonadIO (ParallelT m)
deriving instance MonadAsync m => MonadThrow (ParallelT m)
deriving instance (MonadBase b m, MonadAsync m) => MonadBase b (ParallelT m)
deriving instance MonadAsync m => Alternative (ParallelT m)
deriving instance MonadAsync m => MonadPlus (ParallelT m)
deriving instance (MonadError e m, MonadAsync m) => MonadError e (ParallelT m)
deriving instance (MonadReader r m, MonadAsync m) => MonadReader r (ParallelT m)
deriving instance (MonadState s m, MonadAsync m) => MonadState s (ParallelT m)

infixr 5 <=>

class Streaming t where
    -- sum style operations

    -- | Sequential interleaved composition, similar to '<>' except that it
    -- fairly interleaves the two 'AsyncT' streams, yielding an element from
    -- each stream alternately. Should be used only on finite streams.
    (<=>) :: t m a -> t m a -> t m a

    -- | Parallel composition similar to '<|>' except that it is left-biased
    -- instead of being fair.  Action on the left is likely to be given a
    -- chance to execute before the action on the right. If the left action
    -- keeps yielding results without blocking it continues running until it
    -- finishes and only then the right action runs. Unlike '<|>' it can be
    -- used on infinite streams.
    (<|) :: MonadAsync m => t m a -> t m a -> t m a

    -- Conversions
    fromStream      :: StreamT m a      -> t m a
    fromInterleaved :: InterleavedT m a -> t m a
    fromAsync       :: AsyncT m a       -> t m a
    fromParallel    :: ParallelT m a    -> t m a
    fromZipStream   :: ZipStream m a    -> t m a
    fromZipAsync    :: ZipAsync m a     -> t m a

    -- * Elimination

    -- | Decompose a stream into its head and tail. If the stream is empty,
    -- returns 'Nothing'. If the stream is non-empty, returns 'Just (a, ma)',
    -- where 'a' is the head of the stream and 'ma' its tail.
    uncons :: MonadAsync m => t m a -> m (Maybe (a, t m a))

    -- * Construction
    unfoldr :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> t m a

    -- * Special folds
    take :: MonadAsync m => Int -> t m a -> t m a
    drop :: MonadAsync m => Int -> t m a -> t m a

    -- * Zipping
    zipWith :: Monad m => (a -> b -> c) -> t m a -> t m b -> t m c
    zipWithM :: Monad m =>
        (a -> b -> t m c) -> t m a -> t m b -> t m c

    zipAsyncWithM :: MonadAsync m
        => (a -> b -> t m c) -> t m a -> t m b -> t m c
    zipAsyncWith :: MonadAsync m
        => (a -> b -> c) -> t m a -> t m b -> t m c

instance Streaming StreamT where
    {-# INLINE (<=>) #-}
    m1 <=> m2 = StreamT $ interleave (getStreamT m1) (getStreamT m2)

    {-# INLINE (<|) #-}
    m1 <| m2 = StreamT $ parLeft (getStreamT m1) (getStreamT m2)

    fromStream      = id
    fromInterleaved = StreamT . getInterleavedT
    fromAsync       = StreamT . getAsyncT
    fromParallel    = StreamT . getParallelT
    fromZipStream   = StreamT . getZipStream
    fromZipAsync    = StreamT . getZipAsync

    uncons (StreamT s) = fmap (fmap (second StreamT)) (next s)
    unfoldr step seed = StreamT $ unfold step seed
    take n (StreamT s) = StreamT $ taking n s
    drop n (StreamT s) = StreamT $ dropping n s
    zipWith f (StreamT s1) (StreamT s2) = StreamT $ zippingWith f s1 s2
    zipWithM f (StreamT s1) (StreamT s2) = StreamT $ zippingWithM g s1 s2
        where g a b = getStreamT (f a b)
    zipAsyncWith f (StreamT s1) (StreamT s2) = StreamT $ zippingAsyncWith f s1 s2
    zipAsyncWithM f (StreamT s1) (StreamT s2) = StreamT $ zippingAsyncWithM g s1 s2
        where g a b = getStreamT (f a b)

instance Streaming InterleavedT where
    {-# INLINE (<=>) #-}
    m1 <=> m2 = InterleavedT $ interleave (getInterleavedT m1) (getInterleavedT m2)

    {-# INLINE (<|) #-}
    m1 <| m2 = InterleavedT $ parLeft (getInterleavedT m1) (getInterleavedT m2)

    fromStream      = InterleavedT . getStreamT
    fromInterleaved = id
    fromAsync       = InterleavedT . getAsyncT
    fromParallel    = InterleavedT . getParallelT
    fromZipStream   = InterleavedT . getZipStream
    fromZipAsync    = InterleavedT . getZipAsync

    uncons (InterleavedT s) = fmap (fmap (second InterleavedT)) (next s)
    unfoldr step seed = InterleavedT $ unfold step seed
    take n (InterleavedT s) = InterleavedT $ taking n s
    drop n (InterleavedT s) = InterleavedT $ dropping n s
    zipWith f (InterleavedT s1) (InterleavedT s2) = InterleavedT $ zippingWith f s1 s2
    zipWithM f (InterleavedT s1) (InterleavedT s2) = InterleavedT $ zippingWithM g s1 s2
        where g a b = getInterleavedT (f a b)
    zipAsyncWith f (InterleavedT s1) (InterleavedT s2) = InterleavedT $ zippingAsyncWith f s1 s2
    zipAsyncWithM f (InterleavedT s1) (InterleavedT s2) = InterleavedT $ zippingAsyncWithM g s1 s2
        where g a b = getInterleavedT (f a b)

instance Streaming AsyncT where
    {-# INLINE (<=>) #-}
    m1 <=> m2 = AsyncT $ interleave (getAsyncT m1) (getAsyncT m2)

    {-# INLINE (<|) #-}
    m1 <| m2 = AsyncT $ parLeft (getAsyncT m1) (getAsyncT m2)

    fromStream      = AsyncT . getStreamT
    fromInterleaved = AsyncT . getInterleavedT
    fromAsync       = id
    fromParallel    = AsyncT . getParallelT
    fromZipStream   = AsyncT . getZipStream
    fromZipAsync    = AsyncT . getZipAsync

    uncons (AsyncT s) = fmap (fmap (second AsyncT)) (next s)
    unfoldr step seed = AsyncT $ unfold step seed
    take n (AsyncT s) = AsyncT $ taking n s
    drop n (AsyncT s) = AsyncT $ dropping n s
    zipWith f (AsyncT s1) (AsyncT s2) = AsyncT $ zippingWith f s1 s2
    zipWithM f (AsyncT s1) (AsyncT s2) = AsyncT $ zippingWithM g s1 s2
        where g a b = getAsyncT (f a b)
    zipAsyncWith f (AsyncT s1) (AsyncT s2) = AsyncT $ zippingAsyncWith f s1 s2
    zipAsyncWithM f (AsyncT s1) (AsyncT s2) = AsyncT $ zippingAsyncWithM g s1 s2
        where g a b = getAsyncT (f a b)

instance Streaming ParallelT where
    {-# INLINE (<=>) #-}
    m1 <=> m2 = ParallelT $ interleave (getParallelT m1) (getParallelT m2)

    {-# INLINE (<|) #-}
    m1 <| m2 = ParallelT $ parLeft (getParallelT m1) (getParallelT m2)

    fromStream      = ParallelT . getStreamT
    fromInterleaved = ParallelT . getInterleavedT
    fromAsync       = ParallelT . getAsyncT
    fromParallel    = id
    fromZipStream   = ParallelT . getZipStream
    fromZipAsync    = ParallelT . getZipAsync

    uncons (ParallelT s) = fmap (fmap (second ParallelT)) (next s)
    unfoldr step seed = ParallelT $ unfold step seed
    take n (ParallelT s) = ParallelT $ taking n s
    drop n (ParallelT s) = ParallelT $ dropping n s
    zipWith f (ParallelT s1) (ParallelT s2) = ParallelT $ zippingWith f s1 s2
    zipWithM f (ParallelT s1) (ParallelT s2) = ParallelT $ zippingWithM g s1 s2
        where g a b = getParallelT (f a b)
    zipAsyncWith f (ParallelT s1) (ParallelT s2) = ParallelT $ zippingAsyncWith f s1 s2
    zipAsyncWithM f (ParallelT s1) (ParallelT s2) = ParallelT $ zippingAsyncWithM g s1 s2
        where g a b = getParallelT (f a b)

------------------------------------------------------------------------------
-- Alternative ways to bind
------------------------------------------------------------------------------

instance Monad m => Monad (InterleavedT m) where
    -- | Execute a monadic action for each element in the stream, in a fairly
    -- interleaved manner i.e. iterations yield alternately.
    (InterleavedT (Stream m)) >>= f = InterleavedT $ Stream $ \_ stp yld ->
        let run x = (runStream x) Nothing stp yld
            yield a Nothing  = run $ getInterleavedT (f a)
            yield a (Just r) = run $ getInterleavedT (f a)
                                     `interleave`
                                     getInterleavedT ((InterleavedT r) >>= f)
        in m Nothing stp yield

{-# INLINE parbind #-}
parbind
    :: (forall c. Stream m c -> Stream m c -> Stream m c)
    -> Stream m a
    -> (a -> Stream m b)
    -> Stream m b
parbind k m f = go m
    where
        go (Stream g) =
            Stream $ \ctx stp yld ->
            let run x = (runStream x) ctx stp yld
                yield a Nothing  = run $ f a
                yield a (Just r) = run $ f a `k` (go r)
            in g Nothing stp yield

instance MonadAsync m => Monad (AsyncT m) where
    -- | Execute a monadic action for each element in the stream, running
    -- iterations in parallel, but giving preference to iterations started
    -- earlier.
    (AsyncT m) >>= f = AsyncT $ parbind par m g
        where g x = getAsyncT (f x)
              par = parallel (CtxType Conjunction LIFO)

instance MonadAsync m => Monad (ParallelT m) where
    -- | Execute a monadic action for each element in the stream, running
    -- iterations in a fairly parallel manner, i.e. all iterations are equally
    -- likely to run.
    (ParallelT m) >>= f = ParallelT $ parbind par m g
        where g x = getParallelT (f x)
              par = parallel (CtxType Conjunction FIFO)

serially :: Monad m => StreamT m a -> m ()
serially = runStreaming . getStreamT

toListSerial :: Monad m => StreamT m a -> m [a]
toListSerial = toList . getStreamT

interleaved :: Monad m => InterleavedT m a -> m ()
interleaved = runStreaming . getInterleavedT

toListInterleaved :: Monad m => InterleavedT m a -> m [a]
toListInterleaved = toList . getInterleavedT

asyncly :: Monad m => AsyncT m a -> m ()
asyncly = runStreaming . getAsyncT

toListAsync :: Monad m => AsyncT m a -> m [a]
toListAsync = toList . getAsyncT

parallely :: Monad m => ParallelT m a -> m ()
parallely = runStreaming . getParallelT

toListParallel :: Monad m => ParallelT m a -> m [a]
toListParallel = toList . getParallelT

------------------------------------------------------------------------------
-- Construct and deconstruct
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns 'Just (a, ma)', where 'a' is
-- the head of the stream and 'ma' its tail.
next :: MonadAsync m => Stream m a -> m (Maybe (a, Stream m a))
next m = (runStream m) Nothing stop yield

    where

    stop = return Nothing

    {-# INLINE yield #-}
    yield a Nothing  = return (Just (a, empty))
    yield a (Just x) = return (Just (a, x))

-- | Build a Stream by unfolding steps starting from a seed.
unfold :: MonadAsync m => (b -> m (Maybe (a, b))) -> b -> Stream m a
unfold step = go
    where
    go s = Stream $ \_ stp yld -> do
        mayb <- step s
        case mayb of
            Nothing -> stp
            Just (a, b) -> yld a (Just (go b))

------------------------------------------------------------------------------
-- Special folds
------------------------------------------------------------------------------

-- | Take first 'n' elements from the stream and discard the rest.
taking :: MonadAsync m => Int -> Stream m a -> Stream m a
taking n m = Stream $ \ctx stp yld -> do
    let yield a Nothing  = yld a Nothing
        yield a (Just x) = yld a (Just (taking (n - 1) x))
    if (n <= 0)
    then stp
    else (runStream m) ctx stp yield

-- | Discard first 'n' elements from the stream and take the rest.
dropping :: MonadAsync m => Int -> Stream m a -> Stream m a
dropping n m = Stream $ \ctx stp yld -> do
    let yield _ Nothing  = stp
        yield _ (Just x) = (runStream $ dropping (n - 1) x) ctx stp yld
    if (n <= 0)
    then (runStream m) ctx stp yld
    else (runStream m) ctx stp yield

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | Zip two AsyncT streams serially using a monadic zipping function.
zippingWithM :: Monad m =>
    (a -> b -> Stream m c) -> Stream m a -> Stream m b -> Stream m c
zippingWithM f m1 m2 = Stream $ \_ stp yld -> do
    let merge a ra =
            let yield2 b Nothing   = (runStream (f a b)) Nothing stp yld
                yield2 b (Just rb) =
                    (runStream ((f a b) <> (zippingWithM f ra rb))) Nothing stp yld
             in (runStream m2) Nothing stp yield2
    let yield1 a Nothing   = merge a mempty
        yield1 a (Just ra) = merge a ra
    (runStream m1) Nothing stp yield1

-- | Zip two AsyncT streams serially using a pure zipping function.
zippingWith :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zippingWith f m1 m2 = Stream $ \_ stp yld -> do
    let merge a ra =
            let yield2 b Nothing   = yld (f a b) Nothing
                yield2 b (Just rb) = yld (f a b) (Just (zippingWith f ra rb))
             in (runStream m2) Nothing stp yield2
    let yield1 a Nothing   = merge a mempty
        yield1 a (Just ra) = merge a ra
    (runStream m1) Nothing stp yield1

-- | Wrapper around AsyncT type with a serial zipping Applicative instance.
-- Note that the binary function interleave (\<=>) is a special case of
-- ZipStream Applicative.
--
-- > f <$> ZipStream xs1 <*> ... <*> ZipStream xsN
newtype ZipStream m a = ZipStream {getZipStream :: Stream m a}
        deriving (Functor)

instance Monad m => Applicative (ZipStream m) where
    pure a = ZipStream $ cycle1 (pure a)
    (ZipStream xs) <*> (ZipStream ys) = ZipStream (zippingWith id xs ys)

instance Streaming ZipStream where
    {-# INLINE (<=>) #-}
    m1 <=> m2 = ZipStream $ interleave (getZipStream m1) (getZipStream m2)

    {-# INLINE (<|) #-}
    m1 <| m2 = ZipStream $ parLeft (getZipStream m1) (getZipStream m2)

    fromStream      = ZipStream . getStreamT
    fromInterleaved = ZipStream . getInterleavedT
    fromAsync       = ZipStream . getAsyncT
    fromParallel    = ZipStream . getParallelT
    fromZipStream   = ZipStream . getZipStream
    fromZipAsync    = ZipStream . getZipAsync

    uncons (ZipStream s) = fmap (fmap (second ZipStream)) (next s)
    unfoldr step seed = ZipStream $ unfold step seed
    take n (ZipStream s) = ZipStream $ taking n s
    drop n (ZipStream s) = ZipStream $ dropping n s
    zipWith f (ZipStream s1) (ZipStream s2) = ZipStream $ zippingWith f s1 s2
    zipWithM f (ZipStream s1) (ZipStream s2) = ZipStream $ zippingWithM g s1 s2
        where g a b = getZipStream (f a b)
    zipAsyncWith f (ZipStream s1) (ZipStream s2) = ZipStream $ zippingAsyncWith f s1 s2
    zipAsyncWithM f (ZipStream s1) (ZipStream s2) = ZipStream $ zippingAsyncWithM g s1 s2
        where g a b = getZipStream (f a b)

instance (Monad m, Num a) => Num (ZipStream m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (ZipStream m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (ZipStream m a) where
    pi = pure pi

    exp  = fmap exp
    sqrt = fmap sqrt
    log  = fmap log
    sin  = fmap sin
    tan  = fmap tan
    cos  = fmap cos
    asin = fmap asin
    atan = fmap atan
    acos = fmap acos
    sinh = fmap sinh
    tanh = fmap tanh
    cosh = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase

------------------------------------------------------------------------------
-- Asyncly Zipping Streams
------------------------------------------------------------------------------

-- | Zip two AsyncT streams asyncly (i.e. both the streams are generated
-- concurrently) using a monadic zipping function.
zippingAsyncWithM :: MonadAsync m
    => (a -> b -> Stream m c) -> Stream m a -> Stream m b -> Stream m c
zippingAsyncWithM f m1 m2 = Stream $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runStream (zippingWithM f ma mb)) Nothing stp yld

-- | Zip two AsyncT streams asyncly (i.e. both the streams are generated
-- concurrently) using a pure zipping function.
zippingAsyncWith :: MonadAsync m
    => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zippingAsyncWith f m1 m2 = Stream $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runStream (zippingWith f ma mb)) Nothing stp yld

-- | Wrapper around AsyncT type with a parallel zipping Applicative instance.
-- Note that the binary operator (\<|>) from the Alternative instance of AsyncT
-- is a special case of ZipAsync Applicative.
--
-- > f <$> ZipAsync xs1 <*> ... <*> ZipAsync xsN
newtype ZipAsync m a = ZipAsync {getZipAsync :: Stream m a}
        deriving (Functor)

instance MonadAsync m => Applicative (ZipAsync m) where
    pure a = ZipAsync $ cycle1 (pure a)
    (ZipAsync xs) <*> (ZipAsync ys) = ZipAsync (zippingAsyncWith id xs ys)

instance Streaming ZipAsync where
    {-# INLINE (<=>) #-}
    m1 <=> m2 = ZipAsync $ interleave (getZipAsync m1) (getZipAsync m2)

    {-# INLINE (<|) #-}
    m1 <| m2 = ZipAsync $ parLeft (getZipAsync m1) (getZipAsync m2)

    fromStream      = ZipAsync . getStreamT
    fromInterleaved = ZipAsync . getInterleavedT
    fromAsync       = ZipAsync . getAsyncT
    fromParallel    = ZipAsync . getParallelT
    fromZipStream   = ZipAsync . getZipStream
    fromZipAsync    = ZipAsync . getZipAsync

    uncons (ZipAsync s) = fmap (fmap (second ZipAsync)) (next s)
    unfoldr step seed = ZipAsync $ unfold step seed
    take n (ZipAsync s) = ZipAsync $ taking n s
    drop n (ZipAsync s) = ZipAsync $ dropping n s
    zipWith f (ZipAsync s1) (ZipAsync s2) = ZipAsync $ zippingWith f s1 s2
    zipWithM f (ZipAsync s1) (ZipAsync s2) = ZipAsync $ zippingWithM g s1 s2
        where g a b = getZipAsync (f a b)
    zipAsyncWith f (ZipAsync s1) (ZipAsync s2) = ZipAsync $ zippingAsyncWith f s1 s2
    zipAsyncWithM f (ZipAsync s1) (ZipAsync s2) = ZipAsync $ zippingAsyncWithM g s1 s2
        where g a b = getZipAsync (f a b)

instance (MonadAsync m, Num a) => Num (ZipAsync m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (MonadAsync m, Fractional a) => Fractional (ZipAsync m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (MonadAsync m, Floating a) => Floating (ZipAsync m a) where
    pi = pure pi

    exp  = fmap exp
    sqrt = fmap sqrt
    log  = fmap log
    sin  = fmap sin
    tan  = fmap tan
    cos  = fmap cos
    asin = fmap asin
    atan = fmap atan
    acos = fmap acos
    sinh = fmap sinh
    tanh = fmap tanh
    cosh = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase
