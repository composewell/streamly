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
    (
    -- * Product Style Composition
      StreamT
    , InterleavedT
    , AsyncT
    , ParallelT
    , ZipStream
    , ZipAsync

    -- * Stream Type Adapters
    , serially
    , interleaving
    , asyncly
    , parallely
    , zipping
    , zippingAsync
    , adapt

    -- * Construction
    , unfoldr

    -- * Elimination
    , uncons

    -- * Special folds
    , take
    , drop

    -- * Zipping
    , zipWith
    , zipWithM
    , zipAsyncWith
    , zipAsyncWithM
    )
where

import           Control.Applicative         (Alternative (..), liftA2)
import           Control.Monad               (MonadPlus(..))
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Error.Class   (MonadError(..))
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.Reader.Class  (MonadReader(..))
import           Control.Monad.State.Class   (MonadState(..))
import           Control.Monad.Trans.Class   (MonadTrans)
import           Data.Semigroup              (Semigroup(..), cycle1)
import           Prelude hiding              (drop, take, zipWith)
import           Asyncly.AsyncT

------------------------------------------------------------------------------
-- StreamT iterates serially in the Monad and Applicative compositions
------------------------------------------------------------------------------

newtype StreamT m a = StreamT {getStreamT :: Stream m a}
    deriving (Functor, Applicative, Monad, Semigroup, Monoid,
              MonadTrans, MonadIO, MonadThrow)

deriving instance MonadAsync m => Alternative (StreamT m)
deriving instance MonadAsync m => MonadPlus (StreamT m)
deriving instance (MonadBase b m, Monad m) => MonadBase b (StreamT m)
deriving instance MonadError e m => MonadError e (StreamT m)
deriving instance MonadReader r m => MonadReader r (StreamT m)
deriving instance MonadState s m => MonadState s (StreamT m)

instance Streaming StreamT where
    toStream = getStreamT
    fromStream = StreamT

-- XXX The Functor/Applicative/Num instances for all the types are exactly the
-- same, how can we reduce this boilerplate (use TH)? We cannot derive them
-- from a single base type because they depend on the Monad instance which is
-- different for each type.

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (Monad m, Num a) => Num (StreamT m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (StreamT m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (StreamT m a) where
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
-- InterleavedT interleaves the iterations in the Monad and Applicative
-- compositions. It does not make much sense for infinite streams.
------------------------------------------------------------------------------

newtype InterleavedT m a = InterleavedT {getInterleavedT :: Stream m a}
    deriving (Functor, Applicative, Semigroup, Monoid,
              MonadTrans, MonadIO, MonadThrow)

deriving instance MonadAsync m => Alternative (InterleavedT m)
deriving instance MonadAsync m => MonadPlus (InterleavedT m)
deriving instance (MonadBase b m, Monad m) => MonadBase b (InterleavedT m)
deriving instance MonadError e m => MonadError e (InterleavedT m)
deriving instance MonadReader r m => MonadReader r (InterleavedT m)
deriving instance MonadState s m => MonadState s (InterleavedT m)

instance Streaming InterleavedT where
    toStream = getInterleavedT
    fromStream = InterleavedT

-- | Execute a monadic action for each element in the stream, in a fairly
-- interleaved manner i.e. iterations yield alternately.
instance Monad m => Monad (InterleavedT m) where
    (InterleavedT (Stream m)) >>= f = InterleavedT $ Stream $ \_ stp yld ->
        let run x = (runStream x) Nothing stp yld
            yield a Nothing  = run $ getInterleavedT (f a)
            yield a (Just r) = run $ getInterleavedT (f a)
                                     `interleave`
                                     getInterleavedT ((InterleavedT r) >>= f)
        in m Nothing stp yield

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (Monad m, Num a) => Num (InterleavedT m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (InterleavedT m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (InterleavedT m a) where
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
-- AsyncT runs the iterations asynchronously in the Monad and Applicative
-- compositions. More iterations are started in parallel on demand. It can work
-- with infinite streams.
------------------------------------------------------------------------------

newtype AsyncT m a = AsyncT {getAsyncT :: Stream m a}
    deriving (Functor, Applicative, Semigroup, Monoid, MonadTrans)

deriving instance MonadAsync m => MonadIO (AsyncT m)
deriving instance MonadAsync m => MonadThrow (AsyncT m)
deriving instance (MonadBase b m, MonadAsync m) => MonadBase b (AsyncT m)
deriving instance MonadAsync m => Alternative (AsyncT m)
deriving instance MonadAsync m => MonadPlus (AsyncT m)
deriving instance (MonadError e m, MonadAsync m) => MonadError e (AsyncT m)
deriving instance (MonadReader r m, MonadAsync m) => MonadReader r (AsyncT m)
deriving instance (MonadState s m, MonadAsync m) => MonadState s (AsyncT m)

instance Streaming AsyncT where
    toStream = getAsyncT
    fromStream = AsyncT

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

-- | Execute a monadic action for each element in the stream, running
-- iterations in parallel, but giving preference to iterations started
-- earlier.
instance MonadAsync m => Monad (AsyncT m) where
    (AsyncT m) >>= f = AsyncT $ parbind par m g
        where g x = getAsyncT (f x)
              par = parallel (CtxType Conjunction LIFO)

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (Monad m, Num a) => Num (AsyncT m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (AsyncT m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (AsyncT m a) where
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
-- ParallelT runs all iterations in parallel in the Monad and Applicative
-- compositions. Iterations are scheduled in a round-robin fashion. It cannot
-- work well when the streams being composed are infinite.
------------------------------------------------------------------------------

newtype ParallelT m a = ParallelT {getParallelT :: Stream m a}
    deriving (Functor, Applicative, Semigroup, Monoid, MonadTrans)

deriving instance MonadAsync m => MonadIO (ParallelT m)
deriving instance MonadAsync m => MonadThrow (ParallelT m)
deriving instance (MonadBase b m, MonadAsync m) => MonadBase b (ParallelT m)
deriving instance MonadAsync m => Alternative (ParallelT m)
deriving instance MonadAsync m => MonadPlus (ParallelT m)
deriving instance (MonadError e m, MonadAsync m) => MonadError e (ParallelT m)
deriving instance (MonadReader r m, MonadAsync m) => MonadReader r (ParallelT m)
deriving instance (MonadState s m, MonadAsync m) => MonadState s (ParallelT m)

instance Streaming ParallelT where
    toStream = getParallelT
    fromStream = ParallelT

-- | Execute a monadic action for each element in the stream, running
-- iterations in a fairly parallel manner, i.e. all iterations are equally
-- likely to run.
instance MonadAsync m => Monad (ParallelT m) where
    (ParallelT m) >>= f = ParallelT $ parbind par m g
        where g x = getParallelT (f x)
              par = parallel (CtxType Conjunction FIFO)

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (Monad m, Num a) => Num (ParallelT m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (ParallelT m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (ParallelT m a) where
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
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | Zip two AsyncT streams serially using a monadic zipping function.
zipWithM :: Streaming t => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \_ stp yld -> do
        let merge a ra =
                let yield2 b Nothing   = (runStream (g a b)) Nothing stp yld
                    yield2 b (Just rb) =
                        (runStream ((g a b) <> (go ra rb))) Nothing stp yld
                 in (runStream my) Nothing stp yield2
        let yield1 a Nothing   = merge a mempty
            yield1 a (Just ra) = merge a ra
        (runStream mx) Nothing stp yield1
    g a b = toStream $ f a b

-- | Zip two AsyncT streams serially using a pure zipping function.
zipWith :: Streaming t => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \_ stp yld -> do
        let merge a ra =
                let yield2 b Nothing   = yld (f a b) Nothing
                    yield2 b (Just rb) = yld (f a b) (Just (go ra rb))
                 in (runStream my) Nothing stp yield2
        let yield1 a Nothing   = merge a mempty
            yield1 a (Just ra) = merge a ra
        (runStream mx) Nothing stp yield1

-- | Wrapper around AsyncT type with a serial zipping Applicative instance.
-- Note that the binary function interleave (\<=>) is a special case of
-- ZipStream Applicative.
--
-- > f <$> ZipStream xs1 <*> ... <*> ZipStream xsN
newtype ZipStream m a = ZipStream {getZipStream :: Stream m a}
        deriving (Functor)

instance Monad m => Applicative (ZipStream m) where
    pure a = ZipStream $ cycle1 (pure a)
    (<*>) = zipWith id

instance Streaming ZipStream where
    toStream = getZipStream
    fromStream = ZipStream

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
zipAsyncWithM :: (Streaming t, MonadAsync m)
    => (a -> b -> t m c) -> t m a -> t m b -> t m c
zipAsyncWithM f m1 m2 = fromStream $ Stream $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runStream (toStream (zipWithM f ma mb))) Nothing stp yld

-- | Zip two AsyncT streams asyncly (i.e. both the streams are generated
-- concurrently) using a pure zipping function.
zipAsyncWith :: (Streaming t, MonadAsync m)
    => (a -> b -> c) -> t m a -> t m b -> t m c
zipAsyncWith f m1 m2 = fromStream $ Stream $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (runStream (toStream (zipWith f ma mb))) Nothing stp yld

-- | Wrapper around AsyncT type with a parallel zipping Applicative instance.
-- Note that the binary operator (\<|>) from the Alternative instance of AsyncT
-- is a special case of ZipAsync Applicative.
--
-- > f <$> ZipAsync xs1 <*> ... <*> ZipAsync xsN
newtype ZipAsync m a = ZipAsync {getZipAsync :: Stream m a}
        deriving (Functor)

instance MonadAsync m => Applicative (ZipAsync m) where
    pure a = ZipAsync $ cycle1 (pure a)
    (<*>) = zipAsyncWith id

instance Streaming ZipAsync where
    toStream = getZipAsync
    fromStream = ZipAsync

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

-------------------------------------------------------------------------------
-- Type adapting combinators
-------------------------------------------------------------------------------

-- | Adapt one streaming type to another.
adapt :: (Streaming t1, Streaming t2) => t1 m a -> t2 m a
adapt = fromStream . toStream

-- | Interpret an ambiguously typed stream as 'StreamT'.
serially :: StreamT m a -> StreamT m a
serially x = x

-- | Interpret an ambiguously typed stream as 'InterleavedT'.
interleaving :: InterleavedT m a -> InterleavedT m a
interleaving x = x

-- | Interpret an ambiguously typed stream as 'AsyncT'.
asyncly :: AsyncT m a -> AsyncT m a
asyncly x = x

-- | Interpret an ambiguously typed stream as 'ParallelT'.
parallely :: ParallelT m a -> ParallelT m a
parallely x = x

-- | Interpret an ambiguously typed stream as 'ZipStream'.
zipping :: ZipStream m a -> ZipStream m a
zipping x = x

-- | Interpret an ambiguously typed stream as 'ZipAsync'.
zippingAsync :: ZipAsync m a -> ZipAsync m a
zippingAsync x = x

------------------------------------------------------------------------------
-- Construct and deconstruct
------------------------------------------------------------------------------

-- | Decompose a stream into its head and tail. If the stream is empty, returns
-- 'Nothing'. If the stream is non-empty, returns 'Just (a, ma)', where 'a' is
-- the head of the stream and 'ma' its tail.
uncons :: (Streaming t, Monad m, Monoid (t m a))
    => t m a -> m (Maybe (a, t m a))
uncons m = (runStream (toStream m)) Nothing stop yield
    where
    stop = return Nothing

    {-# INLINE yield #-}
    yield a Nothing  = return (Just (a, mempty))
    yield a (Just x) = return (Just (a, (fromStream x)))

-- | Build a Stream by unfolding steps starting from a seed.
unfoldr :: (Streaming t, Monad m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldr step = fromStream . go
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
take :: Streaming t => Int -> t m a -> t m a
take n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp yld -> do
        let yield a Nothing  = yld a Nothing
            yield a (Just x) = yld a (Just (go (n1 - 1) x))
        if (n1 <= 0)
        then stp
        else (runStream m1) ctx stp yield

-- | Discard first 'n' elements from the stream and take the rest.
drop :: Streaming t => Int -> t m a -> t m a
drop n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \ctx stp yld -> do
        let yield _ Nothing  = stp
            yield _ (Just x) = (runStream $ go (n1 - 1) x) ctx stp yld
        if (n1 <= 0)
        then (runStream m1) ctx stp yld
        else (runStream m1) ctx stp yield
