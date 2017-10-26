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
    -- $product
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
import           Control.Monad               (MonadPlus(..), ap, liftM)
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

-- $product
--
-- Different styles of streams dictate the manner in which a stream is
-- processed in monadic or applicative composition. Each style is represented
-- by a different type. The types have the same underlying representation and
-- therefore can be converted back and forth easily to switch the composition
-- style at any point. In other words, processing order of the elements or
-- processing parallelism is decided by the type of the stream.
--
-- 'StreamT' and 'InterleavedT' process the streams serially but differ in the
-- order in which the elements of the streams are processed. 'StreamT' goes
-- depth first that is it combine one element of the first stream to every
-- element of the second stream before it comes back to process th second
-- element of the first stream.  'InterleavedT' takes turns when processing
-- elements from each stream and therefore all iterations finish at the same
-- time rather than one iteration at a time as in the case of 'StreamT'.
-- Depending on the cache behavior or IO behavior 'InterleavedT' might be
-- slightly more efficient than 'StreamT' or vice-versa.
--
-- 'AsyncT' and 'ParallelT' can process the individual elements of the streams
-- in parallel. 'AsyncT' is the parallel version of 'StreamT', it follows the
-- order of 'StreamT' processing multiple elements of a stream concurrently.
-- 'ParallelT' follows the order of 'InterleavedT' processing and interleaving
-- both streams concurrently. Also, note that 'fmap' on these streams would
-- also work in a concurrent fashion i.e. multiple elements in a stream or
-- multiple streams can be mapped concurrently.
--
-- Zipping types have a different style of composition altogether, they zip the
-- streams rather than multiplying them. These types provide only Applicative
-- composition and no Monad instances. 'ZipStream' zips serially i.e. it
-- produces one element from each stream in a serial manner and then zips the
-- two items. 'ZipAsync' on the other hand zips in parallel, it produces one
-- element from each stream concurrently and then zips the two.
--
-- It may help develop a better understanding if we think in terms of stream
-- generation (production) side parallelism versus stream processing
-- (consumption) side parallelism. The sum style operators (e.g. '<|' or '<|>')
-- generate streams in parallel and then combine the results into a unified
-- stream. On the other hand the product style composition (e.g. 'AsyncT' or
-- 'ParallelT' or 'ZipAsync') dictate how the elements of a stream are
-- processed when it is consumed.

------------------------------------------------------------------------------
-- StreamT iterates serially in the Monad and Applicative compositions
------------------------------------------------------------------------------

-- | Note that 'StreamT' style composition works well operationally for
-- infinite streams.  However if the second stream is infinite we will never
-- come back to the second iteration of the first stream.

newtype StreamT m a = StreamT {getStreamT :: Stream m a}
    deriving (Semigroup, Monoid, MonadTrans, MonadIO, MonadThrow)

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
-- Monad
------------------------------------------------------------------------------

-- | Execute a monadic action sequentially for each element in the 'AsyncT'
-- stream, i.e. an iteration finishes completely before the next one starts.
instance Monad m => Monad (StreamT m) where
    return = pure
    (StreamT (Stream m)) >>= f = StreamT $ Stream $ \_ stp yld ->
        let run x = (runStream x) Nothing stp yld
            yield a Nothing  = run $ getStreamT (f a)
            yield a (Just r) = run $ getStreamT (f a)
                                  <> getStreamT (StreamT r >>= f)
        in m Nothing stp yield

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance Monad m => Applicative (StreamT m) where
    pure = StreamT . yielding
    (<*>) = ap

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (StreamT m) where
    fmap = liftM

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
-- compositions.
------------------------------------------------------------------------------

-- | Note that 'InterleavedT' style composition cannot work well operationally
-- on infinite streams. It will keep accumulating the elements of the stream
-- resulting in continuous increase in space consumption.
--
newtype InterleavedT m a = InterleavedT {getInterleavedT :: Stream m a}
    deriving (Semigroup, Monoid, MonadTrans, MonadIO, MonadThrow)

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
    return = pure
    (InterleavedT (Stream m)) >>= f = InterleavedT $ Stream $ \_ stp yld ->
        let run x = (runStream x) Nothing stp yld
            yield a Nothing  = run $ getInterleavedT (f a)
            yield a (Just r) = run $ getInterleavedT (f a)
                                     `interleave`
                                     getInterleavedT (InterleavedT r >>= f)
        in m Nothing stp yield

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance Monad m => Applicative (InterleavedT m) where
    pure = InterleavedT . yielding
    (<*>) = ap

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (InterleavedT m) where
    fmap = liftM

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

-- | Just like 'StreamT', 'AsyncT' style composition works well operationally
-- for infinite streams.  However, if the second stream is infinite we will
-- never be able to finish the first stream.
--
-- Note that 'fmap' on 'AsyncT' would map on multiple elements concurrently.

newtype AsyncT m a = AsyncT {getAsyncT :: Stream m a}
    deriving (Semigroup, Monoid, MonadTrans)

deriving instance MonadAsync m => Alternative (AsyncT m)
deriving instance MonadAsync m => MonadPlus (AsyncT m)
deriving instance MonadAsync m => MonadIO (AsyncT m)
deriving instance MonadAsync m => MonadThrow (AsyncT m)
deriving instance (MonadBase b m, MonadAsync m) => MonadBase b (AsyncT m)
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
    return = pure
    (AsyncT m) >>= f = AsyncT $ parbind par m g
        where g x = getAsyncT (f x)
              par = parallel (CtxType Conjunction LIFO)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance MonadAsync m => Applicative (AsyncT m) where
    pure = AsyncT . yielding
    (<*>) = ap

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance MonadAsync m => Functor (AsyncT m) where
    fmap = liftM

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (MonadAsync m, Num a) => Num (AsyncT m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (MonadAsync m, Fractional a) => Fractional (AsyncT m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (MonadAsync m, Floating a) => Floating (AsyncT m a) where
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
-- compositions. Iterations are scheduled in a round-robin fashion.
------------------------------------------------------------------------------

-- | Note that, just like 'InterleavedT', 'ParallelT' style composition cannot
-- work well operationally on infinite streams. It will keep accumulating the
-- elements of the stream resulting in continuous increase in space
-- consumption.
--
-- Note that 'fmap' on 'ParallelT' would map on both streams concurrently.
--
newtype ParallelT m a = ParallelT {getParallelT :: Stream m a}
    deriving (Semigroup, Monoid, MonadTrans)

deriving instance MonadAsync m => Alternative (ParallelT m)
deriving instance MonadAsync m => MonadPlus (ParallelT m)
deriving instance MonadAsync m => MonadIO (ParallelT m)
deriving instance MonadAsync m => MonadThrow (ParallelT m)
deriving instance (MonadBase b m, MonadAsync m) => MonadBase b (ParallelT m)
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
    return = pure
    (ParallelT m) >>= f = ParallelT $ parbind par m g
        where g x = getParallelT (f x)
              par = parallel (CtxType Conjunction FIFO)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance MonadAsync m => Applicative (ParallelT m) where
    pure = ParallelT . yielding
    (<*>) = ap

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance MonadAsync m => Functor (ParallelT m) where
    fmap = liftM

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (MonadAsync m, Num a) => Num (ParallelT m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (MonadAsync m, Fractional a) => Fractional (ParallelT m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (MonadAsync m, Floating a) => Floating (ParallelT m a) where
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

-- | 'ZipStream' zips serially i.e. it produces one element from each stream in
-- a serial manner and then zips the two elements.
newtype ZipStream m a = ZipStream {getZipStream :: Stream m a}
        deriving (Semigroup, Monoid)

deriving instance MonadAsync m => Alternative (ZipStream m)

instance Monad m => Functor (ZipStream m) where
    fmap f (ZipStream (Stream m)) = ZipStream $ Stream $ \_ stp yld ->
        let yield a Nothing  = yld (f a) Nothing
            yield a (Just r) = yld (f a)
                               (Just (getZipStream (fmap f (ZipStream r))))
        in m Nothing stp yield

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
-- Parallely Zipping Streams
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

-- | 'ZipAsync' zips in parallel, it produces one element from each stream
-- concurrently and then zips the two.
newtype ZipAsync m a = ZipAsync {getZipAsync :: Stream m a}
        deriving (Semigroup, Monoid)

deriving instance MonadAsync m => Alternative (ZipAsync m)

instance Monad m => Functor (ZipAsync m) where
    fmap f (ZipAsync (Stream m)) = ZipAsync $ Stream $ \_ stp yld ->
        let yield a Nothing  = yld (f a) Nothing
            yield a (Just r) = yld (f a)
                               (Just (getZipAsync (fmap f (ZipAsync r))))
        in m Nothing stp yield

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
