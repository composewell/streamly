{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Asyncly.Streams
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Asyncly.Streams
    (
    -- * Product Style Composition
    -- $product
      Streaming (..)
    , MonadAsync
    , EndOfStream (..)
    , SVar

    -- * Stream Styles
    , StreamT
    , InterleavedT
    , AsyncT
    , ParallelT
    , ZipStream
    , ZipAsync

    -- * Type Adapters
    , serially
    , interleaving
    , asyncly
    , parallely
    , zipping
    , zippingAsync
    , adapt

    -- * Construction
    , streamBuild
    , fromCallback
    , fromSVar

    -- * Elimination
    , streamFold
    , runStreaming
    , toSVar

    -- * Running Streams
    , runStreamT
    , runInterleavedT
    , runAsyncT
    , runParallelT
    , runZipStream
    , runZipAsync

    -- * Transformation
    , async

    -- * Zipping
    , zipWith
    , zipAsyncWith

    -- * Sum Style Composition
    , (<=>)
    , (<|)

    -- * Fold Utilities
    , foldWith
    , foldMapWith
    , forEachWith
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
import           Data.Semigroup              (Semigroup(..))
import           Prelude hiding              (drop, take, zipWith)
import           Asyncly.Core

-- $product
--
-- Different styles of streams represented by different types dictate the
-- manner in which a stream is processed in monadic or applicative composition.
-- Each type differs in the processing order of the elements or processing
-- parallelism.  The types have the same underlying representation and
-- therefore can be converted back and forth easily to switch the composition
-- style at any point.
--
-- 'StreamT' and 'InterleavedT' process the streams serially but differ in the
-- order in which the elements of the streams are processed. 'StreamT' goes
-- depth first that is it combine one element of the first stream to every
-- element of the second stream before it comes back to process the second
-- element of the first stream.  'InterleavedT' takes turns when processing
-- elements from each stream and therefore all iterations finish at the same
-- time rather than one iteration at a time as in the case of 'StreamT'.
-- Depending on the cache behavior or IO behavior 'InterleavedT' might be
-- slightly more efficient than 'StreamT' or vice-versa.
--
-- 'AsyncT' and 'ParallelT' can process the individual elements of the streams
-- in parallel. 'AsyncT' is the parallel version of 'StreamT', it follows the
-- order of 'StreamT', processing multiple elements of a stream concurrently.
-- 'ParallelT' follows the order of 'InterleavedT', processing and interleaving
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
-- Types that can behave as a Stream
------------------------------------------------------------------------------

-- | Class of types that can represent a stream of elements of some type 'a' in
-- some monad 'm'.
class Streaming t where
    toStream :: t m a -> Stream m a
    fromStream :: Stream m a -> t m a

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
    pure = StreamT . yields
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
    pure = InterleavedT . yields
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
              par = parallel (SVarStyle Conjunction LIFO)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance MonadAsync m => Applicative (AsyncT m) where
    pure = AsyncT . yields
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
              par = parallel (SVarStyle Conjunction FIFO)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance MonadAsync m => Applicative (ParallelT m) where
    pure = ParallelT . yields
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

-- | Zip two streams serially using a pure zipping function.
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
    pure = ZipStream . yields
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

-- | Zip two streams asyncly (i.e. both the elements being zipped are generated
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
    pure = ZipAsync . yields
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
-- Constructing a stream
------------------------------------------------------------------------------

-- XXX Need to accept an SVar as well
-- | Build a stream from its church encoding.  The function passed maps
-- directly to the underlying representation of the stream type.
streamBuild :: Streaming t
    => (forall r. (a -> Maybe (t m a) -> m r) -> m r -> m r) -> t m a
streamBuild k = fromStream $ Stream $ \_ stp yld ->
    let yield a Nothing = yld a Nothing
        yield a (Just r) = yld a (Just (toStream r))
     in k yield stp

-- | Convert a callback into a stream.
fromCallback :: (Streaming t) => (forall r. (a -> m r) -> m r) -> t m a
fromCallback k = fromStream $ Stream $ \_ _ yld -> k (\a -> yld a Nothing)

-- | Convert an SVar to a stream. Throws 'EndOfStream' if the SVar is accessed
-- even after it has been fully drained.
fromSVar :: (MonadAsync m, Streaming t) => SVar m a -> t m a
fromSVar sv = fromStream $ streamSVar sv

------------------------------------------------------------------------------
-- Destroying a stream
------------------------------------------------------------------------------

-- | Fold a stream using its church encoding. The first argument is the
-- "step" function consuming one element and the rest of the stream. The second
-- argument is the "done" function that is called when the stream is over.
streamFold :: Streaming t => (a -> Maybe (t m a) -> m r) -> m r -> t m a -> m r
streamFold step done m =
    let yield a Nothing = step a Nothing
        yield a (Just x) = step a (Just (fromStream x))
     in (runStream (toStream m)) Nothing done yield

-- | Run a streaming composition, discard the results.
runStreaming :: (Monad m, Streaming t) => t m a -> m ()
runStreaming m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            yield _ Nothing  = stop
            yield _ (Just x) = go x
         in (runStream m1) Nothing stop yield

-- | Convert a stream to an SVar, thus making it asynchronous.
toSVar :: (Streaming t, MonadAsync m) => t m a -> m (SVar m a)
toSVar m = newSVar1 (SVarStyle Disjunction LIFO) (toStream m)

-------------------------------------------------------------------------------
-- Running Streams, convenience functions specialized to types
-------------------------------------------------------------------------------

-- | Run a 'StreamT' computation. Same as @runStreaming . serially@.
runStreamT :: Monad m => StreamT m a -> m ()
runStreamT = runStreaming

-- | Run an 'InterleavedT' computation. Same as @runStreaming . interleaving@.
runInterleavedT :: Monad m => InterleavedT m a -> m ()
runInterleavedT = runStreaming

-- | Run an 'AsyncT' computation. Same as @runStreaming . asyncly@.
runAsyncT :: Monad m => AsyncT m a -> m ()
runAsyncT = runStreaming

-- | Run a 'ParallelT' computation. Same as @runStreaming . parallely@.
runParallelT :: Monad m => ParallelT m a -> m ()
runParallelT = runStreaming

-- | Run a 'ZipStream' computation. Same as @runStreaming . zipping@.
runZipStream :: Monad m => ZipStream m a -> m ()
runZipStream = runStreaming

-- | Run a 'ZipAsync' computation. Same as @runStreaming . zippingAsync@.
runZipAsync :: Monad m => ZipAsync m a -> m ()
runZipAsync = runStreaming

------------------------------------------------------------------------------
-- Transformation
------------------------------------------------------------------------------

-- XXX Get rid of this?
-- | Make a stream asynchronous, triggers the computation and returns a stream
-- in the underlying monad representing the output generated by the original
-- computation. The returned action must be drained once and only once. If not
-- drained fully we may have a thread blocked forever and once drained it will
-- throw an 'EndOfStream' exception if we try to run it again

async :: (Streaming t, MonadAsync m) => t m a -> m (t m a)
async m = toSVar m >>= return . fromSVar

------------------------------------------------------------------------------
-- Sum Style Composition
------------------------------------------------------------------------------

infixr 5 <=>

-- | Sequential interleaved composition, in contrast to '<>' this operator
-- fairly interleaves the two streams instead of appending them; yielding one
-- element from each stream alternately. Unlike '<>' it cannot be used to fold
-- an infinite container of streams.
{-# INLINE (<=>) #-}
(<=>) :: Streaming t => t m a -> t m a -> t m a
m1 <=> m2 = fromStream $ interleave (toStream m1) (toStream m2)

-- | Parallel interleaved composition, in contrast to '<|>' this operator
-- "merges" streams in a left biased manner rather than fairly interleaving
-- them.  It keeps yielding from the stream on the left as long as it can. If
-- the left stream blocks or cannot keep up with the pace of the consumer it
-- can yield from the stream on the right in parallel.  Unlike '<|>' it can be
-- used to fold infinite containers of streams.
{-# INLINE (<|) #-}
(<|) :: (Streaming t, MonadAsync m) => t m a -> t m a -> t m a
m1 <| m2 = fromStream $ parLeft (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Fold Utilities
------------------------------------------------------------------------------

-- | Fold a 'Foldable' container using the given function.
{-# INLINABLE foldWith #-}
foldWith :: (Monoid b, Foldable t) => (a -> b -> b) -> t a -> b
foldWith f = foldr f mempty

-- | Fold a 'Foldable' container using a function that is a composition of the
-- two arguments.
{-# INLINABLE foldMapWith #-}
foldMapWith :: (Monoid b, Foldable t) =>
    (b1 -> b -> b) -> (a -> b1) -> t a -> b
foldMapWith f g = foldr (f . g) mempty

-- | Fold a 'Foldable' container using a function that is a composition of the
-- first and the third argument.
{-# INLINABLE forEachWith #-}
forEachWith :: (Monoid b, Foldable t) =>
    (b1 -> b -> b) -> t a -> (a -> b1) -> b
forEachWith f xs g = foldr (f . g) mempty xs
