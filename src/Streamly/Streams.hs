{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Streams
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Streams
    (
      Streaming (..)
    , MonadAsync

    -- * SVars
    , SVarSched (..)
    , SVarTag (..)
    , SVarStyle (..)
    , SVar
    , newEmptySVar

    -- * Construction
    , nil
    , cons
    , (.:)
    , streamBuild
    , fromCallback
    , fromSVar

    -- * Elimination
    , streamFold
    , runStreaming
    , toSVar

    -- * Transformation
    , async

    -- * Stream Styles
    , StreamT
    , ReverseT
    , InterleavedT
    , AsyncT
    , ParallelT
    , ZipStream
    , ZipAsync

    -- * Type Adapters
    , serially
    , reversely
    , interleaving
    , asyncly
    , parallely
    , zipping
    , zippingAsync
    , adapt

    -- * Running Streams
    , runStreamT
    , runReverseT
    , runInterleavedT
    , runAsyncT
    , runParallelT
    , runZipStream
    , runZipAsync

    -- * Zipping
    , zipWith
    , zipAsyncWith

    -- * Sum Style Composition
    , (<=>)
    , (<|)

    -- * Fold Utilities
    -- $foldutils
    , foldWith
    , foldMapWith
    , forEachWith
    )
where

import           Control.Applicative         (Alternative (..), liftA2)
import           Control.Monad               (MonadPlus(..), ap)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.Error.Class   (MonadError(..))
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.Reader.Class  (MonadReader(..))
import           Control.Monad.State.Class   (MonadState(..))
import           Control.Monad.Trans.Class   (MonadTrans)
import           Data.Semigroup              (Semigroup(..))
import           Prelude hiding              (zipWith)
import           Streamly.Core

------------------------------------------------------------------------------
-- Types that can behave as a Stream
------------------------------------------------------------------------------

-- | Class of types that can represent a stream of elements of some type 'a' in
-- some monad 'm'.
class Streaming t where
    toStream :: t m a -> Stream m a
    fromStream :: Stream m a -> t m a

------------------------------------------------------------------------------
-- Constructing a stream
------------------------------------------------------------------------------

-- | Represesnts an empty stream just like @[]@ represents an empty list.
nil :: Streaming t => t m a
nil = fromStream snil

infixr 5 `cons`

-- | Constructs a stream by adding a pure value at the head of an existing
-- stream, just like ':' constructs lists. For example:
--
-- @
-- > let stream = 1 \`cons` 2 \`cons` 3 \`cons` nil
-- > (toList . serially) stream
-- [1,2,3]
-- @
cons :: (Streaming t) => a -> t m a -> t m a
cons a r = fromStream $ scons a (Just (toStream r))

infixr 5 .:

-- | Operator equivalent of 'cons' so that you can construct a stream of pure
-- values more succinctly like this:
--
-- @
-- > let stream = 1 .: 2 .: 3 .: nil
-- > (toList . serially) stream
-- [1,2,3]
-- @
--
-- '.:' constructs a stream just like ':' constructs a list.
--
-- Also note that another equivalent way of building streams from pure values
-- is:
--
-- @
-- > let stream = pure 1 <> pure 2 <> pure 3
-- > (toList . serially) stream
-- [1,2,3]
-- @
--
-- In the first method we construct a stream by adding one element at a time.
-- In the second method we first construct singleton streams using 'pure' and
-- then compose all those streams together using the 'Semigroup' style
-- composition of streams. The former method is a bit more efficient than the
-- latter.
--
(.:) :: (Streaming t) => a -> t m a -> t m a
(.:) = cons

-- | Build a stream from its church encoding.  The function passed maps
-- directly to the underlying representation of the stream type. The second
-- parameter to the function is the "yield" function yielding a value and the
-- remaining stream if any otherwise 'Nothing'. The third parameter is to
-- represent an "empty" stream.
streamBuild :: Streaming t
    => (forall r. Maybe (SVar m a)
        -> (a -> Maybe (t m a) -> m r)
        -> m r
        -> m r)
    -> t m a
streamBuild k = fromStream $ Stream $ \sv stp yld ->
    let yield a Nothing = yld a Nothing
        yield a (Just r) = yld a (Just (toStream r))
     in k sv yield stp

-- | Build a singleton stream from a callback function.
fromCallback :: (Streaming t) => (forall r. (a -> m r) -> m r) -> t m a
fromCallback k = fromStream $ Stream $ \_ _ yld -> k (\a -> yld a Nothing)

-- | Read an SVar to get a stream.
fromSVar :: (MonadAsync m, Streaming t) => SVar m a -> t m a
fromSVar sv = fromStream $ fromStreamVar sv

------------------------------------------------------------------------------
-- Destroying a stream
------------------------------------------------------------------------------

-- | Fold a stream using its church encoding. The second argument is the "step"
-- function consuming an element and the remaining stream, if any. The third
-- argument is for consuming an "empty" stream that yields nothing.
streamFold :: Streaming t
    => Maybe (SVar m a) -> (a -> Maybe (t m a) -> m r) -> m r -> t m a -> m r
streamFold sv step blank m =
    let yield a Nothing = step a Nothing
        yield a (Just x) = step a (Just (fromStream x))
     in (runStream (toStream m)) sv blank yield

-- | Run a streaming composition, discard the results.
runStreaming :: (Monad m, Streaming t) => t m a -> m ()
runStreaming m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            yield _ Nothing  = stop
            yield _ (Just x) = go x
         in (runStream m1) Nothing stop yield

-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
toSVar :: (Streaming t, MonadAsync m) => SVar m a -> t m a -> m ()
toSVar sv m = toStreamVar sv (toStream m)

------------------------------------------------------------------------------
-- Transformation
------------------------------------------------------------------------------

-- XXX Get rid of this?
-- | Make a stream asynchronous, triggers the computation and returns a stream
-- in the underlying monad representing the output generated by the original
-- computation. The returned action is exhaustible and must be drained once. If
-- not drained fully we may have a thread blocked forever and once exhausted it
-- will always return 'empty'.

async :: (Streaming t, MonadAsync m) => t m a -> m (t m a)
async m = do
    sv <- newStreamVar1 (SVarStyle Disjunction LIFO) (toStream m)
    return $ fromSVar sv

------------------------------------------------------------------------------
-- StreamT
------------------------------------------------------------------------------

-- | The 'Monad' instance of 'StreamT' runs the /monadic continuation/ for each
-- element of the stream, serially.
--
-- @
-- main = 'runStreamT' $ do
--     x <- return 1 \<\> return 2
--     liftIO $ print x
-- @
-- @
-- 1
-- 2
-- @
--
-- 'StreamT' nests streams serially in a depth first manner.
--
-- @
-- main = 'runStreamT' $ do
--     x <- return 1 \<\> return 2
--     y <- return 3 \<\> return 4
--     liftIO $ print (x, y)
-- @
-- @
-- (1,3)
-- (1,4)
-- (2,3)
-- (2,4)
-- @
--
-- This behavior is exactly like a list transformer. We call the monadic code
-- being run for each element of the stream a monadic continuation. In
-- imperative paradigm we can think of this composition as nested @for@ loops
-- and the monadic continuation is the body of the loop. The loop iterates for
-- all elements of the stream.
--
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
    pure = StreamT . singleton
    (<*>) = ap

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (StreamT m) where
    fmap f (StreamT (Stream m)) = StreamT $ Stream $ \_ stp yld ->
        let yield a Nothing  = yld (f a) Nothing
            yield a (Just r) = yld (f a)
                               (Just (getStreamT (fmap f (StreamT r))))
        in m Nothing stp yield

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
-- ReverseT
------------------------------------------------------------------------------

-- TODO: documenation

newtype ReverseT m a = ReverseT {getReverseT :: Stream m a}
    deriving (Semigroup, Monoid, MonadTrans, MonadIO, MonadThrow)

deriving instance MonadAsync m => Alternative (ReverseT m)
deriving instance MonadAsync m => MonadPlus (ReverseT m)
deriving instance (MonadBase b m, Monad m) => MonadBase b (ReverseT m)
deriving instance MonadError e m => MonadError e (ReverseT m)
deriving instance MonadReader r m => MonadReader r (ReverseT m)
deriving instance MonadState s m => MonadState s (ReverseT m)

instance Streaming ReverseT where
    toStream = getReverseT
    fromStream = ReverseT

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (ReverseT m) where
     return = pure
     s >>= f = ReverseT . roundrobin . getReverseT $ fmap (getReverseT . f) s

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance Monad m => Applicative (ReverseT m) where
    pure = ReverseT . singleton
    (<*>) = ap

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (ReverseT m) where
    fmap f (ReverseT (Stream m)) = ReverseT $ Stream $ \_ stp yld ->
        let yield a Nothing  = yld (f a) Nothing
            yield a (Just r) = yld (f a)
                               (Just (getReverseT (fmap f (ReverseT r))))
        in m Nothing stp yield

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (Monad m, Num a) => Num (ReverseT m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (ReverseT m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (ReverseT m a) where
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
-- InterleavedT
------------------------------------------------------------------------------

-- | Like 'StreamT' but different in nesting behavior. It fairly interleaves
-- the iterations of the inner and the outer loop, nesting loops in a breadth
-- first manner.
--
--
-- @
-- main = 'runInterleavedT' $ do
--     x <- return 1 \<\> return 2
--     y <- return 3 \<\> return 4
--     liftIO $ print (x, y)
-- @
-- @
-- (1,3)
-- (2,3)
-- (1,4)
-- (2,4)
-- @
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
    pure = InterleavedT . singleton
    (<*>) = ap

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (InterleavedT m) where
    fmap f (InterleavedT (Stream m)) = InterleavedT $ Stream $ \_ stp yld ->
        let yield a Nothing  = yld (f a) Nothing
            yield a (Just r) =
                yld (f a) (Just (getInterleavedT (fmap f (InterleavedT r))))
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
-- AsyncT
------------------------------------------------------------------------------

-- | Like 'StreamT' but /may/ run each iteration concurrently using demand
-- driven concurrency.  More concurrent iterations are started only if the
-- previous iterations are not able to produce enough output for the consumer.
--
-- @
-- import "Streamly"
-- import Control.Concurrent
--
-- main = 'runAsyncT' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     liftIO $ do
--          threadDelay (n * 1000000)
--          myThreadId >>= \\tid -> putStrLn (show tid ++ ": Delay " ++ show n)
-- @
-- @
-- ThreadId 40: Delay 1
-- ThreadId 39: Delay 2
-- ThreadId 38: Delay 3
-- @
--
-- All iterations may run in the same thread if they do not block.
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
parbind par m f = go m
    where
        go (Stream g) =
            Stream $ \ctx stp yld ->
            let run x = (runStream x) ctx stp yld
                yield a Nothing  = run $ f a
                yield a (Just r) = run $ f a `par` go r
            in g Nothing stp yield

instance MonadAsync m => Monad (AsyncT m) where
    return = pure
    (AsyncT m) >>= f = AsyncT $ parbind par m g
        where g x = getAsyncT (f x)
              par = joinStreamVar2 (SVarStyle Conjunction LIFO)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance MonadAsync m => Applicative (AsyncT m) where
    pure = AsyncT . singleton
    (<*>) = ap

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (AsyncT m) where
    fmap f (AsyncT (Stream m)) = AsyncT $ Stream $ \_ stp yld ->
        let yield a Nothing  = yld (f a) Nothing
            yield a (Just r) = yld (f a) (Just (getAsyncT (fmap f (AsyncT r))))
        in m Nothing stp yield

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
-- ParallelT
------------------------------------------------------------------------------

-- | Like 'StreamT' but runs /all/ iterations fairly concurrently using a round
-- robin scheduling.
--
-- @
-- import "Streamly"
-- import Control.Concurrent
--
-- main = 'runParallelT' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     liftIO $ do
--          threadDelay (n * 1000000)
--          myThreadId >>= \\tid -> putStrLn (show tid ++ ": Delay " ++ show n)
-- @
-- @
-- ThreadId 40: Delay 1
-- ThreadId 39: Delay 2
-- ThreadId 38: Delay 3
-- @
--
-- Unlike 'AsyncT' all iterations are guaranteed to run fairly concurrently,
-- unconditionally.
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

instance MonadAsync m => Monad (ParallelT m) where
    return = pure
    (ParallelT m) >>= f = ParallelT $ parbind par m g
        where g x = getParallelT (f x)
              par = joinStreamVar2 (SVarStyle Conjunction FIFO)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance MonadAsync m => Applicative (ParallelT m) where
    pure = ParallelT . singleton
    (<*>) = ap

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (ParallelT m) where
    fmap f (ParallelT (Stream m)) = ParallelT $ Stream $ \_ stp yld ->
        let yield a Nothing  = yld (f a) Nothing
            yield a (Just r) = yld (f a)
                                   (Just (getParallelT (fmap f (ParallelT r))))
        in m Nothing stp yield

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
        let yield1 a Nothing   = merge a snil
            yield1 a (Just ra) = merge a ra
        (runStream mx) Nothing stp yield1

-- | 'ZipStream' zips serially i.e. it produces one element from each stream
-- serially and then zips the two elements. Note, for convenience we have used
-- the 'zipping' combinator in the following example instead of using a type
-- annotation.
--
-- @
-- main = (toList . 'zipping' $ (,) \<$\> s1 \<*\> s2) >>= print
--     where s1 = pure 1 <> pure 2
--           s2 = pure 3 <> pure 4
-- @
-- @
-- [(1,3),(2,4)]
-- @
--
-- This applicative operation can be seen as the zipping equivalent of
-- interleaving with '<=>'.
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
    pure = ZipStream . srepeat
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

-- | Like 'ZipStream' but zips in parallel, it generates both the elements to
-- be zipped concurrently.
--
-- @
-- main = (toList . 'zippingAsync' $ (,) \<$\> s1 \<*\> s2) >>= print
--     where s1 = pure 1 <> pure 2
--           s2 = pure 3 <> pure 4
-- @
-- @
-- [(1,3),(2,4)]
-- @
--
-- This applicative operation can be seen as the zipping equivalent of
-- parallel composition with '<|>'.
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
    pure = ZipAsync . srepeat
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

-- | Interpret an ambiguously typed stream as 'ReverseT'.
reversely :: ReverseT m a -> ReverseT m a
reversely x = x

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

-------------------------------------------------------------------------------
-- Running Streams, convenience functions specialized to types
-------------------------------------------------------------------------------

-- | Same as @runStreaming . serially@.
runStreamT :: Monad m => StreamT m a -> m ()
runStreamT = runStreaming

-- | Same as @runStreaming . reversely@.
runReverseT :: Monad m => ReverseT m a -> m ()
runReverseT = runStreaming

-- | Same as @runStreaming . interleaving@.
runInterleavedT :: Monad m => InterleavedT m a -> m ()
runInterleavedT = runStreaming

-- | Same as @runStreaming . asyncly@.
runAsyncT :: Monad m => AsyncT m a -> m ()
runAsyncT = runStreaming

-- | Same as @runStreaming . parallely@.
runParallelT :: Monad m => ParallelT m a -> m ()
runParallelT = runStreaming

-- | Same as @runStreaming . zipping@.
runZipStream :: Monad m => ZipStream m a -> m ()
runZipStream = runStreaming

-- | Same as @runStreaming . zippingAsync@.
runZipAsync :: Monad m => ZipAsync m a -> m ()
runZipAsync = runStreaming

------------------------------------------------------------------------------
-- Sum Style Composition
------------------------------------------------------------------------------

infixr 5 <=>

-- | Sequential interleaved composition, in contrast to '<>' this operator
-- fairly interleaves two streams instead of appending them; yielding one
-- element from each stream alternately.
--
-- @
-- main = ('toList' . 'serially' $ (return 1 <> return 2) \<=\> (return 3 <> return 4)) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- This operator corresponds to the 'InterleavedT' style. Unlike '<>', this
-- operator cannot be used to fold infinite containers since that might
-- accumulate too many partially drained streams.  To be clear, it can combine
-- infinite streams but not infinite number of streams.
{-# INLINE (<=>) #-}
(<=>) :: Streaming t => t m a -> t m a -> t m a
m1 <=> m2 = fromStream $ interleave (toStream m1) (toStream m2)

-- | Demand driven concurrent composition. In contrast to '<|>' this operator
-- concurrently "merges" streams in a left biased manner rather than fairly
-- interleaving them.  It keeps yielding from the stream on the left as long as
-- it can. If the left stream blocks or cannot keep up with the pace of the
-- consumer it can concurrently yield from the stream on the right in parallel.
--
-- @
-- main = ('toList' . 'serially' $ (return 1 <> return 2) \<| (return 3 <> return 4)) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- Unlike '<|>' it can be used to fold infinite containers of streams. This
-- operator corresponds to the 'AsyncT' type for product style composition.
--
{-# INLINE (<|) #-}
(<|) :: (Streaming t, MonadAsync m) => t m a -> t m a -> t m a
m1 <| m2 = fromStream $ parLeft (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Fold Utilities
------------------------------------------------------------------------------

-- $foldutils
-- These utilities are designed to pass the first argument as one of the sum
-- style composition operators (i.e. '<>', '<=>', '<|', '<|>') to conveniently
-- fold a container using any style of stream composition.

-- | Like the 'Prelude' 'fold' but allows you to specify a binary sum style
-- stream composition operator to fold a container of streams.
--
-- @foldWith (<>) $ map return [1..3]@
{-# INLINABLE foldWith #-}
foldWith :: (Streaming t, Foldable f)
    => (t m a -> t m a -> t m a) -> f (t m a) -> t m a
foldWith f = foldr f nil

-- | Like 'foldMap' but allows you to specify a binary sum style composition
-- operator to fold a container of streams. Maps a monadic streaming action on
-- the container before folding it.
--
-- @foldMapWith (<>) return [1..3]@
{-# INLINABLE foldMapWith #-}
foldMapWith :: (Streaming t, Foldable f)
    => (t m b -> t m b -> t m b) -> (a -> t m b) -> f a -> t m b
foldMapWith f g = foldr (f . g) nil

-- | Like 'foldMapWith' but with the last two arguments reversed i.e. the
-- monadic streaming function is the last argument.
{-# INLINABLE forEachWith #-}
forEachWith :: (Streaming t, Foldable f)
    => (t m b -> t m b -> t m b) -> f a -> (a -> t m b) -> t m b
forEachWith f xs g = foldr (f . g) nil xs
