{-# LANGUAGE ConstraintKinds           #-}
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
      IsStream (..)
    , Streaming         -- deprecated
    , MonadAsync

    -- * SVars
    , SVarSched (..)
    , SVarTag (..)
    , SVarStyle (..)
    , SVar
    , S.newEmptySVar

    -- * Construction
    , nil
    , cons
    , (.:)
    , streamBuild
    , fromCallback
    , fromSVar

    -- * Elimination
    , streamFold
    , runStream
    , runStreaming      -- deprecated
    , toSVar

    -- * Transformation
    , async

    -- * Merging Streams
    , append
    , interleave
    , asyncmerge
    , parmerge
    , (<=>)            --deprecated
    , (<|)             --deprecated

    -- * Stream Styles
    , SerialT
    , StreamT           -- deprecated
    , InterleavedT
    , AsyncT
    , ParallelT
    , ZipSerial
    , ZipStream         -- deprecated
    , ZipAsync

    -- * Type Adapters
    , serially
    , interleaving
    , asyncly
    , parallely
    , zipping
    , zippingAsync
    , adapt

    -- * Running Streams
    , runSerialT
    , runStreamT       -- deprecated
    , runInterleavedT
    , runAsyncT
    , runParallelT
    , runZipSerial
    , runZipStream     -- deprecated
    , runZipAsync

    -- * Zipping
    , zipWith
    , zipAsyncWith

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
import           Streamly.Core               ( MonadAsync, Stream(Stream)
                                             , SVar, SVarStyle(..)
                                             , SVarTag(..), SVarSched(..))
import qualified Streamly.Core as S

------------------------------------------------------------------------------
-- Types that can behave as a Stream
------------------------------------------------------------------------------

-- | Class of types that can represent a stream of elements of some type 'a' in
-- some monad 'm'.
class IsStream t where
    toStream :: t m a -> Stream m a
    fromStream :: Stream m a -> t m a

-- | Same as 'IsStream'.
{-# Deprecated Streaming "Please use IsStream instead." #-}
type Streaming = IsStream

------------------------------------------------------------------------------
-- Constructing a stream
------------------------------------------------------------------------------

-- | Represesnts an empty stream just like @[]@ represents an empty list.
nil :: IsStream t => t m a
nil = fromStream S.nil

infixr 5 `cons`

-- | Constructs a stream by adding a pure value at the head of an existing
-- stream, just like ':' constructs lists. For example:
--
-- @
-- > let stream = 1 \`cons` 2 \`cons` 3 \`cons` nil
-- > (toList . serially) stream
-- [1,2,3]
-- @
cons :: IsStream t => a -> t m a -> t m a
cons a r = fromStream $ S.cons a (Just (toStream r))

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
(.:) :: IsStream t => a -> t m a -> t m a
(.:) = cons

-- | Build a stream from its church encoding.  The function passed maps
-- directly to the underlying representation of the stream type. The second
-- parameter to the function is the "yield" function yielding a value and the
-- remaining stream if any otherwise 'Nothing'. The third parameter is to
-- represent an "empty" stream.
streamBuild :: IsStream t
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
fromCallback :: IsStream t => (forall r. (a -> m r) -> m r) -> t m a
fromCallback k = fromStream $ Stream $ \_ _ yld -> k (\a -> yld a Nothing)

-- | Read an SVar to get a stream.
fromSVar :: (MonadAsync m, IsStream t) => SVar m a -> t m a
fromSVar sv = fromStream $ S.fromStreamVar sv

------------------------------------------------------------------------------
-- Destroying a stream
------------------------------------------------------------------------------

-- | Fold a stream using its church encoding. The second argument is the "step"
-- function consuming an element and the remaining stream, if any. The third
-- argument is for consuming an "empty" stream that yields nothing.
streamFold :: IsStream t
    => Maybe (SVar m a) -> (a -> Maybe (t m a) -> m r) -> m r -> t m a -> m r
streamFold sv step blank m =
    let yield a Nothing = step a Nothing
        yield a (Just x) = step a (Just (fromStream x))
     in (S.runStream (toStream m)) sv blank yield

-- | Run a streaming composition, discard the results.
runStream :: (Monad m, IsStream t) => t m a -> m ()
runStream m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            yield _ Nothing  = stop
            yield _ (Just x) = go x
         in (S.runStream m1) Nothing stop yield

-- | Same as 'runStream'
{-# Deprecated runStreaming "Please use runStream instead." #-}
runStreaming :: (Monad m, IsStream t) => t m a -> m ()
runStreaming = runStream

-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
toSVar :: (IsStream t, MonadAsync m) => SVar m a -> t m a -> m ()
toSVar sv m = S.toStreamVar sv (toStream m)

------------------------------------------------------------------------------
-- Transformation
------------------------------------------------------------------------------

-- XXX Get rid of this?
-- | Make a stream asynchronous, triggers the computation and returns a stream
-- in the underlying monad representing the output generated by the original
-- computation. The returned action is exhaustible and must be drained once. If
-- not drained fully we may have a thread blocked forever and once exhausted it
-- will always return 'empty'.

async :: (IsStream t, MonadAsync m) => t m a -> m (t m a)
async m = do
    sv <- S.newStreamVar1 (SVarStyle Disjunction LIFO) (toStream m)
    return $ fromSVar sv

------------------------------------------------------------------------------
-- SerialT
------------------------------------------------------------------------------

-- | The 'Semigroup' instance of 'SerialT' appends two streams sequentially,
-- yielding all elements from the first stream, and then all elements from the
-- second stream.
--
-- @
-- main = ('toList' . 'serially' $ (fromFoldable [1,2]) \<\> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- The 'Monad' instance runs the /monadic continuation/ for each
-- element of the stream, serially.
--
-- @
-- main = 'runSerialT' $ do
--     x <- return 1 \<\> return 2
--     liftIO $ print x
-- @
-- @
-- 1
-- 2
-- @
--
-- 'SerialT' nests streams serially in a depth first manner.
--
-- @
-- main = 'runSerialT' $ do
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
-- This behavior of 'SerialT' is exactly like a list transformer. We call the
-- monadic code being run for each element of the stream a monadic
-- continuation. In imperative paradigm we can think of this composition as
-- nested @for@ loops and the monadic continuation is the body of the loop. The
-- loop iterates for all elements of the stream.
--
-- Note that serial composition can be used to combine an infinite number of
-- streams as it explores only one stream at a time.
--
newtype SerialT m a = SerialT {getSerialT :: Stream m a}
    deriving (Semigroup, Monoid, MonadTrans, MonadIO, MonadThrow)

deriving instance MonadAsync m => Alternative (SerialT m)
deriving instance MonadAsync m => MonadPlus (SerialT m)
deriving instance (MonadBase b m, Monad m) => MonadBase b (SerialT m)
deriving instance MonadError e m => MonadError e (SerialT m)
deriving instance MonadReader r m => MonadReader r (SerialT m)
deriving instance MonadState s m => MonadState s (SerialT m)

instance IsStream SerialT where
    toStream = getSerialT
    fromStream = SerialT

-- | Same as SerialT.
{-# Deprecated StreamT "Please use SerialT instead." #-}
type StreamT = SerialT

-- XXX The Functor/Applicative/Num instances for all the types are exactly the
-- same, how can we reduce this boilerplate (use TH)? We cannot derive them
-- from a single base type because they depend on the Monad instance which is
-- different for each type.

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Same as the 'Semigroup' instance of 'SerialT'. Appends two streams
-- sequentially, yielding all elements from the first stream, and then all
-- elements from the second stream.
{-# INLINE append #-}
append :: IsStream t => t m a -> t m a -> t m a
append m1 m2 = fromStream $ S.append (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (SerialT m) where
    return = pure
    (SerialT (Stream m)) >>= f = SerialT $ Stream $ \_ stp yld ->
        let run x = (S.runStream x) Nothing stp yld
            yield a Nothing  = run $ toStream (f a)
            yield a (Just r) = run $ toStream $ f a <> (fromStream r >>= f)
        in m Nothing stp yield

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance Monad m => Applicative (SerialT m) where
    pure a = SerialT $ S.cons a Nothing
    (<*>) = ap

------------------------------------------------------------------------------
-- Functor
------------------------------------------------------------------------------

instance Monad m => Functor (SerialT m) where
    fmap f (SerialT (Stream m)) = SerialT $ Stream $ \_ stp yld ->
        let yield a Nothing  = yld (f a) Nothing
            yield a (Just r) = yld (f a)
                               (Just (getSerialT (fmap f (SerialT r))))
        in m Nothing stp yield

------------------------------------------------------------------------------
-- Num
------------------------------------------------------------------------------

instance (Monad m, Num a) => Num (SerialT m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (SerialT m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (SerialT m a) where
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

-- | The 'Semigroup' instance of 'InterleavedT' interleaves two streams,
-- yielding one element from each stream alternately.
--
-- @
-- main = ('toList' . 'interleaving $ (fromFoldable [1,2]) \<\> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- Similarly, the 'Monad' instance fairly interleaves the iterations of the
-- inner and the outer loop, nesting loops in a breadth first manner.
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
-- Note that interleaving composition can only combine a finite number of
-- streams as it needs to retain state for each unfinished stream.
--
newtype InterleavedT m a = InterleavedT {getInterleavedT :: Stream m a}
    deriving (Monoid, MonadTrans, MonadIO, MonadThrow)

deriving instance MonadAsync m => Alternative (InterleavedT m)
deriving instance MonadAsync m => MonadPlus (InterleavedT m)
deriving instance (MonadBase b m, Monad m) => MonadBase b (InterleavedT m)
deriving instance MonadError e m => MonadError e (InterleavedT m)
deriving instance MonadReader r m => MonadReader r (InterleavedT m)
deriving instance MonadState s m => MonadState s (InterleavedT m)

instance IsStream InterleavedT where
    toStream = getInterleavedT
    fromStream = InterleavedT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Same as the 'Semigroup' instance of 'InterleavedT'.  Interleaves two
-- streams, yielding one element from each stream alternately.
{-# INLINE interleave #-}
interleave :: IsStream t => t m a -> t m a -> t m a
interleave m1 m2 = fromStream $ S.interleave (toStream m1) (toStream m2)

instance Semigroup (InterleavedT m a) where
    (<>) = interleave

infixr 5 <=>

-- | Same as 'interleave'.
{-# Deprecated (<=>) "Please use '<>' of InterleavedT or 'interleave' instead." #-}
{-# INLINE (<=>) #-}
(<=>) :: IsStream t => t m a -> t m a -> t m a
(<=>) = interleave

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (InterleavedT m) where
    return = pure
    (InterleavedT (Stream m)) >>= f = InterleavedT $ Stream $ \_ stp yld ->
        let run x = (S.runStream x) Nothing stp yld
            yield a Nothing  = run $ toStream (f a)
            yield a (Just r) = run $ toStream $ f a <> (fromStream r >>= f)
        in m Nothing stp yield

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance Monad m => Applicative (InterleavedT m) where
    pure a = InterleavedT $ S.cons a Nothing
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

-- | Left biased concurrent composition.
--
-- The Semigroup instance of 'AsyncT' concurrently /merges/ streams in a left
-- biased manner.  It keeps yielding elements from the left stream as long as
-- it can. If the left stream blocks or cannot keep up with the pace of the
-- consumer it can concurrently yield from the stream on the right as well.
--
-- @
-- main = ('toList' . 'asyncly' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- Similarly, the monad instance of 'AsyncT' /may/ run each iteration
-- concurrently using demand driven concurrency.  More concurrent iterations
-- are started only if the previous iterations are not able to produce enough
-- output for the consumer.
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
--
-- Note that this composition can be used to combine infinite number of streams
-- as it explores only a bounded number of streams at a time.
--
newtype AsyncT m a = AsyncT {getAsyncT :: Stream m a}
    deriving (MonadTrans)

deriving instance MonadAsync m => Monoid (AsyncT m a)
deriving instance MonadAsync m => Alternative (AsyncT m)
deriving instance MonadAsync m => MonadPlus (AsyncT m)
deriving instance MonadAsync m => MonadIO (AsyncT m)
deriving instance MonadAsync m => MonadThrow (AsyncT m)
deriving instance (MonadBase b m, MonadAsync m) => MonadBase b (AsyncT m)
deriving instance (MonadError e m, MonadAsync m) => MonadError e (AsyncT m)
deriving instance (MonadReader r m, MonadAsync m) => MonadReader r (AsyncT m)
deriving instance (MonadState s m, MonadAsync m) => MonadState s (AsyncT m)

instance IsStream AsyncT where
    toStream = getAsyncT
    fromStream = AsyncT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Same as the 'Semigroup' instance of 'AsyncT'.  Merges two streams
-- concurrently, preferring the elements from the left one when available.
{-# INLINE asyncmerge #-}
asyncmerge :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
asyncmerge m1 m2 = fromStream $ S.asyncmerge (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (AsyncT m a) where
    (<>) = asyncmerge

-- | Same as 'asyncmerge'.
{-# DEPRECATED (<|) "Please use '<>' of AsyncT or 'asyncmerge' instead." #-}
{-# INLINE (<|) #-}
(<|) :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
(<|) = asyncmerge

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

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
            let run x = (S.runStream x) ctx stp yld
                yield a Nothing  = run $ f a
                yield a (Just r) = run $ f a `par` go r
            in g Nothing stp yield

instance MonadAsync m => Monad (AsyncT m) where
    return = pure
    (AsyncT m) >>= f = AsyncT $ parbind par m g
        where g x = getAsyncT (f x)
              par = S.joinStreamVar2 (SVarStyle Conjunction LIFO)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance MonadAsync m => Applicative (AsyncT m) where
    pure a = AsyncT $ S.cons a Nothing
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

-- | Round robin concurrent composition.
--
-- The Semigroup instance of 'ParallelT' concurrently /merges/ streams in a
-- round robin fashion, yielding elements from both streams alternately.
--
-- @
-- main = ('toList' . 'parallely' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- Similarly, the 'Monad' instance of 'ParallelT' runs /all/ iterations fairly
-- concurrently using a round robin scheduling.
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
--
-- Note that round robin composition can only combine a finite number of
-- streams as it needs to retain state for each unfinished stream.
--
newtype ParallelT m a = ParallelT {getParallelT :: Stream m a}
    deriving (MonadTrans)

deriving instance MonadAsync m => Monoid (ParallelT m a)
deriving instance MonadAsync m => Alternative (ParallelT m)
deriving instance MonadAsync m => MonadPlus (ParallelT m)
deriving instance MonadAsync m => MonadIO (ParallelT m)
deriving instance MonadAsync m => MonadThrow (ParallelT m)
deriving instance (MonadBase b m, MonadAsync m) => MonadBase b (ParallelT m)
deriving instance (MonadError e m, MonadAsync m) => MonadError e (ParallelT m)
deriving instance (MonadReader r m, MonadAsync m) => MonadReader r (ParallelT m)
deriving instance (MonadState s m, MonadAsync m) => MonadState s (ParallelT m)

instance IsStream ParallelT where
    toStream = getParallelT
    fromStream = ParallelT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Same as the 'Semigroup' instance of 'ParallelT'.  Merges two streams
-- concurrently choosing elements from both fairly.
{-# INLINE parmerge #-}
parmerge :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parmerge m1 m2 = fromStream $ S.parmerge (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (ParallelT m a) where
    (<>) = parmerge

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance MonadAsync m => Monad (ParallelT m) where
    return = pure
    (ParallelT m) >>= f = ParallelT $ parbind par m g
        where g x = getParallelT (f x)
              par = S.joinStreamVar2 (SVarStyle Conjunction FIFO)

------------------------------------------------------------------------------
-- Applicative
------------------------------------------------------------------------------

instance MonadAsync m => Applicative (ParallelT m) where
    pure a = ParallelT $ S.cons a Nothing
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
zipWith :: IsStream t => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \_ stp yld -> do
        let merge a ra =
                let yield2 b Nothing   = yld (f a b) Nothing
                    yield2 b (Just rb) = yld (f a b) (Just (go ra rb))
                 in (S.runStream my) Nothing stp yield2
        let yield1 a Nothing   = merge a S.nil
            yield1 a (Just ra) = merge a ra
        (S.runStream mx) Nothing stp yield1

-- | The applicative instance of 'ZipSerial' zips a number of streams serially
-- i.e. it produces one element from each stream serially and then zips all
-- those elements. Note, for convenience we have used the 'zipping' combinator
-- in the following example instead of using a 'ZipSerial' type annotation.
--
-- @
-- main = (toList . 'zipping' $ (,,) \<$\> s1 \<*\> s2 \<*\> s3) >>= print
--     where s1 = fromFoldable [1, 2]
--           s2 = fromFoldable [3, 4]
--           s3 = fromFoldable [5, 6]
-- @
-- @
-- [(1,3,5),(2,4,6)]
-- @
--
-- The 'Semigroup' instance of this type works the same way as that of
-- 'SerialT'.
--
newtype ZipSerial m a = ZipSerial {getZipSerial :: Stream m a}
        deriving (Semigroup, Monoid)

{-# Deprecated ZipStream "Please use ZipSerial instead." #-}
-- | Same as ZipSerial.
type ZipStream = ZipSerial

deriving instance MonadAsync m => Alternative (ZipSerial m)

instance Monad m => Functor (ZipSerial m) where
    fmap f (ZipSerial (Stream m)) = ZipSerial $ Stream $ \_ stp yld ->
        let yield a Nothing  = yld (f a) Nothing
            yield a (Just r) = yld (f a)
                               (Just (getZipSerial (fmap f (ZipSerial r))))
        in m Nothing stp yield

instance Monad m => Applicative (ZipSerial m) where
    pure = ZipSerial . S.repeat
    (<*>) = zipWith id

instance IsStream ZipSerial where
    toStream = getZipSerial
    fromStream = ZipSerial

instance (Monad m, Num a) => Num (ZipSerial m a) where
    fromInteger n = pure (fromInteger n)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (ZipSerial m a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (ZipSerial m a) where
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
zipAsyncWith :: (IsStream t, MonadAsync m)
    => (a -> b -> c) -> t m a -> t m b -> t m c
zipAsyncWith f m1 m2 = fromStream $ Stream $ \_ stp yld -> do
    ma <- async m1
    mb <- async m2
    (S.runStream (toStream (zipWith f ma mb))) Nothing stp yld

-- | Like 'ZipSerial' but zips in parallel, it generates all the elements to
-- be zipped concurrently.
--
-- @
-- main = (toList . 'zippingAsync' $ (,,) \<$\> s1 \<*\> s2 \<*\> s3) >>= print
--     where s1 = fromFoldable [1, 2]
--           s2 = fromFoldable [3, 4]
--           s3 = fromFoldable [5, 6]
-- @
-- @
-- [(1,3,5),(2,4,6)]
-- @
--
-- The 'Semigroup' instance of this type works the same way as that of
-- 'SerialT'.
--
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
    pure = ZipAsync . S.repeat
    (<*>) = zipAsyncWith id

instance IsStream ZipAsync where
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
adapt :: (IsStream t1, IsStream t2) => t1 m a -> t2 m a
adapt = fromStream . toStream

-- | Interpret an ambiguously typed stream as 'SerialT'.
serially :: SerialT m a -> SerialT m a
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

-- | Interpret an ambiguously typed stream as 'ZipSerial'.
zipping :: ZipSerial m a -> ZipSerial m a
zipping x = x

-- | Interpret an ambiguously typed stream as 'ZipAsync'.
zippingAsync :: ZipAsync m a -> ZipAsync m a
zippingAsync x = x

-------------------------------------------------------------------------------
-- Running Streams, convenience functions specialized to types
-------------------------------------------------------------------------------

-- | Same as @runStream . serially@.
runSerialT :: Monad m => SerialT m a -> m ()
runSerialT = runStream

-- | Same as @runSerialT@.
{-# Deprecated runStreamT "Please use runSerialT instead." #-}
runStreamT :: Monad m => SerialT m a -> m ()
runStreamT = runSerialT

-- | Same as @runStream . interleaving@.
runInterleavedT :: Monad m => InterleavedT m a -> m ()
runInterleavedT = runStream

-- | Same as @runStream . asyncly@.
runAsyncT :: Monad m => AsyncT m a -> m ()
runAsyncT = runStream

-- | Same as @runStream . parallely@.
runParallelT :: Monad m => ParallelT m a -> m ()
runParallelT = runStream

-- | Same as @runStream . zipping@.
runZipSerial :: Monad m => ZipSerial m a -> m ()
runZipSerial = runStream

{-# Deprecated runZipStream "Please use runZipSerial instead." #-}
-- | Same as ZipSerial.
runZipStream :: Monad m => ZipSerial m a -> m ()
runZipStream = runZipSerial

-- | Same as @runStream . zippingAsync@.
runZipAsync :: Monad m => ZipAsync m a -> m ()
runZipAsync = runStream

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
foldWith :: (IsStream t, Foldable f)
    => (t m a -> t m a -> t m a) -> f (t m a) -> t m a
foldWith f = foldr f nil

-- | Like 'foldMap' but allows you to specify a binary sum style composition
-- operator to fold a container of streams. Maps a monadic streaming action on
-- the container before folding it.
--
-- @foldMapWith (<>) return [1..3]@
{-# INLINABLE foldMapWith #-}
foldMapWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> (a -> t m b) -> f a -> t m b
foldMapWith f g = foldr (f . g) nil

-- | Like 'foldMapWith' but with the last two arguments reversed i.e. the
-- monadic streaming function is the last argument.
{-# INLINABLE forEachWith #-}
forEachWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> f a -> (a -> t m b) -> t m b
forEachWith f xs g = foldr (f . g) nil xs
