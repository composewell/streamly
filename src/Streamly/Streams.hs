{-# LANGUAGE CPP                       #-}
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
    , MonadParallel
    , S.MonadAsync      -- deprecated

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
    , consM
    , (|:)
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
    , splice
    , cosplice
    , parAhead
    , coparAhead
    , (<=>)            --deprecated
    , (<|)             --deprecated

    -- * IO Streams
    , Stream
    , Costream
    , ParAhead
    , CoparAhead
    , ZipStream
    , ZipParallel

    -- * Stream Transformers
    , StreamT
    , CostreamT
    , InterleavedT      -- deprecated
    , ParAheadT
    , AsyncT            -- deprecated
    , CoparAheadT
    , ZipStreamM
    , ZipParallelM
    , ZipAsync          -- deprecated

    -- * Type Adapters
    , asStream
    , serially         -- deprecated
    , asCostream
    , interleaving     -- deprecated
    , asParAhead
    , asyncly          -- deprecated
    , asCoparAhead
    , parallely        -- deprecated
    , asZipStream
    , zipping          -- deprecated
    , asZipParallel
    , zippingAsync     -- deprecated
    , adapt

    -- * Running Streams
    , runStreamT       -- deprecated
    , runInterleavedT  -- deprecated
    , runAsyncT        -- deprecated
    , runParallelT     -- deprecated
    , runZipStream     -- deprecated
    , runZipAsync      -- deprecated

    -- * Fold Utilities
    , foldWith
    , foldMapWith
    , forEachWith
    )
where

import           Control.Monad               (ap)
import           Control.Monad.Base          (MonadBase (..), liftBaseDefault)
import           Control.Monad.Catch         (MonadThrow, throwM)
-- import           Control.Monad.Error.Class   (MonadError(..))
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.Reader.Class  (MonadReader(..))
import           Control.Monad.State.Class   (MonadState(..))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Data.Semigroup              (Semigroup(..))
import           Streamly.Core               ( MonadParallel
                                             , SVar, SVarStyle(..)
                                             , SVarTag(..), SVarSched(..))
import qualified Streamly.Core as S

------------------------------------------------------------------------------
-- Types that can behave as a Stream
------------------------------------------------------------------------------

-- | Class of types that can represent a stream of elements of some type 'a' in
-- some monad 'm'.
--
-- @since 0.2.0
class IsStream t where
    toStream :: t m a -> S.Stream m a
    fromStream :: S.Stream m a -> t m a

-- | Same as 'IsStream'.
--
-- @since 0.1.0
{-# DEPRECATED Streaming "Please use IsStream instead." #-}
type Streaming = IsStream

------------------------------------------------------------------------------
-- Constructing a stream
------------------------------------------------------------------------------

-- | An empty stream.
--
-- @
-- > toList nil
-- []
-- @
--
-- @since 0.1.0
nil :: IsStream t => t m a
nil = fromStream S.nil

infixr 5 `consM`

-- | Constructs a stream by adding a monadic action at the head of an existing
-- stream. For example:
--
-- @
-- > toList $ getLine \`consM` getLine \`consM` nil
-- hello
-- world
-- ["hello","world"]
-- @
--
-- @since 0.2.0
consM :: (IsStream t, Monad m) => m a -> t m a -> t m a
consM m r = fromStream $ S.consM m (toStream r)

infixr 5 |:

-- | Operator equivalent of 'consM'.
--
-- @
-- > toList $ getLine |: getLine |: nil
-- hello
-- world
-- ["hello","world"]
-- @
--
-- @since 0.2.0
(|:) :: (IsStream t, Monad m) => m a -> t m a -> t m a
(|:) = consM

infixr 5 `cons`

-- | Construct a stream by adding a pure value at the head of an existing
-- stream. Same as @consM . return@. For example:
--
-- @
-- > toList $ 1 \`cons` 2 \`cons` 3 \`cons` nil
-- [1,2,3]
-- @
--
-- @since 0.1.0
cons :: IsStream t => a -> t m a -> t m a
cons a r = fromStream $ S.cons a (toStream r)

infixr 5 .:

-- | Operator equivalent of 'cons'.
--
-- @
-- > toList $ 1 .: 2 .: 3 .: nil
-- [1,2,3]
-- @
--
-- @since 0.1.1
(.:) :: IsStream t => a -> t m a -> t m a
(.:) = cons

-- | Build a stream from its church encoding.  The function passed maps
-- directly to the underlying representation of the stream type. The second
-- parameter to the function is the "yield" function yielding a value and the
-- remaining stream if any otherwise 'Nothing'. The third parameter is to
-- represent an "empty" stream.
streamBuild :: IsStream t
    => (forall r. Maybe (SVar m a)
        -> (a -> t m a -> m r)
        -> (a -> m r)
        -> m r
        -> m r)
    -> t m a
streamBuild k = fromStream $ S.Stream $ \svr stp sng yld ->
    let yield a r = yld a (toStream r)
     in k svr yield sng stp

-- | Build a singleton stream from a callback function.
fromCallback :: IsStream t => (forall r. (a -> m r) -> m r) -> t m a
fromCallback k = fromStream $ S.Stream $ \_ _ sng _ -> k sng

-- | Read an SVar to get a stream.
fromSVar :: (MonadParallel m, IsStream t) => SVar m a -> t m a
fromSVar sv = fromStream $ S.fromStreamVar sv

------------------------------------------------------------------------------
-- Destroying a stream
------------------------------------------------------------------------------

-- | Fold a stream using its church encoding. The second argument is the "step"
-- function consuming an element and the remaining stream, if any. The third
-- argument is for consuming an "empty" stream that yields nothing.
streamFold
    :: IsStream t
    => Maybe (SVar m a)
    -> (a -> t m a -> m r)
    -> (a -> m r)
    -> m r
    -> t m a
    -> m r
streamFold svr step single blank m =
    let yield a x = step a (fromStream x)
     in (S.runStream (toStream m)) svr blank single yield

-- | Run a streaming composition, discard the results. By default it interprets
-- the stream as 'StreamT', to run other types of streams use the type adapting
-- combinators for example @runStream . 'asParAhead'@.
--
-- @since 0.2.0
runStream :: Monad m => StreamT m a -> m ()
runStream m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            single _ = return ()
            yield _ r = go r
         in (S.runStream m1) Nothing stop single yield

-- | Same as 'runStream'
--
-- @since 0.1.0
{-# DEPRECATED runStreaming "Please use runStream instead." #-}
runStreaming :: (Monad m, IsStream t) => t m a -> m ()
runStreaming = runStream . adapt

-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
toSVar :: (IsStream t, MonadParallel m) => SVar m a -> t m a -> m ()
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
--
-- @since 0.1.0
async :: (IsStream t, MonadParallel m) => t m a -> m (t m a)
async m = do
    sv <- S.newStreamVar1 (SVarStyle Disjunction LIFO) (toStream m)
    return $ fromSVar sv

------------------------------------------------------------------------------
-- CPP macros for common instances
------------------------------------------------------------------------------

-- XXX use template haskell instead and include Monoid and IsStream instances
-- as well.

#define MONADPARALLEL , MonadParallel m

#define MONAD_APPLICATIVE_INSTANCE(STREAM,CONSTRAINT)         \
instance (Monad m CONSTRAINT) => Applicative (STREAM m) where { \
    pure = STREAM . S.singleton;                     \
    (<*>) = ap }

#define MONAD_COMMON_INSTANCES(STREAM,CONSTRAINT)                            \
instance (MonadBase b m, Monad m CONSTRAINT) => MonadBase b (STREAM m) where {\
    liftBase = liftBaseDefault };                                             \
                                                                              \
instance (MonadIO m CONSTRAINT) => MonadIO (STREAM m) where {                 \
    liftIO = lift . liftIO };                                                 \
                                                                              \
instance (MonadThrow m CONSTRAINT) => MonadThrow (STREAM m) where {           \
    throwM = lift . throwM };                                                 \
                                                                              \
{- \
instance (MonadError e m CONSTRAINT) => MonadError e (STREAM m) where {       \
    throwError = lift . throwError;                                           \
    catchError m h =                                                          \
        fromStream $ S.withCatchError (toStream m) (\e -> toStream $ h e) };  \
-} \
                                                                              \
instance (MonadReader r m CONSTRAINT) => MonadReader r (STREAM m) where {     \
    ask = lift ask;                                                           \
    local f m = fromStream $ S.withLocal f (toStream m) };                    \
                                                                              \
instance (MonadState s m CONSTRAINT) => MonadState s (STREAM m) where {       \
    get     = lift get;                                                       \
    put x   = lift (put x);                                                   \
    state k = lift (state k) }

------------------------------------------------------------------------------
-- StreamT
------------------------------------------------------------------------------

-- | The 'Semigroup' instance of 'StreamT' appends two streams sequentially,
-- yielding all elements from the first stream, and then all elements from the
-- second stream.
--
-- @
-- main = ('toList' . 'asStream' $ (fromFoldable [1,2]) \<\> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- The 'Monad' instance runs the /monadic continuation/ for each
-- element of the stream, asStream.
--
-- @
-- main = 'runStream' . 'asStream' $ do
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
-- main = 'runStream' . 'asStream' $ do
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
-- This behavior of 'StreamT' is exactly like a list transformer. We call the
-- monadic code being run for each element of the stream a monadic
-- continuation. In imperative paradigm we can think of this composition as
-- nested @for@ loops and the monadic continuation is the body of the loop. The
-- loop iterates for all elements of the stream.
--
-- The 'asStream' combinator can be omitted as the default stream type is
-- 'StreamT'.
-- Note that serial composition can be used to combine an infinite number of
-- streams as it explores only one stream at a time.
--
-- @since 0.1.0
newtype StreamT m a = StreamT {getStreamT :: S.Stream m a}
    deriving (Semigroup, Monoid, Functor, MonadTrans)

instance IsStream StreamT where
    toStream = getStreamT
    fromStream = StreamT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'StreamT'.
-- Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- @since 0.2.0
{-# INLINE splice #-}
splice :: IsStream t => t m a -> t m a -> t m a
splice m1 m2 = fromStream $ S.splice (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (StreamT m) where
    return = pure
    (StreamT (S.Stream m)) >>= f = StreamT $ S.Stream $ \_ stp sng yld ->
        let run x = (S.runStream x) Nothing stp sng yld
            single a  = run $ toStream (f a)
            yield a r = run $ toStream $ f a <> (fromStream r >>= f)
        in m Nothing stp single yield

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(StreamT,)
MONAD_COMMON_INSTANCES(StreamT,)

------------------------------------------------------------------------------
-- CostreamT
------------------------------------------------------------------------------

-- | The 'Semigroup' instance of 'CostreamT' interleaves two streams,
-- yielding one element from each stream alternately.
--
-- @
-- main = ('toList' . 'asCostream' $ (fromFoldable [1,2]) \<\> (fromFoldable [3,4])) >>= print
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
-- main = 'runStream' . 'asCostream' $ do
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
-- Note that a 'Costream' composition can only combine a finite number of
-- streams as it needs to retain state for each unfinished stream.
--
-- @since 0.2.0
newtype CostreamT m a = CostreamT {getCostreamT :: S.Stream m a}
    deriving (Functor, MonadTrans)

-- |
-- @since 0.1.0
{-# DEPRECATED InterleavedT "Please use 'CostreamT' instead." #-}
type InterleavedT = CostreamT

instance IsStream CostreamT where
    toStream = getCostreamT
    fromStream = CostreamT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'CostreamT'.
-- Interleaves two streams, yielding one element from each stream alternately.
--
-- @since 0.2.0
{-# INLINE cosplice #-}
cosplice :: IsStream t => t m a -> t m a -> t m a
cosplice m1 m2 = fromStream $ S.cosplice (toStream m1) (toStream m2)

instance Semigroup (CostreamT m a) where
    (<>) = cosplice

infixr 5 <=>

-- | Same as 'cosplice'.
--
-- @since 0.1.0
{-# DEPRECATED (<=>) "Please use 'cosplice' instead." #-}
{-# INLINE (<=>) #-}
(<=>) :: IsStream t => t m a -> t m a -> t m a
(<=>) = cosplice

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (CostreamT m a) where
    mempty = nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (CostreamT m) where
    return = pure
    (CostreamT (S.Stream m)) >>= f = CostreamT $ S.Stream $ \_ stp sng yld ->
        let run x = (S.runStream x) Nothing stp sng yld
            single a  = run $ toStream (f a)
            yield a r = run $ toStream $ f a <> (fromStream r >>= f)
        in m Nothing stp single yield

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(CostreamT,)
MONAD_COMMON_INSTANCES(CostreamT,)

------------------------------------------------------------------------------
-- ParAheadT
------------------------------------------------------------------------------

-- | Adaptive ahead parallel, in a left to right 'Semigroup' composition it tries to
-- yield elements from the left stream as long as it can, but it can run the
-- right stream in parallel if it needs to, based on demand. The right stream
-- can be run if the left stream blocks on IO or cannot produce elements fast
-- enough for the consumer.
--
-- @
-- main = ('toList' . 'asParAhead' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream. The output and exceptions from a single stream are guaranteed
-- to arrive in the same order in the resulting stream as they were generated
-- in the input stream. However, the relative ordering of elements from
-- different streams in the resulting stream can vary depending on scheduling
-- and generation delays.
--
-- Similarly, the monad instance of 'CoparAheadT' /may/ run each iteration
-- concurrently based on demand.  More concurrent iterations are started only
-- if the previous iterations are not able to produce enough output for the
-- consumer.
--
-- @
-- import "Streamly"
-- import Control.Concurrent
--
-- main = 'runStream' . 'asParAhead' $ do
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
-- @since 0.2.0
newtype ParAheadT m a = ParAheadT {getParAheadT :: S.Stream m a}
    deriving (Functor, MonadTrans)

-- |
-- @since 0.1.0
{-# DEPRECATED AsyncT "Please use 'ParAheadT' instead." #-}
type AsyncT = ParAheadT

instance IsStream ParAheadT where
    toStream = getParAheadT
    fromStream = ParAheadT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'ParAheadT'.
-- Merges two streams possibly concurrently, preferring the
-- elements from the left one when available.
--
-- @since 0.2.0
{-# INLINE parAhead #-}
parAhead :: (IsStream t, MonadParallel m) => t m a -> t m a -> t m a
parAhead m1 m2 = fromStream $ S.parAhead (toStream m1) (toStream m2)

instance MonadParallel m => Semigroup (ParAheadT m a) where
    (<>) = parAhead

-- | Same as 'parAhead'.
--
-- @since 0.1.0
{-# DEPRECATED (<|) "Please use 'parAhead' instead." #-}
{-# INLINE (<|) #-}
(<|) :: (IsStream t, MonadParallel m) => t m a -> t m a -> t m a
(<|) = parAhead

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadParallel m => Monoid (ParAheadT m a) where
    mempty = nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

{-# INLINE parbind #-}
parbind
    :: (forall c. S.Stream m c -> S.Stream m c -> S.Stream m c)
    -> S.Stream m a
    -> (a -> S.Stream m b)
    -> S.Stream m b
parbind par m f = go m
    where
        go (S.Stream g) =
            S.Stream $ \ctx stp sng yld ->
            let run x = (S.runStream x) ctx stp sng yld
                single a  = run $ f a
                yield a r = run $ f a `par` go r
            in g Nothing stp single yield

instance MonadParallel m => Monad (ParAheadT m) where
    return = pure
    (ParAheadT m) >>= f = ParAheadT $ parbind par m (getParAheadT . f)
        where par = S.joinStreamVar2 (SVarStyle Conjunction LIFO)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(ParAheadT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(ParAheadT, MONADPARALLEL)

------------------------------------------------------------------------------
-- CoparAheadT
------------------------------------------------------------------------------

-- | Round robin concurrent composition.
--
-- The Semigroup instance of 'CoparAheadT' concurrently /merges/ streams in a
-- round robin fashion, yielding elements from both streams alternately.
--
-- @
-- main = ('toList' . 'asCoparAhead' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream. The output and exceptions from a single stream are guaranteed
-- to arrive in the same order in the resulting stream as they were generated
-- in the input stream. However, the relative ordering of elements from
-- different streams in the resulting stream can vary depending on scheduling
-- and generation delays.
--
-- Similarly, the 'Monad' instance of 'CoparAheadT' runs /all/ iterations fairly
-- concurrently using a round robin scheduling.
--
-- @
-- import "Streamly"
-- import Control.Concurrent
--
-- main = 'runStream' . 'asCoparAhead' $ do
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
-- Unlike 'ParAheadT' all iterations are guaranteed to run fairly
-- concurrently, unconditionally.
--
-- Note that round robin composition can only combine a finite number of
-- streams as it needs to retain state for each unfinished stream.
--
-- @since 0.1.0
newtype CoparAheadT m a = CoparAheadT {getCoparAheadT :: S.Stream m a}
    deriving (Functor, MonadTrans)

instance IsStream CoparAheadT where
    toStream = getCoparAheadT
    fromStream = CoparAheadT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'CoparAheadT'.
-- Merges two streams concurrently choosing elements from both fairly.
--
-- @since 0.2.0
{-# INLINE coparAhead #-}
coparAhead :: (IsStream t, MonadParallel m) => t m a -> t m a -> t m a
coparAhead m1 m2 = fromStream $ S.coparAhead (toStream m1) (toStream m2)

instance MonadParallel m => Semigroup (CoparAheadT m a) where
    (<>) = coparAhead

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadParallel m => Monoid (CoparAheadT m a) where
    mempty = nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance MonadParallel m => Monad (CoparAheadT m) where
    return = pure
    (CoparAheadT m) >>= f = CoparAheadT $ parbind par m (getCoparAheadT . f)
        where par = S.joinStreamVar2 (SVarStyle Conjunction FIFO)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(CoparAheadT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(CoparAheadT, MONADPARALLEL)

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | The applicative instance of 'ZipStreamM' zips a number of streams serially
-- i.e. it produces one element from each stream serially and then zips all
-- those elements.
--
-- @
-- main = (toList . 'asZipStream' $ (,,) \<$\> s1 \<*\> s2 \<*\> s3) >>= print
--     where s1 = fromFoldable [1, 2]
--           s2 = fromFoldable [3, 4]
--           s3 = fromFoldable [5, 6]
-- @
-- @
-- [(1,3,5),(2,4,6)]
-- @
--
-- The 'Semigroup' instance of this type works the same way as that of
-- 'StreamT'.
--
-- @since 0.2.0
newtype ZipStreamM m a = ZipStreamM {getZipStreamM :: S.Stream m a}
        deriving (Functor, Semigroup, Monoid)

instance IsStream ZipStreamM where
    toStream = getZipStreamM
    fromStream = ZipStreamM

instance Monad m => Applicative (ZipStreamM m) where
    pure = ZipStreamM . S.repeat
    m1 <*> m2 = fromStream $ S.zipWith id (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Parallely Zipping Streams
------------------------------------------------------------------------------

-- | Like 'ZipStreamM' but zips in parallel, it generates all the elements to
-- be zipped concurrently.
--
-- @
-- main = (toList . 'asZipParallel' $ (,,) \<$\> s1 \<*\> s2 \<*\> s3) >>= print
--     where s1 = fromFoldable [1, 2]
--           s2 = fromFoldable [3, 4]
--           s3 = fromFoldable [5, 6]
-- @
-- @
-- [(1,3,5),(2,4,6)]
-- @
--
-- The 'Semigroup' instance of this type works the same way as that of
-- 'StreamT'.
--
-- @since 0.2.0
newtype ZipParallelM m a = ZipParallelM {getZipParallelM :: S.Stream m a}
        deriving (Functor, Semigroup, Monoid)

-- |
-- @since 0.1.0
{-# DEPRECATED ZipAsync "Please use ZipParallelM instead." #-}
type ZipAsync = ZipParallelM

instance IsStream ZipParallelM where
    toStream = getZipParallelM
    fromStream = ZipParallelM

instance MonadParallel m => Applicative (ZipParallelM m) where
    pure = ZipParallelM . S.repeat
    m1 <*> m2 = fromStream $ S.zipParallelWith id (toStream m1) (toStream m2)

-------------------------------------------------------------------------------
-- Type adapting combinators
-------------------------------------------------------------------------------

-- | Adapt any specific stream type to any other specific stream type.
--
-- @since 0.1.0
adapt :: (IsStream t1, IsStream t2) => t1 m a -> t2 m a
adapt = fromStream . toStream

-- | Fix the type of a polymorphic stream as 'StreamT'.
--
-- @since 0.2.0
asStream :: IsStream t => StreamT m a -> t m a
asStream = adapt

-- | Same as 'asStream'.
--
-- @since 0.1.0
{-# DEPRECATED serially "Please use asStream instead." #-}
serially :: IsStream t => StreamT m a -> t m a
serially = asStream

-- | Fix the type of a polymorphic stream as 'CostreamT'.
--
-- @since 0.2.0
asCostream :: IsStream t => CostreamT m a -> t m a
asCostream = adapt

-- | Same as 'asCostream'.
--
-- @since 0.1.0
{-# DEPRECATED interleaving "Please use asCostream instead." #-}
interleaving :: IsStream t => CostreamT m a -> t m a
interleaving = asCostream

-- | Fix the type of a polymorphic stream as 'ParAheadT'.
--
-- @since 0.2.0
asParAhead :: IsStream t => ParAheadT m a -> t m a
asParAhead = adapt

-- | Same as 'asParAhead'.
--
-- @since 0.1.0
{-# DEPRECATED asyncly "Please use asParAhead instead." #-}
asyncly :: IsStream t => ParAheadT m a -> t m a
asyncly = asParAhead

-- | Fix the type of a polymorphic stream as 'CoparAheadT'.
--
-- @since 0.1.0
asCoparAhead :: IsStream t => CoparAheadT m a -> t m a
asCoparAhead = adapt

-- | Same as 'asCoparAhead'.
--
-- @since 0.1.0
{-# DEPRECATED parallely "Please use asCoparAhead instead." #-}
parallely :: IsStream t => CoparAheadT m a -> t m a
parallely = asCoparAhead

-- | Fix the type of a polymorphic stream as 'ZipStreamM'.
--
-- @since 0.2.0
asZipStream :: IsStream t => ZipStreamM m a -> t m a
asZipStream = adapt

-- | Same as 'asZipStream'.
--
-- @since 0.1.0
{-# DEPRECATED zipping "Please use asZipStream instead." #-}
zipping :: IsStream t => ZipStreamM m a -> t m a
zipping = asZipStream

-- | Fix the type of a polymorphic stream as 'ZipParallelM'.
--
-- @since 0.2.0
asZipParallel :: IsStream t => ZipParallelM m a -> t m a
asZipParallel = adapt

-- | Same as 'asZipParallel'.
--
-- @since 0.1.0
{-# DEPRECATED zippingAsync "Please use asZipParallel instead." #-}
zippingAsync :: IsStream t => ZipParallelM m a -> t m a
zippingAsync = asZipParallel

-------------------------------------------------------------------------------
-- Running Streams, convenience functions specialized to types
-------------------------------------------------------------------------------

-- | Same as @runStream@.
--
-- @since 0.1.0
{-# DEPRECATED runStreamT "Please use runStream instead." #-}
runStreamT :: Monad m => StreamT m a -> m ()
runStreamT = runStream

-- | Same as @runStream . asCostream@.
--
-- @since 0.1.0
{-# DEPRECATED runInterleavedT "Please use 'runStream . interleaving' instead." #-}
runInterleavedT :: Monad m => InterleavedT m a -> m ()
runInterleavedT = runStream . asCostream

-- | Same as @runStream . asParAhead@.
--
-- @since 0.1.0
{-# DEPRECATED runAsyncT "Please use 'runStream . asParAhead' instead." #-}
runAsyncT :: Monad m => ParAheadT m a -> m ()
runAsyncT = runStream . asParAhead

-- | Same as @runStream . asCoparAhead@.
--
-- @since 0.1.0
{-# DEPRECATED runParallelT "Please use 'runStream . asCoparAhead' instead." #-}
runParallelT :: Monad m => CoparAheadT m a -> m ()
runParallelT = runStream . asCoparAhead

-- | Same as @runStream . zipping@.
--
-- @since 0.1.0
{-# DEPRECATED runZipStream "Please use 'runStream . asZipStream instead." #-}
runZipStream :: Monad m => ZipStreamM m a -> m ()
runZipStream = runStream . asZipStream

-- | Same as @runStream . zippingAsync@.
--
-- @since 0.1.0
{-# DEPRECATED runZipAsync "Please use 'runStream . asZipParallel instead." #-}
runZipAsync :: Monad m => ZipParallelM m a -> m ()
runZipAsync = runStream . asZipParallel

------------------------------------------------------------------------------
-- IO Streams
------------------------------------------------------------------------------

-- | A serial IO stream of elements of type @a@. See 'StreamT' documentation
-- for more details.
--
-- @since 0.2.0
type Stream a = StreamT IO a

-- | An interleaving serial IO stream of elements of type @a@. See 'CostreamT'
-- documentation for more details.
--
-- @since 0.2.0
type Costream a = CostreamT IO a

-- | A demand driven left biased parallely composing IO stream of elements of
-- type @a@.  See 'ParAheadT' documentation for more details.
--
-- @since 0.2.0
type ParAhead a = ParAheadT IO a

-- | A round robin parallely composing IO stream of elements of type @a@.
-- See 'CoparAheadT' documentation for more details.
--
-- @since 0.2.0
type CoparAhead a = CoparAheadT IO a

-- | An IO stream whose applicative instance zips streams serially.
--
-- @since 0.2.0
type ZipStream a = ZipStreamM IO a

-- | An IO stream whose applicative instance zips streams asCoparAhead.
--
-- @since 0.2.0
type ZipParallel a = ZipParallelM IO a

------------------------------------------------------------------------------
-- Fold Utilities
------------------------------------------------------------------------------

-- | A variant of 'Data.Foldable.fold' that allows you to fold a 'Foldable'
-- container of streams using the specified stream sum operation.
--
-- @foldWith 'parAhead' $ map return [1..3]@
--
-- @since 0.1.0
{-# INLINABLE foldWith #-}
foldWith :: (IsStream t, Foldable f)
    => (t m a -> t m a -> t m a) -> f (t m a) -> t m a
foldWith f = foldr f nil

-- | A variant of 'foldMap' that allows you to map a monadic streaming action
-- on a 'Foldable' container and then fold it using the specified stream sum
-- operation.
--
-- @foldMapWith 'parAhead' return [1..3]@
--
-- @since 0.1.0
{-# INLINABLE foldMapWith #-}
foldMapWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> (a -> t m b) -> f a -> t m b
foldMapWith f g = foldr (f . g) nil

-- | Like 'foldMapWith' but with the last two arguments reversed i.e. the
-- monadic streaming function is the last argument.
--
-- @since 0.1.0
{-# INLINABLE forEachWith #-}
forEachWith :: (IsStream t, Foldable f)
    => (t m b -> t m b -> t m b) -> f a -> (a -> t m b) -> t m b
forEachWith f xs g = foldr (f . g) nil xs
