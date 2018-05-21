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
    , S.MonadAsync

    -- * SVars
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
    , mkAsync

    -- * Merging Streams
    , serial
    , wSerial
    , ahead
    , async
    , wAsync
    , parallel
    , (<=>)            --deprecated
    , (<|)             --deprecated

    -- * IO Streams
    , Serial
    , WSerial
    , Ahead
    , Async
    , WAsync
    , Parallel
    , ZipSerial
    , ZipAsync

    -- * Stream Transformers
    , SerialT
    , StreamT           -- deprecated
    , WSerialT
    , InterleavedT      -- deprecated
    , AheadT
    , AsyncT
    , WAsyncT
    , ParallelT
    , ZipStream         -- deprecated
    , ZipSerialM
    , ZipAsyncM

    -- * Type Adapters
    , serially         -- deprecated
    , wSerially
    , interleaving     -- deprecated
    , aheadly
    , asyncly
    , wAsyncly
    , parallely
    , zipSerially
    , zipping          -- deprecated
    , zipAsyncly
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
import           Streamly.Core               ( MonadAsync , SVar,
                                               SVarStyle(..))
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
-- stream. For pure values it can be faster than 'consM'. For example:
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
fromSVar :: (MonadAsync m, IsStream t) => SVar m a -> t m a
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
-- the stream as 'SerialT', to run other types of streams use the type adapting
-- combinators for example @runStream . 'asyncly'@.
--
-- @since 0.2.0
runStream :: Monad m => SerialT m a -> m ()
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
--
-- @since 0.2.0
mkAsync :: (IsStream t, MonadAsync m) => t m a -> m (t m a)
mkAsync m = do
    sv <- S.newStreamVar1 AsyncVar (toStream m)
    return $ fromSVar sv

------------------------------------------------------------------------------
-- CPP macros for common instances
------------------------------------------------------------------------------

-- XXX use template haskell instead and include Monoid and IsStream instances
-- as well.

#define MONADPARALLEL , MonadAsync m

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
-- SerialT
------------------------------------------------------------------------------

-- | Deep serial composition or serial composition with depth first traversal.
-- The 'Semigroup' instance of 'SerialT' appends two streams serially in a
-- depth first manner, yielding all elements from the first stream, and then
-- all elements from the second stream.
--
-- @
-- import Streamly
-- import qualified "Streamly.Prelude" as S
--
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
-- main = 'runStream' . 'serially' $ do
--     x <- return 1 \<\> return 2
--     S.once $ print x
-- @
-- @
-- 1
-- 2
-- @
--
-- 'SerialT' nests streams serially in a depth first manner.
--
-- @
-- main = 'runStream' . 'serially' $ do
--     x <- return 1 \<\> return 2
--     y <- return 3 \<\> return 4
--     S.once $ print (x, y)
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
-- The 'serially' combinator can be omitted as the default stream type is
-- 'SerialT'.
-- Note that serial composition with depth first traversal can be used to
-- combine an infinite number of streams as it explores only one stream at a
-- time.
--
-- @since 0.2.0
newtype SerialT m a = SerialT {getSerialT :: S.Stream m a}
    deriving (Semigroup, Monoid, Functor, MonadTrans)

-- |
-- @since 0.1.0
{-# DEPRECATED StreamT "Please use 'SerialT' instead." #-}
type StreamT = SerialT

instance IsStream SerialT where
    toStream = getSerialT
    fromStream = SerialT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'SerialT'.
-- Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- @since 0.2.0
{-# INLINE serial #-}
serial :: IsStream t => t m a -> t m a -> t m a
serial m1 m2 = fromStream $ S.serial (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (SerialT m) where
    return = pure
    (SerialT (S.Stream m)) >>= f = SerialT $ S.Stream $ \_ stp sng yld ->
        let run x = (S.runStream x) Nothing stp sng yld
            single a  = run $ toStream (f a)
            yield a r = run $ toStream $ f a <> (fromStream r >>= f)
        in m Nothing stp single yield

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(SerialT,)
MONAD_COMMON_INSTANCES(SerialT,)

------------------------------------------------------------------------------
-- WSerialT
------------------------------------------------------------------------------

-- | Wide serial composition or serial composition with a breadth first
-- traversal. The 'Semigroup' instance of 'WSerialT' traverses
-- the two streams in a breadth first manner. In other words, it interleaves
-- two streams, yielding one element from each stream alternately.
--
-- @
-- import Streamly
-- import qualified "Streamly.Prelude" as S
--
-- main = ('toList' . 'wSerially' $ (fromFoldable [1,2]) \<\> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- Similarly, the 'Monad' instance interleaves the iterations of the
-- inner and the outer loop, nesting loops in a breadth first manner.
--
--
-- @
-- main = 'runStream' . 'wSerially' $ do
--     x <- return 1 \<\> return 2
--     y <- return 3 \<\> return 4
--     S.once $ print (x, y)
-- @
-- @
-- (1,3)
-- (2,3)
-- (1,4)
-- (2,4)
-- @
--
-- Note that a serial composition with breadth first traversal can only combine
-- a finite number of streams as it needs to retain state for each unfinished
-- stream.
--
-- @since 0.2.0
newtype WSerialT m a = WSerialT {getWSerialT :: S.Stream m a}
    deriving (Functor, MonadTrans)

-- |
-- @since 0.1.0
{-# DEPRECATED InterleavedT "Please use 'WSerialT' instead." #-}
type InterleavedT = WSerialT

instance IsStream WSerialT where
    toStream = getWSerialT
    fromStream = WSerialT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'WSerialT'.
-- Interleaves two streams, yielding one element from each stream alternately.
--
-- @since 0.2.0
{-# INLINE wSerial #-}
wSerial :: IsStream t => t m a -> t m a -> t m a
wSerial m1 m2 = fromStream $ S.wSerial (toStream m1) (toStream m2)

instance Semigroup (WSerialT m a) where
    (<>) = wSerial

infixr 5 <=>

-- | Same as 'wSerial'.
--
-- @since 0.1.0
{-# DEPRECATED (<=>) "Please use 'wSerial' instead." #-}
{-# INLINE (<=>) #-}
(<=>) :: IsStream t => t m a -> t m a -> t m a
(<=>) = wSerial

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (WSerialT m a) where
    mempty = nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance Monad m => Monad (WSerialT m) where
    return = pure
    (WSerialT (S.Stream m)) >>= f = WSerialT $ S.Stream $ \_ stp sng yld ->
        let run x = (S.runStream x) Nothing stp sng yld
            single a  = run $ toStream (f a)
            yield a r = run $ toStream $ f a <> (fromStream r >>= f)
        in m Nothing stp single yield

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(WSerialT,)
MONAD_COMMON_INSTANCES(WSerialT,)

------------------------------------------------------------------------------
-- AheadT
------------------------------------------------------------------------------

-- | Deep ahead composition or ahead composition with depth first traversal.
-- The semigroup composition of 'AheadT' appends streams in a depth first
-- manner just like 'SerialT' except that it can produce elements concurrently
-- ahead of time. It is like 'AsyncT' except that 'AsyncT' produces the output
-- as it arrives whereas 'AheadT' orders the output in the traversal order.
--
-- @
-- main = ('toList' . 'aheadly' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,2,3,4]
-- @
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream.
--
-- Similarly, the monad instance of 'AheadT' may run each iteration
-- concurrently ahead of time but presents the results in the same order as
-- 'SerialT'.
--
-- @
-- import "Streamly"
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = 'runStream' . 'aheadly' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     S.once $ do
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
-- Note that ahead composition with depth first traversal can be used to
-- combine infinite number of streams as it explores only a bounded number of
-- streams at a time.
--
-- @since 0.3.0
newtype AheadT m a = AheadT {getAheadT :: S.Stream m a}
    deriving (Functor, MonadTrans)

instance IsStream AheadT where
    toStream = getAheadT
    fromStream = AheadT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'AheadT'.
-- Merges two streams sequentially but with concurrent lookahead.
--
-- @since 0.3.0
{-# INLINE ahead #-}
ahead :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
ahead m1 m2 = fromStream $ S.ahead (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (AheadT m a) where
    (<>) = ahead

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (AheadT m a) where
    mempty = nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

{-# INLINE aheadbind #-}
aheadbind
    :: MonadAsync m
    => S.Stream m a
    -> (a -> S.Stream m b)
    -> S.Stream m b
aheadbind m f = go m
    where
        go (S.Stream g) =
            S.Stream $ \ctx stp sng yld ->
            let run x = (S.runStream x) ctx stp sng yld
                single a  = run $ f a
                yield a r = run $ f a `S.ahead` go r
            in g Nothing stp single yield

instance MonadAsync m => Monad (AheadT m) where
    return = pure
    (AheadT m) >>= f = AheadT $ aheadbind m (getAheadT . f)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(AheadT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(AheadT, MONADPARALLEL)

------------------------------------------------------------------------------
-- AsyncT
------------------------------------------------------------------------------

-- | Deep async composition or async composition with depth first traversal. In
-- a left to right 'Semigroup' composition it tries to yield elements from the
-- left stream as long as it can, but it can run the right stream in parallel
-- if it needs to, based on demand. The right stream can be run if the left
-- stream blocks on IO or cannot produce elements fast enough for the consumer.
--
-- @
-- main = ('toList' . 'asyncly' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
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
-- Similarly, the monad instance of 'AsyncT' /may/ run each iteration
-- concurrently based on demand.  More concurrent iterations are started only
-- if the previous iterations are not able to produce enough output for the
-- consumer.
--
-- @
-- import "Streamly"
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = 'runStream' . 'asyncly' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     S.once $ do
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
-- Note that async composition with depth first traversal can be used to
-- combine infinite number of streams as it explores only a bounded number of
-- streams at a time.
--
-- @since 0.1.0
newtype AsyncT m a = AsyncT {getAsyncT :: S.Stream m a}
    deriving (Functor, MonadTrans)

instance IsStream AsyncT where
    toStream = getAsyncT
    fromStream = AsyncT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'AsyncT'.
-- Merges two streams possibly concurrently, preferring the
-- elements from the left one when available.
--
-- @since 0.2.0
{-# INLINE async #-}
async :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
async m1 m2 = fromStream $ S.async (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (AsyncT m a) where
    (<>) = async

-- | Same as 'async'.
--
-- @since 0.1.0
{-# DEPRECATED (<|) "Please use 'async' instead." #-}
{-# INLINE (<|) #-}
(<|) :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
(<|) = async

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (AsyncT m a) where
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

instance MonadAsync m => Monad (AsyncT m) where
    return = pure
    (AsyncT m) >>= f = AsyncT $ parbind S.async m (getAsyncT . f)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(AsyncT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(AsyncT, MONADPARALLEL)

------------------------------------------------------------------------------
-- WAsyncT
------------------------------------------------------------------------------

-- | Wide async composition or async composition with breadth first traversal.
-- The Semigroup instance of 'WAsyncT' concurrently /traverses/ the composed
-- streams using a depth first travesal or in a round robin fashion, yielding
-- elements from both streams alternately.
--
-- @
-- main = ('toList' . 'wAsyncly' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
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
-- Similarly, the 'Monad' instance of 'WAsyncT' runs /all/ iterations fairly
-- concurrently using a round robin scheduling.
--
-- @
-- import "Streamly"
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = 'runStream' . 'wAsyncly' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     S.once $ do
--          threadDelay (n * 1000000)
--          myThreadId >>= \\tid -> putStrLn (show tid ++ ": Delay " ++ show n)
-- @
-- @
-- ThreadId 40: Delay 1
-- ThreadId 39: Delay 2
-- ThreadId 38: Delay 3
-- @
--
-- Unlike 'AsyncT' all iterations are guaranteed to run fairly
-- concurrently, unconditionally.
--
-- Note that async composition with breadth first traversal can only combine a
-- finite number of streams as it needs to retain state for each unfinished
-- stream.
--
-- @since 0.2.0
newtype WAsyncT m a = WAsyncT {getWAsyncT :: S.Stream m a}
    deriving (Functor, MonadTrans)

instance IsStream WAsyncT where
    toStream = getWAsyncT
    fromStream = WAsyncT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'WAsyncT'.
-- Merges two streams concurrently choosing elements from both fairly.
--
-- @since 0.2.0
{-# INLINE wAsync #-}
wAsync :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
wAsync m1 m2 = fromStream $ S.wAsync (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (WAsyncT m a) where
    (<>) = wAsync

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (WAsyncT m a) where
    mempty = nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance MonadAsync m => Monad (WAsyncT m) where
    return = pure
    (WAsyncT m) >>= f =
        WAsyncT $ parbind S.wAsync m (getWAsyncT . f)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(WAsyncT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(WAsyncT, MONADPARALLEL)

------------------------------------------------------------------------------
-- ParallelT
------------------------------------------------------------------------------

-- | Async composition with simultaneous traversal of all streams.
--
-- The Semigroup instance of 'ParallelT' concurrently /merges/ two streams,
-- running both strictly concurrently and yielding elements from both streams
-- as they arrive. When multiple streams are combined using 'ParallelT' each
-- one is evaluated in its own thread and the results produced are presented in
-- the combined stream on a first come first serve basis.
--
-- 'AsyncT' and 'WAsyncT' are /concurrent lookahead streams/ each with a
-- specific type of consumption pattern (depth first or breadth first). Since
-- they are lookahead, they may introduce certain default latency in starting
-- more concurrent tasks for efficiency reasons or may put a default limitation
-- on the resource consumption (e.g. number of concurrent threads for
-- lookahead).  If we look at the implementation detail, they both can share a
-- pool of worker threads to evaluate the streams in the desired pattern and at
-- the desired rate. However, 'ParallelT' uses a separate runtime thread to
-- evaluate each stream.
--
-- 'WAsyncT' is similar to 'ParallelT', as both of them evaluate the
-- constituent streams fairly in a round robin fashion.
-- However, the key difference is that 'WAsyncT' is lazy or pull driven
-- whereas 'ParallelT' is strict or push driven.  'ParallelT' immediately
-- starts concurrent evaluation of both the streams (in separate threads) and
-- later picks the results whereas 'WAsyncT' may wait for a certain latency
-- threshold before initiating concurrent evaluation of the next stream. The
-- concurrent scheduling of the next stream or the degree of concurrency is
-- driven by the feedback from the consumer. In case of 'ParallelT' each stream
-- is evaluated in a separate thread and results are /pushed/ to a shared
-- output buffer, the evaluation rate is controlled by blocking when the buffer
-- is full.
--
-- Concurrent lookahead streams are generally more efficient than
-- 'ParallelT' and can work pretty efficiently even for smaller tasks because
-- they do not necessarily use a separate thread for each task. So they should
-- be preferred over 'ParallelT' especially when efficiency is a concern and
-- simultaneous strict evaluation is not a requirement.  'ParallelT' is useful
-- for cases when the streams are required to be evaluated simultaneously
-- irrespective of how the consumer consumes them e.g.  when we want to race
-- two tasks and want to start both strictly at the same time or if we have
-- timers in the parallel tasks and our results depend on the timers being
-- started at the same time.  We can say that 'ParallelT' is almost the same
-- (modulo some implementation differences) as 'WAsyncT' when the latter is
-- used with unlimited lookahead and zero latency in initiating lookahead.
--
-- @
-- main = ('toList' . 'parallely' $ (fromFoldable [1,2]) \<> (fromFoldable [3,4])) >>= print
-- @
-- @
-- [1,3,2,4]
-- @
--
-- When streams with more than one element are merged, it yields whichever
-- stream yields first without any bias, unlike the 'Async' style streams.
--
-- Any exceptions generated by a constituent stream are propagated to the
-- output stream. The output and exceptions from a single stream are guaranteed
-- to arrive in the same order in the resulting stream as they were generated
-- in the input stream. However, the relative ordering of elements from
-- different streams in the resulting stream can vary depending on scheduling
-- and generation delays.
--
-- Similarly, the 'Monad' instance of 'ParallelT' runs /all/ iterations
-- of the loop concurrently.
--
-- @
-- import "Streamly"
-- import qualified "Streamly.Prelude" as S
-- import Control.Concurrent
--
-- main = 'runStream' . 'parallely' $ do
--     n <- return 3 \<\> return 2 \<\> return 1
--     S.once $ do
--          threadDelay (n * 1000000)
--          myThreadId >>= \\tid -> putStrLn (show tid ++ ": Delay " ++ show n)
-- @
-- @
-- ThreadId 40: Delay 1
-- ThreadId 39: Delay 2
-- ThreadId 38: Delay 3
-- @
--
-- Note that parallel composition can only combine a finite number of
-- streams as it needs to retain state for each unfinished stream.
--
-- @since 0.1.0
newtype ParallelT m a = ParallelT {getParallelT :: S.Stream m a}
    deriving (Functor, MonadTrans)

instance IsStream ParallelT where
    toStream = getParallelT
    fromStream = ParallelT

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Polymorphic version of the 'Semigroup' operation '<>' of 'ParallelT'
-- Merges two streams concurrently.
--
-- @since 0.2.0
{-# INLINE parallel #-}
parallel :: (IsStream t, MonadAsync m) => t m a -> t m a -> t m a
parallel m1 m2 = fromStream $ S.parallel (toStream m1) (toStream m2)

instance MonadAsync m => Semigroup (ParallelT m a) where
    (<>) = parallel

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance MonadAsync m => Monoid (ParallelT m a) where
    mempty = nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Monad
------------------------------------------------------------------------------

instance MonadAsync m => Monad (ParallelT m) where
    return = pure
    (ParallelT m) >>= f = ParallelT $ parbind S.parallel m (getParallelT . f)

------------------------------------------------------------------------------
-- Other instances
------------------------------------------------------------------------------

MONAD_APPLICATIVE_INSTANCE(ParallelT,MONADPARALLEL)
MONAD_COMMON_INSTANCES(ParallelT, MONADPARALLEL)

------------------------------------------------------------------------------
-- Serially Zipping Streams
------------------------------------------------------------------------------

-- | The applicative instance of 'ZipSerialM' zips a number of streams serially
-- i.e. it produces one element from each stream serially and then zips all
-- those elements.
--
-- @
-- main = (toList . 'zipSerially' $ (,,) \<$\> s1 \<*\> s2 \<*\> s3) >>= print
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
-- @since 0.2.0
newtype ZipSerialM m a = ZipSerialM {getZipSerialM :: S.Stream m a}
        deriving (Functor, Semigroup, Monoid)

-- |
-- @since 0.1.0
{-# DEPRECATED ZipStream "Please use 'ZipSerialM' instead." #-}
type ZipStream = ZipSerialM

instance IsStream ZipSerialM where
    toStream = getZipSerialM
    fromStream = ZipSerialM

instance Monad m => Applicative (ZipSerialM m) where
    pure = ZipSerialM . S.repeat
    m1 <*> m2 = fromStream $ S.zipWith id (toStream m1) (toStream m2)

------------------------------------------------------------------------------
-- Parallely Zipping Streams
------------------------------------------------------------------------------

-- | Like 'ZipSerialM' but zips in parallel, it generates all the elements to
-- be zipped concurrently.
--
-- @
-- main = (toList . 'zipAsyncly' $ (,,) \<$\> s1 \<*\> s2 \<*\> s3) >>= print
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
-- @since 0.2.0
newtype ZipAsyncM m a = ZipAsyncM {getZipAsyncM :: S.Stream m a}
        deriving (Functor, Semigroup, Monoid)

instance IsStream ZipAsyncM where
    toStream = getZipAsyncM
    fromStream = ZipAsyncM

instance MonadAsync m => Applicative (ZipAsyncM m) where
    pure = ZipAsyncM . S.repeat
    m1 <*> m2 = fromStream $ S.zipAsyncWith id (toStream m1) (toStream m2)

-------------------------------------------------------------------------------
-- Type adapting combinators
-------------------------------------------------------------------------------

-- | Adapt any specific stream type to any other specific stream type.
--
-- @since 0.1.0
adapt :: (IsStream t1, IsStream t2) => t1 m a -> t2 m a
adapt = fromStream . toStream

-- | Fix the type of a polymorphic stream as 'SerialT'.
--
-- @since 0.1.0
serially :: IsStream t => SerialT m a -> t m a
serially = adapt

-- | Fix the type of a polymorphic stream as 'WSerialT'.
--
-- @since 0.2.0
wSerially :: IsStream t => WSerialT m a -> t m a
wSerially = adapt

-- | Same as 'wSerially'.
--
-- @since 0.1.0
{-# DEPRECATED interleaving "Please use wSerially instead." #-}
interleaving :: IsStream t => WSerialT m a -> t m a
interleaving = wSerially

-- | Fix the type of a polymorphic stream as 'AheadT'.
--
-- @since 0.3.0
aheadly :: IsStream t => AheadT m a -> t m a
aheadly = adapt

-- | Fix the type of a polymorphic stream as 'AsyncT'.
--
-- @since 0.1.0
asyncly :: IsStream t => AsyncT m a -> t m a
asyncly = adapt

-- | Fix the type of a polymorphic stream as 'WAsyncT'.
--
-- @since 0.2.0
wAsyncly :: IsStream t => WAsyncT m a -> t m a
wAsyncly = adapt

-- | Fix the type of a polymorphic stream as 'ParallelT'.
--
-- @since 0.1.0
parallely :: IsStream t => ParallelT m a -> t m a
parallely = adapt

-- | Fix the type of a polymorphic stream as 'ZipSerialM'.
--
-- @since 0.2.0
zipSerially :: IsStream t => ZipSerialM m a -> t m a
zipSerially = adapt

-- | Same as 'zipSerially'.
--
-- @since 0.1.0
{-# DEPRECATED zipping "Please use zipSerially instead." #-}
zipping :: IsStream t => ZipSerialM m a -> t m a
zipping = zipSerially

-- | Fix the type of a polymorphic stream as 'ZipAsyncM'.
--
-- @since 0.2.0
zipAsyncly :: IsStream t => ZipAsyncM m a -> t m a
zipAsyncly = adapt

-- | Same as 'zipAsyncly'.
--
-- @since 0.1.0
{-# DEPRECATED zippingAsync "Please use zipAsyncly instead." #-}
zippingAsync :: IsStream t => ZipAsyncM m a -> t m a
zippingAsync = zipAsyncly

-------------------------------------------------------------------------------
-- Running Streams, convenience functions specialized to types
-------------------------------------------------------------------------------

-- | Same as @runStream@.
--
-- @since 0.1.0
{-# DEPRECATED runStreamT "Please use runStream instead." #-}
runStreamT :: Monad m => SerialT m a -> m ()
runStreamT = runStream

-- | Same as @runStream . wSerially@.
--
-- @since 0.1.0
{-# DEPRECATED runInterleavedT "Please use 'runStream . interleaving' instead." #-}
runInterleavedT :: Monad m => InterleavedT m a -> m ()
runInterleavedT = runStream . wSerially

-- | Same as @runStream . asyncly@.
--
-- @since 0.1.0
{-# DEPRECATED runAsyncT "Please use 'runStream . asyncly' instead." #-}
runAsyncT :: Monad m => AsyncT m a -> m ()
runAsyncT = runStream . asyncly

-- | Same as @runStream . parallely@.
--
-- @since 0.1.0
{-# DEPRECATED runParallelT "Please use 'runStream . parallely' instead." #-}
runParallelT :: Monad m => ParallelT m a -> m ()
runParallelT = runStream . parallely

-- | Same as @runStream . zipping@.
--
-- @since 0.1.0
{-# DEPRECATED runZipStream "Please use 'runStream . zipSerially instead." #-}
runZipStream :: Monad m => ZipSerialM m a -> m ()
runZipStream = runStream . zipSerially

-- | Same as @runStream . zippingAsync@.
--
-- @since 0.1.0
{-# DEPRECATED runZipAsync "Please use 'runStream . zipAsyncly instead." #-}
runZipAsync :: Monad m => ZipAsyncM m a -> m ()
runZipAsync = runStream . zipAsyncly

------------------------------------------------------------------------------
-- IO Streams
------------------------------------------------------------------------------

-- | A serial IO stream of elements of type @a@. See 'SerialT' documentation
-- for more details.
--
-- @since 0.2.0
type Serial a = SerialT IO a

-- | An interleaving serial IO stream of elements of type @a@. See 'WSerialT'
-- documentation for more details.
--
-- @since 0.2.0
type WSerial a = WSerialT IO a

-- | A serial IO stream of elements of type @a@ with concurrent lookahead.  See
-- 'AheadT' documentation for more details.
--
-- @since 0.3.0
type Ahead a = AheadT IO a

-- | A demand driven left biased parallely composing IO stream of elements of
-- type @a@.  See 'AsyncT' documentation for more details.
--
-- @since 0.2.0
type Async a = AsyncT IO a

-- | A round robin parallely composing IO stream of elements of type @a@.
-- See 'WAsyncT' documentation for more details.
--
-- @since 0.2.0
type WAsync a = WAsyncT IO a

-- | A parallely composing IO stream of elements of type @a@.
-- See 'ParallelT' documentation for more details.
--
-- @since 0.2.0
type Parallel a = ParallelT IO a

-- | An IO stream whose applicative instance zips streams serially.
--
-- @since 0.2.0
type ZipSerial a = ZipSerialM IO a

-- | An IO stream whose applicative instance zips streams wAsyncly.
--
-- @since 0.2.0
type ZipAsync a = ZipAsyncM IO a

------------------------------------------------------------------------------
-- Fold Utilities
------------------------------------------------------------------------------

-- | A variant of 'Data.Foldable.fold' that allows you to fold a 'Foldable'
-- container of streams using the specified stream sum operation.
--
-- @foldWith 'async' $ map return [1..3]@
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
-- @foldMapWith 'async' return [1..3]@
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
