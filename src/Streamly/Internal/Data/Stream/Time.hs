-- |
-- Module      : Streamly.Internal.Data.Stream.Time
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.Time
    (
    -- * Imports for Examples
    -- $setup

    -- * Timers
      periodic -- XXX Should go to Streamly.Data.Stream
    , ticks -- XXX Should go to Streamly.Data.Stream
    , ticksRate
    , interject

    -- * Trimming
    , takeInterval
    , takeLastInterval
    , dropInterval
    , dropLastInterval

    -- * Chunking
    , intervalsOf
    , groupsOfTimeout

    -- * Sampling
    , sampleIntervalEnd
    , sampleIntervalStart
    , sampleBurst
    , sampleBurstEnd
    , sampleBurstStart

    -- * Windowed Sessions
    , classifySessionsByGeneric
    , classifySessionsBy
    , classifySessionsOf
    , classifyKeepAliveSessions

    -- XXX This should go in the concurrent module
    -- * Buffering
    -- | Evaluate strictly using a buffer of results.  When the buffer becomes
    -- full we can block, drop the new elements, drop the oldest element and
    -- insert the new at the end.
    , bufferLatest
    , bufferLatestN
    , bufferOldestN
    )
where

import Control.Concurrent (threadDelay)
import Control.Exception (assert)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Heap (Entry(..))
import Data.Kind (Type)
import Data.Map (Map)
import Data.Maybe (isNothing)
import Data.Proxy (Proxy(..))
import Streamly.Data.Fold (Fold)
import Streamly.Internal.Data.Fold (Fold (..))
import Streamly.Internal.Data.IsMap (IsMap(..))
import Streamly.Internal.Data.Stream (Stream)
import Streamly.Internal.Data.Time.Units
    ( AbsTime
    , MilliSecond64(..)
    , addToAbsTime
    , toAbsTime
    , toRelTime
    )
import Streamly.Internal.Data.Time.Units (NanoSecond64(..), toRelTime64)

import qualified Data.Heap as H
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Internal.Data.Fold as Fold (Step(..))
import qualified Streamly.Internal.Data.IsMap as IsMap
import qualified Streamly.Internal.Data.Stream as Stream
    ( scanlMAfter'
    , timeIndexed
    , timestamped
    )

import Streamly.Internal.Data.Stream.Concurrent

-- $setup
--
-- Imports for example snippets in this module.
--
-- >>> :m
-- >>> import Control.Concurrent (threadDelay)
-- >>> import qualified Streamly.Data.Array as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Data.Stream.Prelude as Stream
-- >>> import qualified Streamly.Internal.Data.Stream as Stream (delayPost, timestamped)
-- >>> import qualified Streamly.Internal.Data.Stream.Concurrent as Stream (parListEagerFst)
-- >>> import qualified Streamly.Internal.Data.Stream.Time as Stream
-- >>> import Prelude hiding (concatMap, concat)
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

-- | Generate a stream by running an action periodically at the specified time
-- interval.
--
{-# INLINE periodic #-}
periodic :: MonadIO m => m a -> Double -> Stream m a
periodic action n = Stream.repeatM timed

    where

    timed = liftIO (threadDelay (round $ n * 1000000)) >> action

-- | Generate a tick stream consisting of '()' elements, each tick is generated
-- after the specified time delay given in seconds.
--
-- >>> ticks = Stream.periodic (return ())
--
{-# INLINE ticks #-}
ticks :: MonadIO m => Double -> Stream m ()
ticks = periodic (return ())

-- | Generate a tick stream, ticks are generated at the specified 'Rate'. The
-- rate is adaptive, the tick generation speed can be increased or decreased at
-- different times to achieve the specified rate.  The specific behavior for
-- different styles of 'Rate' specifications is documented under 'Rate'.  The
-- effective maximum rate achieved by a stream is governed by the processor
-- speed.
--
-- >>> tickStream = Stream.repeatM (return ())
-- >>> ticksRate r = Stream.parEval (Stream.rate (Just r)) tickStream
--
{-# INLINE ticksRate #-}
ticksRate :: MonadAsync m => Rate -> Stream m ()
ticksRate r = parEval (rate (Just r)) $ Stream.repeatM (return ())

-- XXX The case when the interval is 0, we should run only the stream being
-- interjected.

-- | Intersperse a monadic action into the input stream after every @n@
-- seconds.
--
-- Definition:
--
-- >>> interject n f xs = Stream.parListEagerFst [xs, Stream.periodic f n]
--
-- Example:
--
-- >>> s = Stream.fromList "hello"
-- >>> input = Stream.mapM (\x -> threadDelay 1000000 >> putChar x) s
-- >>> Stream.fold Fold.drain $ Stream.interject (putChar ',') 1.05 input
-- h,e,l,l,o
--
{-# INLINE interject #-}
interject :: MonadAsync m => m a -> Double -> Stream m a -> Stream m a
interject f n xs = parListEagerFst [xs, periodic f n]

-- XXX No element should be yielded if the duration is zero.

-- | @takeInterval interval@ runs the stream only upto the specified time
-- @interval@ in seconds.
--
-- The interval starts when the stream is evaluated for the first time.
--
{-# INLINE takeInterval #-}
takeInterval :: MonadAsync m => Double -> Stream m a -> Stream m a
takeInterval d =
    Stream.catMaybes
        . Stream.takeWhile isNothing
        . interject (return Nothing) d . fmap Just

-- | Take time interval @i@ seconds at the end of the stream.
--
-- O(n) space, where n is the number elements taken.
--
-- /Unimplemented/
{-# INLINE takeLastInterval #-}
takeLastInterval :: -- MonadAsync m =>
    Double -> Stream m a -> Stream m a
takeLastInterval = undefined

-- XXX All elements should be yielded if the duration is zero.

-- | @dropInterval interval@ drops all the stream elements that are generated
-- before the specified @interval@ in seconds has passed.
--
-- The interval begins when the stream is evaluated for the first time.
--
{-# INLINE dropInterval #-}
dropInterval :: MonadAsync m => Double -> Stream m a -> Stream m a
dropInterval d =
    Stream.catMaybes
        . Stream.dropWhile isNothing
        . interject (return Nothing) d . fmap Just

-- | Drop time interval @i@ seconds at the end of the stream.
--
-- O(n) space, where n is the number elements dropped.
--
-- /Unimplemented/
{-# INLINE dropLastInterval #-}
dropLastInterval :: -- MonadAsync m =>
    Int -> Stream m a -> Stream m a
dropLastInterval = undefined

-- | Group the input stream into windows of @n@ second each and then fold each
-- group using the provided fold function.
--
-- >>> twoPerSec = Stream.parEval (Stream.constRate 2) $ Stream.enumerateFrom 1
-- >>> intervals = Stream.intervalsOf 1 Fold.toList twoPerSec
-- >>> Stream.fold Fold.toList $ Stream.take 2 intervals
-- [...,...]
--
{-# INLINE intervalsOf #-}
intervalsOf :: MonadAsync m => Double -> Fold m a b -> Stream m a -> Stream m b
intervalsOf n f xs =
    Stream.foldMany
        (Fold.takeEndBy isNothing (Fold.catMaybes f))
        (interject (return Nothing) n (fmap Just xs))

-- XXX This can be implemented more efficiently by sharing a Clock.Timer across
-- parallel threads and resetting it whenever a span is emitted.

-- | Like 'chunksOf' but if the chunk is not completed within the specified
-- time interval then emit whatever we have collected till now. The chunk
-- timeout is reset whenever a chunk is emitted. The granularity of the clock
-- is 100 ms.
--
-- >>> s = Stream.delayPost 0.3 $ Stream.fromList [1..1000]
-- >>> f = Stream.fold (Fold.drainMapM print) $ Stream.groupsOfTimeout 5 1 Fold.toList s
--
-- /Pre-release/
{-# INLINE groupsOfTimeout #-}
groupsOfTimeout :: MonadAsync m
    => Int -> Double -> Fold m a b -> Stream m a -> Stream m b
groupsOfTimeout n timeout f =
      fmap snd
    . classifySessionsBy
        0.1 False (const (return False)) timeout (Fold.take n f)
    . Stream.timestamped
    . fmap ((),)

------------------------------------------------------------------------------
-- Windowed classification
------------------------------------------------------------------------------

-- TODO: To mark the position in space or time we can have Indexed or
-- TimeStamped types. That can make it easy to deal with the position indices
-- or timestamps.

------------------------------------------------------------------------------
-- Keyed Sliding Windows
------------------------------------------------------------------------------

{-
{-# INLINABLE classifySlidingChunks #-}
classifySlidingChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Int              -- ^ window slide
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifySlidingChunks wsize wslide (Fold step initial extract) str
    = undefined

-- XXX Another variant could be to slide the window on an event, e.g. in TCP we
-- slide the send window when an ack is received and we slide the receive
-- window when a sequence is complete. Sliding is stateful in case of TCP,
-- sliding releases the send buffer or makes data available to the user from
-- the receive buffer.
{-# INLINABLE classifySlidingSessions #-}
classifySlidingSessions
    :: (IsStream t, MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Double         -- ^ time window size
    -> Double         -- ^ window slide
    -> Fold m a b     -- ^ Fold to be applied to window events
    -> t m (k, a, Bool, AbsTime) -- ^ window key, data, close flag, timestamp
    -> t m (k, b)
classifySlidingSessions tick interval slide (Fold step initial extract) str
    = undefined
-}

------------------------------------------------------------------------------
-- Sliding Window Buffers
------------------------------------------------------------------------------

-- These buffered versions could be faster than concurrent incremental folds of
-- all overlapping windows as in many cases we may not need all the values to
-- compute the fold, we can just compute the result using the old value and new
-- value.  However, we may need the buffer once in a while, for example for
-- string search we usually compute the hash incrementally but when the hash
-- matches the hash of the pattern we need to compare the whole string.
--
-- XXX we should be able to implement sequence based splitting combinators
-- using this combinator.

{-
-- | Buffer n elements of the input in a ring buffer. When t new elements are
-- collected, slide the window to remove the same number of oldest elements,
-- insert the new elements, and apply an incremental fold on the sliding
-- window, supplying the outgoing elements, the new ring buffer as arguments.
slidingChunkBuffer
    :: (IsStream t, Monad m, Ord a, Unboxed a)
    => Int -- window size
    -> Int -- window slide
    -> Fold m (Ring a, Array a) b
    -> t m a
    -> t m b
slidingChunkBuffer = undefined

-- Buffer n seconds worth of stream elements of the input in a radix tree.
-- Every t seconds, remove the items that are older than n seconds, and apply
-- an incremental fold on the sliding window, supplying the outgoing elements,
-- and the new radix tree buffer as arguments.
slidingSessionBuffer
    :: (IsStream t, Monad m, Ord a, Unboxed a)
    => Int    -- window size
    -> Int    -- tick size
    -> Fold m (RTree a, Array a) b
    -> t m a
    -> t m b
slidingSessionBuffer = undefined
-}

------------------------------------------------------------------------------
-- Keyed Session Windows
------------------------------------------------------------------------------

{-
-- | Keyed variable size space windows. Close the window if we do not receive a
-- window event in the next "spaceout" elements.
{-# INLINABLE classifyChunksBy #-}
classifyChunksBy
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Bool  -- ^ reset the spaceout when a chunk window element is received
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyChunksBy spanout reset (Fold step initial extract) str = undefined

-- | Like 'classifyChunksOf' but the chunk size is reset if an element is
-- received within the chunk size window. The chunk gets closed only if no
-- element is received within the chunk window.
--
{-# INLINABLE classifyKeepAliveChunks #-}
classifyKeepAliveChunks
    :: (IsStream t, MonadAsync m, Ord k)
    => Int   -- ^ window spaceout (spread)
    -> Fold m a b       -- ^ Fold to be applied to chunk window elements
    -> t m (k, a, Bool) -- ^ chunk key, data, last element
    -> t m (k, b)
classifyKeepAliveChunks spanout = classifyChunksBy spanout True
-}

data SessionState t m f s b = SessionState
    { sessionCurTime :: !AbsTime  -- ^ time since last event
    , sessionEventTime :: !AbsTime -- ^ time as per last event
    -- We can use the Map size instead of maintaining a count, but if we have
    -- to switch to HashMap then it can be useful.
    , sessionCount :: !Int -- ^ total number of sessions in progress
    , sessionTimerHeap :: H.Heap (H.Entry AbsTime (Key f)) -- ^ heap for timeouts
    , sessionKeyValueMap :: f s -- ^ Stored sessions for keys
    , sessionOutputStream :: t (m :: Type -> Type) (Key f, b) -- ^ Completed sessions
    }

data SessionEntry s = LiveSession !AbsTime !s | ZombieSession

-- delete from map and output the fold accumulator
ejectEntry :: (Monad m, IsMap f) =>
    (acc -> m b)
    -> heap
    -> f entry
    -> Stream m (Key f, b)
    -> Int
    -> acc
    -> Key f
    -> m (heap, f entry, Stream m (Key f, b), Int)
ejectEntry extract hp mp out cnt acc key = do
    sess <- extract acc
    let out1 = Stream.cons (key, sess) out
    let mp1 = IsMap.mapDelete key mp
    return (hp, mp1, out1, cnt - 1)

{-# NOINLINE flush #-}
flush :: (Monad m, IsMap f) =>
       (s -> m b)
    -> SessionState Stream m f (SessionEntry s) b
    -> m (SessionState Stream m f (SessionEntry s) b)
flush extract session@SessionState{..} = do
    (hp', mp', out, count) <-
        ejectAll
            ( sessionTimerHeap
            , sessionKeyValueMap
            , Stream.nil
            , sessionCount
            )
    return $ session
        { sessionCount = count
        , sessionTimerHeap = hp'
        , sessionKeyValueMap = mp'
        , sessionOutputStream = out
        }

    where

    ejectAll (hp, mp, out, !cnt) = do
        let hres = H.uncons hp
        case hres of
            Just (Entry _ key, hp1) -> do
                r <- case IsMap.mapLookup key mp of
                    Nothing -> return (hp1, mp, out, cnt)
                    Just ZombieSession ->
                        return (hp1, IsMap.mapDelete key mp, out, cnt)
                    Just (LiveSession _ acc) ->
                        ejectEntry extract hp1 mp out cnt acc key
                ejectAll r
            Nothing -> do
                assert (IsMap.mapNull mp) (return ())
                return (hp, mp, out, cnt)

{-# NOINLINE ejectOne #-}
ejectOne :: (IsMap f, Monad m) =>
       Bool
    -> (acc -> m b)
    -> ( H.Heap (Entry AbsTime (Key f))
       , f (SessionEntry acc)
       , Stream m (Key f, b)
       , Int
       )
    -> m ( H.Heap (Entry AbsTime (Key f))
         , f (SessionEntry acc)
         , Stream m (Key f, b), Int
         )
ejectOne reset extract = go

    where

    go (hp, mp, out, !cnt) = do
        let hres = H.uncons hp
        case hres of
            Just (Entry expiry key, hp1) ->
                case IsMap.mapLookup key mp of
                    Nothing -> go (hp1, mp, out, cnt)
                    Just ZombieSession ->
                        go (hp1, IsMap.mapDelete key mp, out, cnt)
                    Just (LiveSession expiry1 acc) -> do
                        if not reset || expiry1 <= expiry
                        then ejectEntry extract hp1 mp out cnt acc key
                        else
                            -- reset the session timeout and continue
                            let hp2 = H.insert (Entry expiry1 key) hp1
                            in go (hp2, mp, out, cnt)
            Nothing -> do
                assert (IsMap.mapNull mp) (return ())
                return (hp, mp, out, cnt)

{-# NOINLINE ejectExpired #-}
ejectExpired :: (IsMap f, Monad m) =>
       Bool
    -> (Int -> m Bool)
    -> (acc -> m b)
    -> SessionState Stream m f (SessionEntry acc) b
    -> AbsTime
    -> m (SessionState Stream m f (SessionEntry acc) b)
ejectExpired reset ejectPred extract session@SessionState{..} curTime = do
    (hp', mp', out, count) <-
        ejectLoop
            sessionTimerHeap sessionKeyValueMap Stream.nil sessionCount
    return $ session
        { sessionCurTime = curTime
        , sessionCount = count
        , sessionTimerHeap = hp'
        , sessionKeyValueMap = mp'
        , sessionOutputStream = out
        }

    where

    ejectLoop hp mp out !cnt = do
        let hres = H.uncons hp
        case hres of
            Just (Entry expiry key, hp1) -> do
                (eject, force) <-
                    if curTime >= expiry
                    then return (True, False)
                    else do
                        r <- ejectPred cnt
                        return (r, r)
                if eject
                then
                    case IsMap.mapLookup key mp of
                        Nothing -> ejectLoop hp1 mp out cnt
                        Just ZombieSession ->
                            ejectLoop hp1 (IsMap.mapDelete key mp) out cnt
                        Just (LiveSession expiry1 acc) -> do
                            if expiry1 <= curTime || not reset || force
                            then do
                                (hp2,mp1,out1,cnt1) <-
                                    ejectEntry extract hp1 mp out cnt acc key
                                ejectLoop hp2 mp1 out1 cnt1
                            else
                                -- reset the session timeout and continue
                                let hp2 = H.insert (Entry expiry1 key) hp1
                                in ejectLoop hp2 mp out cnt
                else return (hp, mp, out, cnt)
            Nothing -> do
                assert (IsMap.mapNull mp) (return ())
                return (hp, mp, out, cnt)

-- XXX Use mutable IORef in accumulator
{-# INLINE classifySessionsByGeneric #-}
classifySessionsByGeneric
    :: forall m f a b. (MonadAsync m, IsMap f)
    => Proxy (f :: (Type -> Type))
    -> Double         -- ^ timer tick in seconds
    -> Bool           -- ^ reset the timer when an event is received
    -> (Int -> m Bool) -- ^ predicate to eject sessions based on session count
    -> Double         -- ^ session timeout in seconds
    -> Fold m a b  -- ^ Fold to be applied to session data
    -> Stream m (AbsTime, (Key f, a)) -- ^ timestamp, (session key, session
                                      -- data)
    -> Stream m (Key f, b) -- ^ session key, fold result
classifySessionsByGeneric _ tick reset ejectPred tmout
    (Fold step initial extract final) input =
    Stream.unfoldMany (Unfold.lmap sessionOutputStream Unfold.fromStream)
        $ Stream.scanlMAfter' sstep (return szero) (flush final)
        $ interject (return Nothing) tick
        $ fmap Just input

    where

    timeoutMs = toRelTime (round (tmout * 1000) :: MilliSecond64)
    tickMs = toRelTime (round (tick * 1000) :: MilliSecond64)
    szero = SessionState
        { sessionCurTime = toAbsTime (0 :: MilliSecond64)
        , sessionEventTime = toAbsTime (0 :: MilliSecond64)
        , sessionCount = 0
        , sessionTimerHeap = H.empty
        , sessionKeyValueMap = IsMap.mapEmpty :: f s
        , sessionOutputStream = Stream.nil
        }

    -- We can eject sessions based on the current session count to limit
    -- memory consumption. There are two possible strategies:
    --
    -- 1) Eject old sessions or sessions beyond a certain/lower timeout
    -- threshold even before timeout, effectively reduce the timeout.
    -- 2) Drop creation of new sessions but keep accepting new events for the
    -- old ones.
    --
    -- We use the first strategy as of now.

    -- Got a new stream input element
    sstep session@SessionState{..} (Just (timestamp, (key, value))) = do
        -- XXX instead of a heap we could use a timer wheel.
        --
        -- XXX if the key is an Int, we can also use an IntMap for slightly
        -- better performance.
        --
        -- How it works:
        --
        -- Values for each key are collected in a map using the supplied fold.
        -- When we insert a key in the Map we insert an entry into the heap as
        -- well with the session expiry as the sort key.  The Map entry
        -- consists of the fold result, and the expiry time of the session. If
        -- "reset" is True the expiry time is readjusted whenever a new event
        -- is processed. If the fold terminates and a new session is started
        -- for the same key the expiry time is set to the first timestamp of
        -- the new session.
        --
        -- The heap must have at most one entry for any given key. The heap is
        -- processed periodically to remove the expired entries.  We pick up an
        -- expired entry from the top of the heap and if the session has
        -- expired based on the expiry time in the Map entry then we remove the
        -- session from the Map and yield its fold output. Otherwise, we
        -- reinsert the entry into the heap based on the current expiry in the
        -- Map entry.
        --
        -- If an entry is removed from the Map and not removed from the heap
        -- and in the meantime it is inserted again in the Map (using the same
        -- key) then how do we avoid inserting multiple entries in the heap?
        -- For this reason we maintain the invariant that the Map entry is
        -- removed only when the heap entry is removed. Even if the fold has
        -- finished we still keep a dummy Map entry (ZombieSession) until the
        -- heap entry is removed. That way if we have a Map entry we do not
        -- insert a heap entry because we know it is already there.
        -- XXX The ZombieSession mechanism does not work as expected as we
        -- ignore ZombieSession when inserting a new entry. Anyway, we can
        -- remove this mechanism as at most only two heap entries may be
        -- created and they will be ultimately cleaned up.
        --
        -- Heap processing needs the map and map processing needs the heap,
        -- therefore we cannot separate the two for modularity unless we have a
        -- way to achieve mutual recursion.
        --
        let curTime = max sessionEventTime timestamp
            mOld = IsMap.mapLookup key sessionKeyValueMap
        let done fb = do
                -- deleting a key from the heap is expensive, so we never
                -- delete a key from heap, we just purge it from the Map and it
                -- gets purged from the heap on timeout. We just need an extra
                -- lookup in the Map when the key is purged from the heap, that
                -- should not be expensive.
                --
                let (mp, cnt) = case mOld of
                        Just (LiveSession _ _) ->
                            ( IsMap.mapInsert
                                key ZombieSession sessionKeyValueMap
                            , sessionCount - 1
                            )
                        _ -> (sessionKeyValueMap, sessionCount)
                return $ session
                    { sessionCurTime = curTime
                    , sessionEventTime = curTime
                    , sessionCount = cnt
                    , sessionKeyValueMap = mp
                    , sessionOutputStream = Stream.fromPure (key, fb)
                    }
            partial fs1 = do
                let expiry = addToAbsTime timestamp timeoutMs
                (hp1, mp1, out1, cnt1) <- do
                        let vars = (sessionTimerHeap, sessionKeyValueMap,
                                           Stream.nil, sessionCount)
                        case mOld of
                            -- inserting new entry
                            Nothing -> do
                                -- Eject a session from heap and map if needed
                                eject <- ejectPred sessionCount
                                (hp, mp, out, cnt) <-
                                    if eject
                                    then ejectOne reset extract vars
                                    else return vars

                                -- Insert the new session in heap
                                let hp' = H.insert (Entry expiry key) hp
                                 in return (hp', mp, out, cnt + 1)
                            -- updating old entry
                            Just _ -> return vars

                let acc = LiveSession expiry fs1
                    mp2 = IsMap.mapInsert key acc mp1
                return $ SessionState
                    { sessionCurTime = curTime
                    , sessionEventTime = curTime
                    , sessionCount = cnt1
                    , sessionTimerHeap = hp1
                    , sessionKeyValueMap = mp2
                    , sessionOutputStream = out1
                    }
        res0 <- do
            case mOld of
                Just (LiveSession _ acc) -> return $ Fold.Partial acc
                _ -> initial
        case res0 of
            Fold.Done _ ->
                error $ "classifySessionsBy: "
                    ++ "The supplied fold must consume at least one input"
            Fold.Partial fs -> do
                res <- step fs value
                case res of
                    Fold.Done fb -> done fb
                    Fold.Partial fs1 -> partial fs1

    -- Got a timer tick event
    sstep sessionState@SessionState{..} Nothing =
        let curTime = addToAbsTime sessionCurTime tickMs
        in ejectExpired reset ejectPred extract sessionState curTime

-- | @classifySessionsBy tick keepalive predicate timeout fold stream@
-- classifies an input event @stream@ consisting of  @(timestamp, (key,
-- value))@ into sessions based on the @key@, folding all the values
-- corresponding to the same key into a session using the supplied @fold@.
--
-- When the fold terminates or a @timeout@ occurs, a tuple consisting of the
-- session key and the folded value is emitted in the output stream. The
-- timeout is measured from the first event in the session.  If the @keepalive@
-- option is set to 'True' the timeout is reset to 0 whenever an event is
-- received.
--
-- The @timestamp@ in the input stream is an absolute time from some epoch,
-- characterizing the time when the input event was generated.  The notion of
-- current time is maintained by a monotonic event time clock using the
-- timestamps seen in the input stream. The latest timestamp seen till now is
-- used as the base for the current time.  When no new events are seen, a timer
-- is started with a clock resolution of @tick@ seconds. This timer is used to
-- detect session timeouts in the absence of new events.
--
-- To ensure an upper bound on the memory used the number of sessions can be
-- limited to an upper bound. If the ejection @predicate@ returns 'True', the
-- oldest session is ejected before inserting a new session.
--
-- When the stream ends any buffered sessions are ejected immediately.
--
-- If a session key is received even after a session has finished, another
-- session is created for that key.
--
-- >>> :{
-- Stream.fold (Fold.drainMapM print)
--     $ Stream.classifySessionsBy 1 False (const (return False)) 3 (Fold.take 3 Fold.toList)
--     $ Stream.timestamped
--     $ Stream.delay 0.1
--     $ Stream.fromList ((,) <$> [1,2,3] <*> ['a','b','c'])
-- :}
-- (1,"abc")
-- (2,"abc")
-- (3,"abc")
--
-- /Pre-release/
{-# INLINE classifySessionsBy #-}
classifySessionsBy
    :: (MonadAsync m, Ord k)
    => Double         -- ^ timer tick in seconds
    -> Bool           -- ^ reset the timer when an event is received
    -> (Int -> m Bool) -- ^ predicate to eject sessions based on session count
    -> Double         -- ^ session timeout in seconds
    -> Fold m a b  -- ^ Fold to be applied to session data
    -> Stream m (AbsTime, (k, a)) -- ^ timestamp, (session key, session data)
    -> Stream m (k, b) -- ^ session key, fold result
classifySessionsBy = classifySessionsByGeneric (Proxy :: Proxy (Map k))

-- | Same as 'classifySessionsBy' with a timer tick of 1 second and keepalive
-- option set to 'True'.
--
-- @
-- classifyKeepAliveSessions = classifySessionsBy 1 True
-- @
--
-- /Pre-release/
--
{-# INLINE classifyKeepAliveSessions #-}
classifyKeepAliveSessions ::
       (MonadAsync m, Ord k)
    => (Int -> m Bool) -- ^ predicate to eject sessions on session count
    -> Double -- ^ session inactive timeout
    -> Fold m a b -- ^ Fold to be applied to session payload data
    -> Stream m (AbsTime, (k, a)) -- ^ timestamp, (session key, session data)
    -> Stream m (k, b)
classifyKeepAliveSessions = classifySessionsBy 1 True

------------------------------------------------------------------------------
-- Keyed tumbling windows
------------------------------------------------------------------------------

-- Tumbling windows is a special case of sliding windows where the window slide
-- is the same as the window size. Or it can be a special case of session
-- windows where the reset flag is set to False.

-- XXX instead of using the early termination flag in the stream, we can use an
-- early terminating fold instead.

{-
-- | Split the stream into fixed size chunks of specified size. Within each
-- such chunk fold the elements in buckets identified by the keys. A particular
-- bucket fold can be terminated early if a closing flag is encountered in an
-- element for that key.
--
-- @since 0.7.0
{-# INLINABLE classifyChunksOf #-}
classifyChunksOf
    :: (IsStream t, MonadAsync m, Ord k)
    => Int              -- ^ window size
    -> Fold m a b       -- ^ Fold to be applied to window events
    -> t m (k, a, Bool) -- ^ window key, data, close event
    -> t m (k, b)
classifyChunksOf wsize = classifyChunksBy wsize False
-}

-- | Same as 'classifySessionsBy' with a timer tick of 1 second and keepalive
-- option set to 'False'.
--
-- >>> classifySessionsOf = Stream.classifySessionsBy 1 False
--
-- /Pre-release/
--
{-# INLINE classifySessionsOf #-}
classifySessionsOf ::
       (MonadAsync m, Ord k)
    => (Int -> m Bool) -- ^ predicate to eject sessions on session count
    -> Double -- ^ time window size
    -> Fold m a b -- ^ Fold to be applied to session data
    -> Stream m (AbsTime, (k, a)) -- ^ timestamp, (session key, session data)
    -> Stream m (k, b)
classifySessionsOf = classifySessionsBy 1 False

------------------------------------------------------------------------------
-- Sampling
------------------------------------------------------------------------------

-- | Continuously evaluate the input stream and sample the last event in each
-- time window of @n@ seconds.
--
-- This is also known as @throttle@ in some libraries.
--
-- >>> sampleIntervalEnd n = Stream.catMaybes . Stream.intervalsOf n Fold.latest
--
{-# INLINE sampleIntervalEnd #-}
sampleIntervalEnd :: MonadAsync m => Double -> Stream m a -> Stream m a
sampleIntervalEnd n = Stream.catMaybes . intervalsOf n Fold.latest

-- | Like 'sampleInterval' but samples at the beginning of the time window.
--
-- >>> sampleIntervalStart n = Stream.catMaybes . Stream.intervalsOf n Fold.one
--
{-# INLINE sampleIntervalStart #-}
sampleIntervalStart :: MonadAsync m => Double -> Stream m a -> Stream m a
sampleIntervalStart n = Stream.catMaybes . intervalsOf n Fold.one

data BurstState t x =
      BurstNone
    | BurstWait !t !x
    | BurstDone !x
    | BurstDoneNext !x !t !x

{-# INLINE sampleBurst #-}
sampleBurst :: MonadAsync m => Bool -> Double -> Stream m a -> Stream m a
sampleBurst sampleAtEnd gap xs =
    -- XXX Ideally we should schedule a timer event exactly after gap time,
    -- but the tick stream should work well as long as the timer
    -- granularity is small enough compared to the gap.
    Stream.mapMaybe extract
        $ Stream.scan (Fold.foldl' step BurstNone)
        $ Stream.timeIndexed
        $ interject (return Nothing) 0.01 (fmap Just xs)

    where

    gap1 = toRelTime64 (NanoSecond64 (round (gap * 10^(9::Int))))

    {-# INLINE step #-}
    step BurstNone (t1, Just x1) = BurstWait t1 x1
    step BurstNone _ = BurstNone

    step (BurstDone _) (t1, Just x1) = BurstWait t1 x1
    step (BurstDone _) _ = BurstNone

    step old@(BurstWait t0 x0) (t1, Nothing)
        | t1 - t0 >= gap1 = BurstDone x0
        | otherwise = old
    -- This can happen due to scheduling delays, if we received back to
    -- back events spaced by more than the timeout without an
    -- intervening timeout event then we emit the old event instead of
    -- replacing it by the new.
    step (BurstWait t0 x0) (t1, Just x1)
        | t1 - t0 >= gap1 = BurstDoneNext x0 t1 x1
        | sampleAtEnd = BurstWait t1 x1
        | otherwise = BurstWait t1 x0

    step (BurstDoneNext _ t0 x0) (t1, Nothing)
        | t1 - t0 >= gap1 = BurstDone x0
        | otherwise =  BurstWait t0 x0
    step (BurstDoneNext _ t0 x0) (t1, Just x1)
        | t1 - t0 >= gap1 = BurstDoneNext x0 t1 x1
        | sampleAtEnd = BurstWait t1 x1
        | otherwise = BurstWait t1 x0

    {-# INLINE extract #-}
    extract (BurstDoneNext x _ _) = Just x
    extract (BurstDone x) = Just x
    extract _ = Nothing

-- | Sample one event at the end of each burst of events.  A burst is a group
-- of events close together in time, it ends when an event is spaced by more
-- than the specified time interval (in seconds) from the previous event.
--
-- This is known as @debounce@ in some libraries.
--
-- The clock granularity is 10 ms.
--
{-# INLINE sampleBurstEnd #-}
sampleBurstEnd :: MonadAsync m => Double -> Stream m a -> Stream m a
sampleBurstEnd = sampleBurst True

-- | Like 'sampleBurstEnd' but samples the event at the beginning of the burst
-- instead of at the end of it.
--
{-# INLINE sampleBurstStart #-}
sampleBurstStart :: MonadAsync m => Double -> Stream m a -> Stream m a
sampleBurstStart = sampleBurst False

------------------------------------------------------------------------------
-- Lossy Buffering
------------------------------------------------------------------------------

-- XXX We could use 'maxBuffer Block/Drop/Rotate/Sample' instead. However we
-- may want to have the evaluation rate independent of the sampling rate. To
-- support that we can decouple evaluation and sampling in independent stages.
-- The sampling stage would strictly evaluate and sample, the evaluation stage
-- would control the evaluation rate.
--
-- bufferLatest - evaluate retaining only the latest element. Fork a thread
-- that keeps writing the stream to an IORef, another parallel thread reads
-- from the IORef.
--
-- bufferLatestN - Fork a thread that keeps writing to a sliding ring buffer,
-- and the stream unfolds the entire buffer when it is read, and the empty
-- buffer starts filling again in the same way.
--
-- bufferOldestN - Fork a thread that keeps writing to a buffer until it is
-- filled and keeps dropping once it is filled. When the stream is read the
-- buffer is unfolded to a stream and the empty buffer starts filling again.

-- | Evaluate the input stream continuously and keep only the oldest @n@
-- elements in the buffer, discard the new ones when the buffer is full.  When
-- the output stream is evaluated the collected buffer is streamed and the
-- buffer starts filling again.
--
-- /Unimplemented/
--
{-# INLINE bufferOldestN #-}
bufferOldestN :: -- MonadAsync m =>
    Int -> Stream m a -> Stream m a
bufferOldestN = undefined

-- | Evaluate the input stream continuously and keep only the latest @n@
-- elements in a ring buffer, keep discarding the older ones to make space for
-- the new ones.  When the output stream is evaluated the buffer collected till
-- now is streamed and it starts filling again.
--
-- /Unimplemented/
--
{-# INLINE bufferLatestN #-}
bufferLatestN :: -- MonadAsync m =>
    Int -> Stream m a -> Stream m a
bufferLatestN = undefined

-- | Always produce the latest available element from the stream without any
-- delay. The stream is continuously evaluated at the highest possible rate and
-- only the latest element is retained for sampling.
--
-- /Unimplemented/
--
{-# INLINE bufferLatest #-}
bufferLatest :: -- MonadAsync m =>
    Stream m a -> Stream m (Maybe a)
bufferLatest = undefined
