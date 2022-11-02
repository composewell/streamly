-- |
-- Module      : Streamly.Internal.Data.Fold.Extra
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Data.Fold.Extra
    (
    -- * Imports
    -- $setup

      countDistinct
    , countDistinctInt
    , frequency
    , toMap
    , nub
    , nubInt

    -- ** Demultiplexing
    -- | Direct values in the input stream to different folds using an n-ary
    -- fold selector. 'demux' is a generalization of 'classify' (and
    -- 'partition') where each key of the classifier can use a different fold.
    , demux
    , demuxWith
    , demuxScanWith
    , demuxScanMutWith
    , demuxMutWith

    -- TODO: These can be implemented using the above operations
    -- , demuxWithSel -- Stop when the fold for the specified key stops
    -- , demuxWithMin -- Stop when any of the folds stop
    -- , demuxWithAll -- Stop when all the folds stop (run once)

    -- ** Classifying
    -- | In an input stream of key value pairs fold values for different keys
    -- in individual output buckets using the given fold. 'classify' is a
    -- special case of 'demux' where all the branches of the demultiplexer use
    -- the same fold.
    --
    -- Different types of maps can be used with these combinators via the IsMap
    -- type class. Hashmap performs better when there are more collisions, trie
    -- Map performs better otherwise. Trie has an advantage of sorting the keys
    -- at the same time.  For example if we want to store a dictionary of words
    -- and their meanings then trie Map would be better if we also want to
    -- display them in sorted order.

    , classify
    , classifyWith
    , classifyMutWith
    , classifyScanWith
    -- , classifyWithSel
    -- , classifyWithMin
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Streamly.Internal.Data.IsMap (IsMap(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))

import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Prelude
import qualified Streamly.Internal.Data.IsMap as IsMap

import Prelude hiding
       ( filter, foldl1, drop, dropWhile, take, takeWhile, zipWith
       , foldl, foldr, map, mapM_, sequence, all, any, sum, product, elem
       , notElem, maximum, minimum, head, last, tail, length, null
       , reverse, iterate, init, and, or, lookup, (!!)
       , scanl, scanl1, replicate, concatMap, mconcat, foldMap, unzip
       , span, splitAt, break, mapM, zip, maybe)
import Streamly.Internal.Data.Fold

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Fold.Extra as Fold

-- XXX Name as nubOrd? Or write a nubGeneric

-- | Used as a scan. Returns 'Just' for the first occurrence of an element,
-- returns 'Nothing' for any other occurrences.
--
-- Example:
--
-- >>> stream = Stream.fromList [1::Int,1,2,3,4,4,5,1,5,7]
-- >>> Stream.fold Fold.toList $ Stream.scanMaybe Fold.nub stream
-- [1,2,3,4,5,7]
--
-- /Pre-release/
{-# INLINE nub #-}
nub :: (Monad m, Ord a) => Fold m a (Maybe a)
nub = fmap (\(Tuple' _ x) -> x) $ foldl' step initial

    where

    initial = Tuple' Set.empty Nothing

    step (Tuple' set _) x =
        if Set.member x set
        then Tuple' set Nothing
        else Tuple' (Set.insert x set) (Just x)

-- | Like 'nub' but specialized to a stream of 'Int', for better performance.
--
-- Definition:
--
-- >>> nubInt = Fold.nub
--
-- /Pre-release/
{-# INLINE nubInt #-}
nubInt :: Monad m => Fold m Int (Maybe Int)
nubInt = fmap (\(Tuple' _ x) -> x) $ foldl' step initial

    where

    initial = Tuple' IntSet.empty Nothing

    step (Tuple' set _) x =
        if IntSet.member x set
        then Tuple' set Nothing
        else Tuple' (IntSet.insert x set) (Just x)

-- XXX Try Hash set
-- XXX Add a countDistinct window fold
-- XXX Add a bloom filter fold

-- | Count non-duplicate elements in the stream.
--
-- Definition:
--
-- >>> countDistinct = Fold.postscan Fold.nub $ Fold.catMaybes $ Fold.length
--
-- The memory used is proportional to the number of distinct elements in the
-- stream, to guard against using too much memory use it as a scan and
-- terminate if the count reaches more than a threshold.
--
-- /Space/: \(\mathcal{O}(n)\)
--
-- /Pre-release/
--
{-# INLINE countDistinct #-}
countDistinct :: (Monad m, Ord a) => Fold m a Int
countDistinct = postscan nub $ catMaybes length
{-
countDistinct = fmap (\(Tuple' _ n) -> n) $ foldl' step initial

    where

    initial = Tuple' Set.empty 0

    step (Tuple' set n) x = do
        if Set.member x set
        then
            Tuple' set n
        else
            let cnt = n + 1
             in Tuple' (Set.insert x set) cnt
-}

-- | Like 'countDistinct' but specialized to a stream of 'Int', for better
-- performance.
--
-- Definition:
--
-- >>> countDistinctInt = Fold.postscan Fold.nubInt $ Fold.catMaybes $ Fold.length
--
-- /Pre-release/
{-# INLINE countDistinctInt #-}
countDistinctInt :: Monad m => Fold m Int Int
countDistinctInt = postscan nubInt $ catMaybes length
{-
countDistinctInt = fmap (\(Tuple' _ n) -> n) $ foldl' step initial

    where

    initial = Tuple' IntSet.empty 0

    step (Tuple' set n) x = do
        if IntSet.member x set
        then
            Tuple' set n
        else
            let cnt = n + 1
             in Tuple' (IntSet.insert x set) cnt
 -}

------------------------------------------------------------------------------
-- demux: in a key value stream fold each key sub-stream with a different fold
------------------------------------------------------------------------------

-- TODO Demultiplex an input element into a number of typed variants. We want
-- to statically restrict the target values within a set of predefined types,
-- an enumeration of a GADT.
--
-- This is the consumer side dual of the producer side 'mux' operation (XXX to
-- be implemented).
--
-- XXX If we use Refold in it, it can perhaps fuse/be more efficient. For
-- example we can store just the result rather than storing the whole fold in
-- the Map.
--
-- Note: There are separate functions to determine Key and Fold from the input
-- because key is to be determined on each input whereas fold is to be
-- determined only once for a key.

-- | In a key value stream, fold values corresponding to each key with a key
-- specific fold. The fold returns the fold result as the second component of
-- the output tuple whenever a fold terminates. The first component of the
-- tuple is a container of in-progress folds. If a fold terminates, another
-- instance of the fold is started upon receiving an input with that key.
--
-- This can be used to scan a stream and collect the results from the scan
-- output.
--
-- /Pre-release/
--
{-# INLINE demuxScanWith #-}
demuxScanWith :: (Monad m, IsMap f, Traversable f) =>
       (a -> Key f)
    -> (a -> m (Fold m a b))
    -> Fold m a (m (f b), Maybe (Key f, b))
demuxScanWith getKey getFold = fmap extract $ foldlM' step initial

    where

    initial = return $ Tuple' IsMap.mapEmpty Nothing

    {-# INLINE runFold #-}
    runFold kv (Fold step1 initial1 extract1) (k, a) = do
         res <- initial1
         case res of
            Partial s -> do
                res1 <- step1 s a
                return
                    $ case res1 of
                        Partial _ ->
                            let fld = Fold step1 (return res1) extract1
                             in Tuple' (IsMap.mapInsert k fld kv) Nothing
                        Done b -> Tuple' (IsMap.mapDelete k kv) (Just (k, b))
            Done b -> return $ Tuple' kv (Just (k, b))

    step (Tuple' kv _) a = do
        let k = getKey a
        case IsMap.mapLookup k kv of
            Nothing -> do
                fld <- getFold a
                runFold kv fld (k, a)
            Just f -> runFold kv f (k, a)

    extract (Tuple' kv x) = (Prelude.mapM f kv, x)

        where

        f (Fold _ i e) = do
            r <- i
            case r of
                Partial s -> e s
                Done b -> return b

-- | This is specialized version of 'demuxScanWith' that uses mutable cells as
-- fold accumulators for better performance.
--
-- Definition:
--
-- >>> demuxScanMutWith = Fold.demuxScanWith
--
{-# INLINE demuxScanMutWith #-}
demuxScanMutWith :: (MonadIO m, IsMap f, Traversable f) =>
       (a -> Key f)
    -> (a -> m (Fold m a b))
    -> Fold m a (m (f b), Maybe (Key f, b))
demuxScanMutWith getKey getFold = fmap extract $ foldlM' step initial

    where

    initial = return $ Tuple' IsMap.mapEmpty Nothing

    {-# INLINE initFold #-}
    initFold kv (Fold step1 initial1 extract1) (k, a) = do
         res <- initial1
         case res of
            Partial s -> do
                res1 <- step1 s a
                case res1 of
                    Partial _ -> do
                        let fld = Fold step1 (return res1) extract1
                        ref <- liftIO $ newIORef fld
                        return $ Tuple' (IsMap.mapInsert k ref kv) Nothing
                    Done b -> return $ Tuple' kv (Just (k, b))
            Done b -> return $ Tuple' kv (Just (k, b))

    {-# INLINE runFold #-}
    runFold kv ref (Fold step1 initial1 extract1) (k, a) = do
         res <- initial1
         case res of
            Partial s -> do
                res1 <- step1 s a
                case res1 of
                        Partial _ -> do
                            let fld = Fold step1 (return res1) extract1
                            liftIO $ writeIORef ref fld
                            return $ Tuple' kv Nothing
                        Done b ->
                            let kv1 = IsMap.mapDelete k kv
                             in return $ Tuple' kv1 (Just (k, b))
            Done _ -> error "demuxScanMutWith: unreachable"

    step (Tuple' kv _) a = do
        let k = getKey a
        case IsMap.mapLookup k kv of
            Nothing -> do
                f <- getFold a
                initFold kv f (k, a)
            Just ref -> do
                f <- liftIO $ readIORef ref
                runFold kv ref f (k, a)

    extract (Tuple' kv x) = (Prelude.mapM f kv, x)

        where

        f ref = do
            (Fold _ i e) <- liftIO $ readIORef ref
            r <- i
            case r of
                Partial s -> e s
                Done b -> return b

-- | This collects all the results of 'demuxScanWith' in a container.
--
{-# INLINE demuxWith #-}
demuxWith :: (Monad m, IsMap f, Traversable f) =>
    (a -> Key f) -> (a -> m (Fold m a b)) -> Fold m a (f b)
demuxWith getKey getFold =
    let
        classifier = demuxScanWith getKey getFold
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst latest)
                (lmap snd $ catMaybes toMap)
    in postscan classifier aggregator

-- | Same as 'demuxWith' but uses 'demuxScanMutWith' for better performance.
--
-- Definition:
--
-- >>> demuxMutWith = Fold.demuxWith
--
{-# INLINE demuxMutWith #-}
demuxMutWith :: (MonadIO m, IsMap f, Traversable f) =>
    (a -> Key f) -> (a -> m (Fold m a b)) -> Fold m a (f b)
demuxMutWith getKey getFold =
    let
        classifier = demuxScanMutWith getKey getFold
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst latest)
                (lmap snd $ catMaybes toMap)
    in postscan classifier aggregator

-- | Fold a stream of key value pairs using a function that maps keys to folds.
--
-- Definition:
--
-- >>> demux f = Fold.demuxWith fst (Fold.lmap snd . f)
--
-- Example:
--
-- >>> import Data.Map (Map)
-- >>> :{
--  let f "SUM" = return Fold.sum
--      f _ = return Fold.product
--      input = Stream.fromList [("SUM",1),("PRODUCT",2),("SUM",3),("PRODUCT",4)]
--   in Stream.fold (Fold.demux f) input :: IO (Map String Int)
-- :}
-- fromList [("PRODUCT",8),("SUM",4)]
--
-- /Pre-release/
{-# INLINE demux #-}
demux :: (Monad m, IsMap f, Traversable f) =>
    (Key f -> m (Fold m a b)) -> Fold m (Key f, a) (f b)
demux f = demuxWith fst (\(k, _) -> fmap (lmap snd) (f k))

------------------------------------------------------------------------------
-- Classify: Like demux but uses the same fold for all keys.
------------------------------------------------------------------------------

-- XXX Change these to make the behavior similar to demux* variants. We can
-- implement this using classifyScanManyWith. Maintain a set of done folds in
-- the underlying monad, and when initial is called look it up, if the fold is
-- done then initial would set a flag in the state to ignore the input or
-- return an error.

-- | Folds the values for each key using the supplied fold. When scanning, as
-- soon as the fold is complete, its result is available in the second
-- component of the tuple.  The first component of the tuple is a snapshot of
-- the in-progress folds.
--
-- Once the fold for a key is done, any future values of the key are ignored.
--
-- Definition:
--
-- >>> classifyScanWith f fld = Fold.demuxScanWith f (const fld)
--
{-# INLINE classifyScanWith #-}
classifyScanWith :: (Monad m, IsMap f, Traversable f, Ord (Key f)) =>
    -- Note: we need to return the Map itself to display the in-progress values
    -- e.g. to implement top. We could possibly create a separate abstraction
    -- for that use case. We return an action because we want it to be lazy so
    -- that the downstream consumers can choose to process or discard it.
    (a -> Key f) -> Fold m a b -> Fold m a (m (f b), Maybe (Key f, b))
classifyScanWith f (Fold step1 initial1 extract1) =
    fmap extract $ foldlM' step initial

    where

    initial = return $ Tuple3' IsMap.mapEmpty Set.empty Nothing

    {-# INLINE initFold #-}
    initFold kv set k a = do
        x <- initial1
        case x of
              Partial s -> do
                r <- step1 s a
                return
                    $ case r of
                          Partial s1 ->
                            Tuple3' (IsMap.mapInsert k s1 kv) set Nothing
                          Done b ->
                            Tuple3' kv set (Just (k, b))
              Done b -> return (Tuple3' kv (Set.insert k set) (Just (k, b)))

    step (Tuple3' kv set _) a = do
        let k = f a
        case IsMap.mapLookup k kv of
            Nothing -> do
                if Set.member k set
                then return (Tuple3' kv set Nothing)
                else initFold kv set k a
            Just s -> do
                r <- step1 s a
                return
                    $ case r of
                          Partial s1 ->
                            Tuple3' (IsMap.mapInsert k s1 kv) set Nothing
                          Done b ->
                            let kv1 = IsMap.mapDelete k kv
                             in Tuple3' kv1 (Set.insert k set) (Just (k, b))

    extract (Tuple3' kv _ x) = (Prelude.mapM extract1 kv, x)

-- XXX we can use a Prim IORef if we can constrain the state "s" to be Prim
--
-- The code is almost the same as classifyScanWith except the IORef operations.

-- | Same as classifyScanWith except that it uses mutable IORef cells in the
-- Map providing better performance. Be aware that if this is used as a scan,
-- the values in the intermediate Maps would be mutable.
--
-- Definitions:
--
-- >>> classifyScanMutWith = Fold.classifyScanWith
-- >>> classifyScanMutWith f fld = Fold.demuxScanMutWith f (const fld)
--
{-# INLINE classifyScanMutWith #-}
classifyScanMutWith :: (MonadIO m, IsMap f, Traversable f, Ord (Key f)) =>
    (a -> Key f) -> Fold m a b -> Fold m a (m (f b), Maybe (Key f, b))
classifyScanMutWith f (Fold step1 initial1 extract1) =
    fmap extract $ foldlM' step initial

    where

    initial = return $ Tuple3' IsMap.mapEmpty Set.empty Nothing

    {-# INLINE initFold #-}
    initFold kv set k a = do
        x <- initial1
        case x of
              Partial s -> do
                r <- step1 s a
                case r of
                      Partial s1 -> do
                        ref <- liftIO $ newIORef s1
                        return $ Tuple3' (IsMap.mapInsert k ref kv) set Nothing
                      Done b ->
                        return $ Tuple3' kv set (Just (k, b))
              Done b -> return (Tuple3' kv (Set.insert k set) (Just (k, b)))

    step (Tuple3' kv set _) a = do
        let k = f a
        case IsMap.mapLookup k kv of
            Nothing -> do
                if Set.member k set
                then return (Tuple3' kv set Nothing)
                else initFold kv set k a
            Just ref -> do
                s <- liftIO $ readIORef ref
                r <- step1 s a
                case r of
                      Partial s1 -> do
                        liftIO $ writeIORef ref s1
                        return $ Tuple3' kv set Nothing
                      Done b ->
                        let kv1 = IsMap.mapDelete k kv
                         in return
                                $ Tuple3' kv1 (Set.insert k set) (Just (k, b))

    extract (Tuple3' kv _ x) =
        (Prelude.mapM (\ref -> liftIO (readIORef ref) >>= extract1) kv, x)

-- | Fold a key value stream to a key-value container. If the same key appears
-- multiple times, only the last value is retained.
{-# INLINE toMap #-}
toMap :: (Monad m, IsMap f) => Fold m (Key f, a) (f a)
toMap = foldl' (\kv (k, v) -> IsMap.mapInsert k v kv) IsMap.mapEmpty

-- | Split the input stream based on a key field and fold each split using the
-- given fold. Useful for map/reduce, bucketizing the input in different bins
-- or for generating histograms.
--
-- Example:
--
-- >>> import Data.Map.Strict (Map)
-- >>> :{
--  let input = Stream.fromList [("ONE",1),("ONE",1.1),("TWO",2), ("TWO",2.2)]
--      classify = Fold.classifyWith fst (Fold.lmap snd Fold.toList)
--   in Stream.fold classify input :: IO (Map String [Double])
-- :}
-- fromList [("ONE",[1.0,1.1]),("TWO",[2.0,2.2])]
--
-- Once the classifier fold terminates for a particular key any further inputs
-- in that bucket are ignored.
--
-- Space used is proportional to the number of keys seen till now and
-- monotonically increases because it stores whether a key has been seen or
-- not.
--
-- /Stops: never/
--
-- /Pre-release/
--
{-# INLINE classifyWith #-}
classifyWith :: (Monad m, IsMap f, Traversable f, Ord (Key f)) =>
    (a -> Key f) -> Fold m a b -> Fold m a (f b)
classifyWith f fld =
    let
        classifier = classifyScanWith f fld
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst latest)
                (lmap snd $ catMaybes toMap)
    in postscan classifier aggregator

-- | Same as 'classifyWith' but maybe faster because it uses mutable cells as
-- fold accumulators in the Map.
--
-- Definition:
--
-- >>> classifyMutWith = Fold.classifyWith
--
{-# INLINE classifyMutWith #-}
classifyMutWith :: (MonadIO m, IsMap f, Traversable f, Ord (Key f)) =>
    (a -> Key f) -> Fold m a b -> Fold m a (f b)
classifyMutWith f fld =
    let
        classifier = classifyScanMutWith f fld
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst latest)
                (lmap snd $ catMaybes toMap)
    in postscan classifier aggregator

-- | Given an input stream of key value pairs and a fold for values, fold all
-- the values belonging to each key.  Useful for map/reduce, bucketizing the
-- input in different bins or for generating histograms.
--
-- Definition:
--
-- >>> classify = Fold.classifyWith fst . Fold.lmap snd
--
-- Example:
--
-- >>> :{
--  let input = Stream.fromList [("ONE",1),("ONE",1.1),("TWO",2), ("TWO",2.2)]
--   in Stream.fold (Fold.classify Fold.toList) input
-- :}
-- fromList [("ONE",[1.0,1.1]),("TWO",[2.0,2.2])]
--
-- /Pre-release/
{-# INLINE classify #-}
classify :: (Monad m, Ord k) => Fold m a b -> Fold m (k, a) (Map k b)
classify = classifyWith fst . lmap snd

-- | Determine the frequency of each element in the stream.
--
-- You can just collect the keys of the resulting map to get the unique
-- elements in the stream.
--
-- Definition:
--
-- >>> frequency = Fold.classifyWith id Fold.length
--
{-# INLINE frequency #-}
frequency :: (Monad m, Ord a) => Fold m a (Map a Int)
frequency = classifyWith id length
