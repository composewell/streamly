{-# LANGUAGE CPP #-}
-- |
-- Module      : Streamly.Internal.Data.Scanl.Container
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Internal.Data.Scanl.Container
    (
    -- * Set operations
      toSet
    , toIntSet
    , countDistinct
    , countDistinctInt
    , nub
    , nubInt

    -- * Map operations
    -- , frequency

    -- ** Demultiplexing
    -- | Direct values in the input stream to different folds using an n-ary
    -- fold selector. 'demux' is a generalization of 'classify' (and
    -- 'partition') where each key of the classifier can use a different fold.
    --
    -- You need to see only 'demux' if you are looking to find the capabilities
    -- of these combinators, all others are variants of that.

    {-
    -- *** Output is a container
    -- | The fold state snapshot returns the key-value container of in-progress
    -- folds.
    , demuxToContainer
    , demuxToContainerIO
    , demuxToMap
    , demuxToMapIO

    -- *** Input is explicit key-value tuple
    -- | Like above but inputs are in explicit key-value pair form.
    , demuxKvToContainer
    , demuxKvToMap

    -- *** Scan of finished fold results
    -- | Like above, but the resulting fold state snapshot contains the key
    -- value container as well as the finished key result if a fold in the
    -- container finished.
    -}
    , demuxGeneric
    , demux
    , demuxGenericIO
    , demuxIO

    -- TODO: These can be implemented using the above operations
    -- , demuxSel -- Stop when the fold for the specified key stops
    -- , demuxMin -- Stop when any of the folds stop
    -- , demuxAll -- Stop when all the folds stop (run once)

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

    {-
    , kvToMap

    , toContainer
    , toContainerIO
    , toMap
    , toMapIO
    -}

    , classifyGeneric
    , classify
    , classifyGenericIO
    , classifyIO
    -- , toContainerSel
    -- , toContainerMin
    )
where

#include "inline.hs"
#include "ArrayMacros.h"

import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.IntSet (IntSet)
import Data.Set (Set)
import Streamly.Internal.Data.IsMap (IsMap(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))

import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Streamly.Internal.Data.IsMap as IsMap

import Prelude hiding (Foldable(..))
import Streamly.Internal.Data.Scanl.Type
-- import Streamly.Internal.Data.Scanl.Combinators

#include "DocTestDataScanl.hs"

-- | Scan the input adding it to a set.
--
-- Definition:
--
-- >>> toSet = Scanl.mkScanl (flip Set.insert) Set.empty
--
{-# INLINE toSet #-}
toSet :: (Monad m, Ord a) => Scanl m a (Set a)
toSet = mkScanl (flip Set.insert) Set.empty

-- | Scan the input adding it to an int set. For integer inputs this performs
-- better than 'toSet'.
--
-- Definition:
--
-- >>> toIntSet = Scanl.mkScanl (flip IntSet.insert) IntSet.empty
--
{-# INLINE toIntSet #-}
toIntSet :: Monad m => Scanl m Int IntSet
toIntSet = mkScanl (flip IntSet.insert) IntSet.empty

-- XXX Name as nubOrd? Or write a nubGeneric

-- | Returns 'Just' for the first occurrence of an element, returns 'Nothing'
-- for any other occurrences.
--
-- Example:
--
-- >>> stream = Stream.fromList [1::Int,1,2,3,4,4,5,1,5,7]
-- >>> Stream.toList $ Stream.postscanlMaybe Scanl.nub stream
-- [1,2,3,4,5,7]
--
-- /Pre-release/
{-# INLINE nub #-}
nub :: (Monad m, Ord a) => Scanl m a (Maybe a)
nub = fmap (\(Tuple' _ x) -> x) $ mkScanl step initial

    where

    initial = Tuple' Set.empty Nothing

    step (Tuple' set _) x =
        if Set.member x set
        then Tuple' set Nothing
        else Tuple' (Set.insert x set) (Just x)

-- | Like 'nub' but specialized to a stream of 'Int', for better performance.
--
-- /Pre-release/
{-# INLINE nubInt #-}
nubInt :: Monad m => Scanl m Int (Maybe Int)
nubInt = fmap (\(Tuple' _ x) -> x) $ mkScanl step initial

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
-- >>> countDistinct = fmap Set.size Scanl.toSet
-- >>> countDistinct = Scanl.postscanl Scanl.nub $ Scanl.catMaybes $ Scanl.length
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
countDistinct :: (Monad m, Ord a) => Scanl m a Int
-- countDistinct = postscan nub $ catMaybes length
countDistinct = fmap Set.size toSet
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
-- >>> countDistinctInt = fmap IntSet.size Scanl.toIntSet
-- >>> countDistinctInt = Scanl.postscanl Scanl.nubInt $ Scanl.catMaybes $ Scanl.length
--
-- /Pre-release/
{-# INLINE countDistinctInt #-}
countDistinctInt :: Monad m => Scanl m Int Int
-- countDistinctInt = postscan nubInt $ catMaybes length
countDistinctInt = fmap IntSet.size toIntSet
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
-- the Map. This would be similar to a refold based classify.
--
-- Note: There are separate functions to determine Key and Fold from the input
-- because key is to be determined on each input whereas fold is to be
-- determined only once for a key.
--
-- XXX If a scan terminates do not start it again? This can be easily done by
-- installing a drain fold after a fold is done.
--
-- XXX We can use the Scan drain step to drain the buffered map in the end.

-- | This is the most general of all demux, classify operations.
--
-- The first component of the output tuple is a key-value Map of in-progress
-- scans. The scan returns the scan result as the second component of the
-- output tuple.
--
-- See 'demux' for documentation.
{-# INLINE demuxGeneric #-}
demuxGeneric :: (Monad m, IsMap f, Traversable f) =>
       (a -> Key f)
    -> (Key f -> m (Scanl m a b))
    -> Scanl m a (m (f b), Maybe (Key f, b))
demuxGeneric getKey getFold =
    Scanl (\s a -> Partial <$> step s a) (Partial <$> initial) extract final

    where

    initial = return $ Tuple' IsMap.mapEmpty Nothing

    {-# INLINE runFold #-}
    runFold kv (Scanl step1 initial1 extract1 final1) (k, a) = do
         res <- initial1
         case res of
            Partial s -> do
                res1 <- step1 s a
                case res1 of
                        Partial ss -> do
                            b <- extract1 ss
                            let fld = Scanl step1 (return res1) extract1 final1
                            return
                                $ Tuple'
                                    (IsMap.mapInsert k fld kv) (Just (k, b))
                        Done b ->
                            return
                                $ Tuple' (IsMap.mapDelete k kv) (Just (k, b))
            Done b ->
                -- Done in "initial" is possible only for the very first time
                -- the fold is initialized, and in that case we have not yet
                -- inserted it in the Map, so we do not need to delete it.
                return $ Tuple' kv (Just (k, b))

    step (Tuple' kv _) a = do
        let k = getKey a
        case IsMap.mapLookup k kv of
            Nothing -> do
                fld <- getFold k
                runFold kv fld (k, a)
            Just f -> runFold kv f (k, a)

    extract (Tuple' kv x) = return (Prelude.mapM f kv, x)

        where

        f (Scanl _ i e _) = do
            r <- i
            case r of
                Partial s -> e s
                _ -> error "demuxGeneric: unreachable code"

    final (Tuple' kv x) = return (Prelude.mapM f kv, x)

        where

        f (Scanl _ i _ fin) = do
            r <- i
            case r of
                Partial s -> fin s
                _ -> error "demuxGeneric: unreachable code"

{-# INLINE demuxUsingMap #-}
demuxUsingMap :: (Monad m, Ord k) =>
       (a -> k)
    -> (k -> m (Scanl m a b))
    -> Scanl m a (m (Map k b), Maybe (k, b))
demuxUsingMap = demuxGeneric

-- | @demux getKey getScan@: In a key value stream, scan values corresponding
-- to each key using a key specific scan. @getScan@ is invoked to generate a
-- key specific scan when a key is encountered for the first time in the
-- stream. If a scan does not exist corresponding to the key then 'Nothing' is
-- returned otherwise the result of the scan is returned.
--
-- If a scan terminates, another instance of the scan is started upon receiving
-- an input with that key, @getScan@ is invoked again whenever the key is
-- encountered again.
--
-- This can be used to scan a stream, splitting it based on different keys.
--
-- Since the scan generator function is monadic we can add scans dynamically.
-- For example, we can maintain a Map of keys to scans in an IORef and lookup
-- the scan from that corresponding to a key. This Map can be changed
-- dynamically, scans for new keys can be added or scans for old keys can be
-- deleted or modified.
--
-- Compare with 'classify', the scan in 'classify' is a static scan.
--
-- /Pre-release/
--
{-# INLINE demux #-}
demux :: (Monad m, Ord k) =>
       (a -> k)
    -> (k -> m (Scanl m a b))
    -> Scanl m a (Maybe (k, b))
demux getKey = fmap snd . demuxUsingMap getKey

-- XXX We can use the Scan drain step to drain the buffered map in the end.

-- | This is specialized version of 'demuxGeneric' that uses mutable IO cells
-- as fold accumulators for better performance.
--
-- Keep in mind that the values in the returned Map may be changed by the
-- ongoing scan if you are using those concurrently in another thread.
--
{-# INLINE demuxGenericIO #-}
demuxGenericIO :: (MonadIO m, IsMap f, Traversable f) =>
       (a -> Key f)
    -> (Key f -> m (Scanl m a b))
    -> Scanl m a (m (f b), Maybe (Key f, b))
demuxGenericIO getKey getFold =
    Scanl (\s a -> Partial <$> step s a) (Partial <$> initial) extract final

    where

    initial = return $ Tuple' IsMap.mapEmpty Nothing

    {-# INLINE initFold #-}
    initFold kv (Scanl step1 initial1 extract1 final1) (k, a) = do
         res <- initial1
         case res of
            Partial s -> do
                res1 <- step1 s a
                case res1 of
                    Partial ss -> do
                        -- XXX Instead of using a Fold type here use a custom
                        -- type with an IORef (possibly unboxed) for the
                        -- accumulator. That will reduce the allocations.
                        let fld = Scanl step1 (return res1) extract1 final1
                        ref <- liftIO $ newIORef fld
                        b <- extract1 ss
                        return
                            $ Tuple' (IsMap.mapInsert k ref kv) (Just (k, b))
                    Done b -> return $ Tuple' kv (Just (k, b))
            Done b -> return $ Tuple' kv (Just (k, b))

    {-# INLINE runFold #-}
    runFold kv ref (Scanl step1 initial1 extract1 final1) (k, a) = do
         res <- initial1
         case res of
            Partial s -> do
                res1 <- step1 s a
                case res1 of
                        Partial ss -> do
                            let fld = Scanl step1 (return res1) extract1 final1
                            liftIO $ writeIORef ref fld
                            b <- extract1 ss
                            return $ Tuple' kv (Just (k, b))
                        Done b ->
                            let kv1 = IsMap.mapDelete k kv
                             in return $ Tuple' kv1 (Just (k, b))
            Done _ -> error "demuxGenericIO: unreachable"

    step (Tuple' kv _) a = do
        let k = getKey a
        case IsMap.mapLookup k kv of
            Nothing -> do
                f <- getFold k
                initFold kv f (k, a)
            Just ref -> do
                f <- liftIO $ readIORef ref
                runFold kv ref f (k, a)

    extract (Tuple' kv x) = return (Prelude.mapM f kv, x)

        where

        f ref = do
            Scanl _ i e _ <- liftIO $ readIORef ref
            r <- i
            case r of
                Partial s -> e s
                _ -> error "demuxGenericIO: unreachable code"

    final (Tuple' kv x) = return (Prelude.mapM f kv, x)

        where

        f ref = do
            Scanl _ i _ fin <- liftIO $ readIORef ref
            r <- i
            case r of
                Partial s -> fin s
                _ -> error "demuxGenericIO: unreachable code"

{-# INLINE demuxUsingMapIO #-}
demuxUsingMapIO :: (MonadIO m, Ord k) =>
       (a -> k)
    -> (k -> m (Scanl m a b))
    -> Scanl m a (m (Map k b), Maybe (k, b))
demuxUsingMapIO = demuxGenericIO

-- | This is specialized version of 'demux' that uses mutable IO cells as scan
-- accumulators for better performance.
--
{-# INLINE demuxIO #-}
demuxIO :: (MonadIO m, Ord k) =>
       (a -> k)
    -> (k -> m (Scanl m a b))
    -> Scanl m a (Maybe (k, b))
demuxIO getKey = fmap snd . demuxUsingMapIO getKey

{-
-- | Fold a key value stream to a key-value Map. If the same key appears
-- multiple times, only the last value is retained.
{-# INLINE kvToMapOverwriteGeneric #-}
kvToMapOverwriteGeneric :: (Monad m, IsMap f) => Scanl m (Key f, a) (f a)
kvToMapOverwriteGeneric =
    mkScanl (\kv (k, v) -> IsMap.mapInsert k v kv) IsMap.mapEmpty

{-# INLINE demuxToContainer #-}
demuxToContainer :: (Monad m, IsMap f, Traversable f) =>
    (a -> Key f) -> (Key f -> m (Scanl m a b)) -> Scanl m a (f b)
demuxToContainer getKey getFold =
    let
        classifier = demuxGeneric getKey getFold
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst latest)
                (lmap snd $ catMaybes kvToMapOverwriteGeneric)
    in postscan classifier aggregator

-- | This collects all the results of 'demux' in a Map.
--
{-# INLINE demuxToMap #-}
demuxToMap :: (Monad m, Ord k) =>
    (a -> k) -> (k -> m (Scanl m a b)) -> Scanl m a (Map k b)
demuxToMap = demuxToContainer

{-# INLINE demuxToContainerIO #-}
demuxToContainerIO :: (MonadIO m, IsMap f, Traversable f) =>
    (a -> Key f) -> (Key f -> m (Scanl m a b)) -> Scanl m a (f b)
demuxToContainerIO getKey getFold =
    let
        classifier = demuxGenericIO getKey getFold
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst latest)
                (lmap snd $ catMaybes kvToMapOverwriteGeneric)
    in postscan classifier aggregator

-- | Same as 'demuxToMap' but uses 'demuxIO' for better performance.
--
{-# INLINE demuxToMapIO #-}
demuxToMapIO :: (MonadIO m, Ord k) =>
    (a -> k) -> (k -> m (Scanl m a b)) -> Scanl m a (Map k b)
demuxToMapIO = demuxToContainerIO

{-# INLINE demuxKvToContainer #-}
demuxKvToContainer :: (Monad m, IsMap f, Traversable f) =>
    (Key f -> m (Scanl m a b)) -> Scanl m (Key f, a) (f b)
demuxKvToContainer f = demuxToContainer fst (fmap (lmap snd) . f)

-- | Fold a stream of key value pairs using a function that maps keys to folds.
--
-- Definition:
--
-- >>> demuxKvToMap f = Fold.demuxToContainer fst (Fold.lmap snd . f)
--
-- Example:
--
-- >>> import Data.Map (Map)
-- >>> :{
--  let f "SUM" = return Fold.sum
--      f _ = return Fold.product
--      input = Stream.fromList [("SUM",1),("PRODUCT",2),("SUM",3),("PRODUCT",4)]
--   in Stream.fold (Fold.demuxKvToMap f) input :: IO (Map String Int)
-- :}
-- fromList [("PRODUCT",8),("SUM",4)]
--
-- /Pre-release/
{-# INLINE demuxKvToMap #-}
demuxKvToMap :: (Monad m, Ord k) =>
    (k -> m (Scanl m a b)) -> Scanl m (k, a) (Map k b)
demuxKvToMap = demuxKvToContainer
-}

------------------------------------------------------------------------------
-- Classify: Like demux but uses the same fold for all keys.
------------------------------------------------------------------------------

-- XXX Change these to make the behavior similar to demux* variants. We can
-- implement this using classifyScanManyWith. Maintain a set of done folds in
-- the underlying monad, and when initial is called look it up, if the fold is
-- done then initial would set a flag in the state to ignore the input or
-- return an error.

-- XXX Use a Refold m k a b so that we can make the fold key specifc.
-- XXX Is using a function (a -> k) better than using the input (k,a)?
--
-- XXX We can use the Scan drain step to drain the buffered map in the end.

{-# INLINE classifyGeneric #-}
classifyGeneric :: (Monad m, IsMap f, Traversable f, Ord (Key f)) =>
    -- Note: we need to return the Map itself to display the in-progress values
    -- e.g. to implement top. We could possibly create a separate abstraction
    -- for that use case. We return an action because we want it to be lazy so
    -- that the downstream consumers can choose to process or discard it.
    (a -> Key f) -> Scanl m a b -> Scanl m a (m (f b), Maybe (Key f, b))
classifyGeneric f (Scanl step1 initial1 extract1 final1) =
    Scanl (\s a -> Partial <$> step s a) (Partial <$> initial) extract final

    where

    -- XXX Instead of keeping a Set, after a fold terminates just install a
    -- fold that always returns Partial/Nothing.
    initial = return $ Tuple3' IsMap.mapEmpty Set.empty Nothing

    {-# INLINE initFold #-}
    initFold kv set k a = do
        x <- initial1
        case x of
              Partial s -> do
                r <- step1 s a
                case r of
                  Partial s1 -> do
                    b <- extract1 s1
                    return
                        $ Tuple3' (IsMap.mapInsert k s1 kv) set (Just (k, b))
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
            Just s -> do
                r <- step1 s a
                case r of
                  Partial s1 -> do
                    b <- extract1 s1
                    return $ Tuple3' (IsMap.mapInsert k s1 kv) set (Just (k,b))
                  Done b ->
                    let kv1 = IsMap.mapDelete k kv
                     in return $ Tuple3' kv1 (Set.insert k set) (Just (k, b))

    extract (Tuple3' kv _ x) = return (Prelude.mapM extract1 kv, x)

    final (Tuple3' kv set x) = return (IsMap.mapTraverseWithKey f1 kv, x)

        where

        f1 k s = do
            if Set.member k set
            -- XXX Why are we doing this? If it is in the set then it will not
            -- be in the map and vice-versa.
            then extract1 s
            else final1 s

{-# INLINE classifyUsingMap #-}
classifyUsingMap :: (Monad m, Ord k) =>
    (a -> k) -> Scanl m a b -> Scanl m a (m (Map k b), Maybe (k, b))
classifyUsingMap = classifyGeneric

-- XXX Make it consistent with denux.

-- | Scans the values for each key using the supplied scan.
--
-- Once the scan for a key terminates, any future values of the key are ignored.
--
-- Equivalent to the following except that the scan is not restarted:
--
-- >>> classify f fld = Scanl.demux f (const fld)
--
{-# INLINE classify #-}
classify :: (MonadIO m, Ord k) =>
    (a -> k) -> Scanl m a b -> Scanl m a (Maybe (k, b))
classify getKey = fmap snd . classifyUsingMap getKey

-- XXX we can use a Prim IORef if we can constrain the state "s" to be Prim
--
-- The code is almost the same as classifyGeneric except the IORef operations.
--
-- XXX We can use the Scan drain step to drain the buffered map in the end.

-- | Be aware that the values in the intermediate Maps would be mutable.
--
{-# INLINE classifyGenericIO #-}
classifyGenericIO :: (MonadIO m, IsMap f, Traversable f, Ord (Key f)) =>
    (a -> Key f) -> Scanl m a b -> Scanl m a (m (f b), Maybe (Key f, b))
classifyGenericIO f (Scanl step1 initial1 extract1 final1) =
    Scanl (\s a -> Partial <$> step s a) (Partial <$> initial) extract final

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
                        b <- extract1 s1
                        return
                            $ Tuple3'
                                (IsMap.mapInsert k ref kv) set (Just (k, b))
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
                        b <- extract1 s1
                        return $ Tuple3' kv set (Just (k, b))
                      Done b ->
                        let kv1 = IsMap.mapDelete k kv
                         in return
                                $ Tuple3' kv1 (Set.insert k set) (Just (k, b))

    extract (Tuple3' kv _ x) = return (Prelude.mapM g kv, x)

        where

        g ref = liftIO (readIORef ref) >>= extract1

    final (Tuple3' kv set x) = return (IsMap.mapTraverseWithKey g kv, x)

        where

        g k ref = do
            s <- liftIO $ readIORef ref
            if Set.member k set
            then extract1 s
            else final1 s

{-# INLINE classifyUsingMapIO #-}
classifyUsingMapIO :: (MonadIO m, Ord k) =>
    (a -> k) -> Scanl m a b -> Scanl m a (m (Map k b), Maybe (k, b))
classifyUsingMapIO = classifyGenericIO

-- | Same as classify except that it uses mutable IORef cells in the
-- Map, providing better performance.
--
-- Equivalent to the following except that the scan is not restarted:
--
-- >>> classifyIO f fld = Scanl.demuxIO f (const fld)
--
{-# INLINE classifyIO #-}
classifyIO :: (MonadIO m, Ord k) =>
    (a -> k) -> Scanl m a b -> Scanl m a (Maybe (k, b))
classifyIO getKey = fmap snd . classifyUsingMapIO getKey

{-
{-# INLINE toContainer #-}
toContainer :: (Monad m, IsMap f, Traversable f, Ord (Key f)) =>
    (a -> Key f) -> Scanl m a b -> Scanl m a (f b)
toContainer f fld =
    let
        classifier = classifyGeneric f fld
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst latest)
                (lmap snd $ catMaybes kvToMapOverwriteGeneric)
    in postscan classifier aggregator

-- | Split the input stream based on a key field and fold each split using the
-- given fold. Useful for map/reduce, bucketizing the input in different bins
-- or for generating histograms.
--
-- Example:
--
-- >>> import Data.Map.Strict (Map)
-- >>> :{
--  let input = Stream.fromList [("ONE",1),("ONE",1.1),("TWO",2), ("TWO",2.2)]
--      classify = Fold.toMap fst (Fold.lmap snd Fold.toList)
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
-- See 'demuxToMap' for a more powerful version where you can use a different
-- fold for each key. A simpler version of 'toMap' retaining only the last
-- value for a key can be written as:
--
-- >>> toMap = Fold.foldl' (\kv (k, v) -> Map.insert k v kv) Map.empty
--
-- /Stops: never/
--
-- /Pre-release/
--
{-# INLINE toMap #-}
toMap :: (Monad m, Ord k) =>
    (a -> k) -> Scanl m a b -> Scanl m a (Map k b)
toMap = toContainer

{-# INLINE toContainerIO #-}
toContainerIO :: (MonadIO m, IsMap f, Traversable f, Ord (Key f)) =>
    (a -> Key f) -> Scanl m a b -> Scanl m a (f b)
toContainerIO f fld =
    let
        classifier = classifyGenericIO f fld
        getMap Nothing = pure IsMap.mapEmpty
        getMap (Just action) = action
        aggregator =
            teeWith IsMap.mapUnion
                (rmapM getMap $ lmap fst latest)
                (lmap snd $ catMaybes kvToMapOverwriteGeneric)
    in postscan classifier aggregator

-- | Same as 'toMap' but maybe faster because it uses mutable cells as
-- fold accumulators in the Map.
--
{-# INLINE toMapIO #-}
toMapIO :: (MonadIO m, Ord k) =>
    (a -> k) -> Scanl m a b -> Scanl m a (Map k b)
toMapIO = toContainerIO

-- | Given an input stream of key value pairs and a fold for values, fold all
-- the values belonging to each key.  Useful for map/reduce, bucketizing the
-- input in different bins or for generating histograms.
--
-- Definition:
--
-- >>> kvToMap = Fold.toMap fst . Fold.lmap snd
--
-- Example:
--
-- >>> :{
--  let input = Stream.fromList [("ONE",1),("ONE",1.1),("TWO",2), ("TWO",2.2)]
--   in Stream.fold (Fold.kvToMap Fold.toList) input
-- :}
-- fromList [("ONE",[1.0,1.1]),("TWO",[2.0,2.2])]
--
-- /Pre-release/
{-# INLINE kvToMap #-}
kvToMap :: (Monad m, Ord k) => Scanl m a b -> Scanl m (k, a) (Map k b)
kvToMap = toMap fst . lmap snd

-- | Determine the frequency of each element in the stream.
--
-- You can just collect the keys of the resulting map to get the unique
-- elements in the stream.
--
-- Definition:
--
-- >>> frequency = Fold.toMap id Fold.length
--
{-# INLINE frequency #-}
frequency :: (Monad m, Ord a) => Scanl m a (Map a Int)
frequency = toMap id length
-}
