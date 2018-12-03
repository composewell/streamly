{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Streams.StreamK
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
-- Continuation passing style (CPS) stream implementation. The symbol 'K' below
-- denotes a function as well as a Kontinuation.
--
-- @
-- import qualified Streamly.Streams.StreamK as K
-- @
--
module Streamly.Streams.StreamK
    (
    -- * A class for streams
      IsStream (..)
    , adapt

    -- * The stream type
    , Stream (..)
    , unStreamIsolated
    , isolateStream
    , unstreamShared
    , runStreamSVar

    -- * Construction
    , mkStream
    , nil
    , cons
    , (.:)

    -- * Asynchronous construction
    , nilK
    , yieldK
    , consK

    -- * Deconstruction
    , uncons

    -- * Generation
    -- ** Unfolds
    , unfoldr
    , unfoldrM

    -- ** Specialized Generation
    , repeat
    , replicate
    , replicateM

    -- ** Conversions
    , yield
    , yieldM
    , fromFoldable
    , fromList
    , fromStreamK

    -- * Elimination
    -- ** General Folds
    , foldStream
    , foldr
    , foldrM
    , foldr1
    , foldl'
    , foldlM'
    , foldx
    , foldxM

    -- ** Specialized Folds
    , runStream
    , null
    , head
    , tail
    , init
    , elem
    , notElem
    , all
    , any
    , last
    , minimum
    , minimumBy
    , maximum
    , maximumBy
    , findIndices
    , lookup
    , findM
    , find
    , (!!)

    -- ** Map and Fold
    , mapM_

    -- ** Conversions
    , toList
    , toStreamK

    -- * Transformation
    -- ** By folding (scans)
    , scanl'
    , scanx

    -- ** Filtering
    , filter
    , take
    , takeWhile
    , drop
    , dropWhile

    -- ** Mapping
    , map
    , mapM
    , sequence

    -- ** Inserting
    , intersperseM
    , insertBy

    -- ** Deleting
    , deleteBy

    -- ** Map and Filter
    , mapMaybe

    -- ** Zipping
    , zipWith
    , zipWithM

    -- ** Merging
    , mergeBy
    , mergeByM

    -- ** Transformation comprehensions
    , the

    -- * Semigroup Style Composition
    , serial

    -- * Utilities
    , consMSerial
    , bindWith
    , withLocal

    -- * Deprecated
    , Streaming -- deprecated
    , once      -- deprecated
    )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader.Class  (MonadReader(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Semigroup (Semigroup(..))
import Prelude
       hiding (foldl, foldr, last, map, mapM, mapM_, repeat, sequence,
               take, filter, all, any, takeWhile, drop, dropWhile, minimum,
               maximum, elem, notElem, null, head, tail, init, zipWith, lookup,
               foldr1, (!!), replicate)
import qualified Prelude

import Streamly.SVar

------------------------------------------------------------------------------
-- The basic stream type
------------------------------------------------------------------------------

-- | The type @Stream m a@ represents a monadic stream of values of type 'a'
-- constructed using actions in monad 'm'. It uses stop, singleton and yield
-- continuations equivalent to the following direct style type:
--
-- @
-- data Stream m a = Stop | Singleton a | Yield a (Stream m a)
-- @
--
-- To facilitate parallel composition we maintain a local state in an 'SVar'
-- that is shared across and is used for synchronization of the streams being
-- composed.
--
-- The singleton case can be expressed in terms of stop and yield but we have
-- it as a separate case to optimize composition operations for streams with
-- single element.  We build singleton streams in the implementation of 'pure'
-- for Applicative and Monad, and in 'lift' for MonadTrans.
--
newtype Stream m a =
    Stream {
        unStream :: forall r.
               State Stream m a          -- state
            -> m r                       -- stop
            -> (a -> m r)                -- singleton
            -> (a -> Stream m a -> m r)  -- yield
            -> m r
    }

-- XXX make this the default "unStream"
-- | unwraps the Stream type producing the stream function that can be run with
-- continuations.
{-# INLINE unStreamIsolated #-}
unStreamIsolated ::
       Stream m a
    -> State Stream m a          -- state
    -> m r                       -- stop
    -> (a -> m r)                -- singleton
    -> (a -> Stream m a -> m r)  -- yield
    -> m r
unStreamIsolated x st = unStream x (rstState st)

{-# INLINE isolateStream #-}
isolateStream :: Stream m a -> Stream m a
isolateStream x = Stream $ \st stp sng yld ->
    unStreamIsolated x st stp sng yld

-- | Like unstream, but passes a shared SVar across continuations.
{-# INLINE unstreamShared #-}
unstreamShared ::
       Stream m a
    -> State Stream m a          -- state
    -> m r                       -- stop
    -> (a -> m r)                -- singleton
    -> (a -> Stream m a -> m r)  -- yield
    -> m r
unstreamShared = unStream

-- Run the stream using a run function associated with the SVar that runs the
-- streams with a captured snapshot of the monadic state.
{-# INLINE runStreamSVar #-}
runStreamSVar
    :: MonadIO m
    => SVar Stream m a
    -> Stream m a
    -> State Stream m a          -- state
    -> m r                       -- stop
    -> (a -> m r)                -- singleton
    -> (a -> Stream m a -> m r)  -- yield
    -> m ()
runStreamSVar sv m st stp sng yld =
    let mrun = runInIO $ svarMrun sv
    in void $ liftIO $ mrun $ unStream m st stp sng yld

------------------------------------------------------------------------------
-- Types that can behave as a Stream
------------------------------------------------------------------------------

infixr 5 `consM`
infixr 5 |:

-- | Class of types that can represent a stream of elements of some type 'a' in
-- some monad 'm'.
--
-- @since 0.2.0
class IsStream t where
    toStream :: t m a -> Stream m a
    fromStream :: Stream m a -> t m a
    -- | Constructs a stream by adding a monadic action at the head of an
    -- existing stream. For example:
    --
    -- @
    -- > toList $ getLine \`consM` getLine \`consM` nil
    -- hello
    -- world
    -- ["hello","world"]
    -- @
    --
    -- /Concurrent (do not use 'parallely' to construct infinite streams)/
    --
    -- @since 0.2.0
    consM :: MonadAsync m => m a -> t m a -> t m a
    -- | Operator equivalent of 'consM'. We can read it as "@parallel colon@"
    -- to remember that @|@ comes before ':'.
    --
    -- @
    -- > toList $ getLine |: getLine |: nil
    -- hello
    -- world
    -- ["hello","world"]
    -- @
    --
    -- @
    -- let delay = threadDelay 1000000 >> print 1
    -- runStream $ serially  $ delay |: delay |: delay |: nil
    -- runStream $ parallely $ delay |: delay |: delay |: nil
    -- @
    --
    -- /Concurrent (do not use 'parallely' to construct infinite streams)/
    --
    -- @since 0.2.0
    (|:) :: MonadAsync m => m a -> t m a -> t m a
    -- We can define (|:) just as 'consM' but it is defined explicitly for each
    -- type because we want to use SPECIALIZE pragma on the definition.

-- | Same as 'IsStream'.
--
-- @since 0.1.0
{-# DEPRECATED Streaming "Please use IsStream instead." #-}
type Streaming = IsStream

-------------------------------------------------------------------------------
-- Type adapting combinators
-------------------------------------------------------------------------------

-- | Adapt any specific stream type to any other specific stream type.
--
-- @since 0.1.0
adapt :: (IsStream t1, IsStream t2) => t1 m a -> t2 m a
adapt = fromStream . toStream

------------------------------------------------------------------------------
-- Building a stream
------------------------------------------------------------------------------

-- | Build a stream from an 'SVar', a stop continuation, a singleton stream
-- continuation and a yield continuation.
mkStream:: IsStream t
    => (forall r. State Stream m a
        -> m r
        -> (a -> m r)
        -> (a -> t m a -> m r)
        -> m r)
    -> t m a
mkStream k = fromStream $ Stream $ \st stp sng yld ->
    let yieldk a r = yld a (toStream r)
     in k (rstState st) stp sng yieldk

------------------------------------------------------------------------------
-- Construction
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
nil = fromStream $ Stream $ \_ stp _ _ -> stp

infixr 5 `cons`

-- faster than consM because there is no bind.
-- | Construct a stream by adding a pure value at the head of an existing
-- stream. For serial streams this is the same as @(return a) \`consM` r@ but
-- more efficient. For concurrent streams this is not concurrent whereas
-- 'consM' is concurrent. For example:
--
-- @
-- > toList $ 1 \`cons` 2 \`cons` 3 \`cons` nil
-- [1,2,3]
-- @
--
-- @since 0.1.0
cons :: IsStream t => a -> t m a -> t m a
cons a r = fromStream $ Stream $ \_ _ _ yld -> yld a (toStream r)

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

{-# INLINE consMSerial #-}
consMSerial :: (Monad m) => m a -> Stream m a -> Stream m a
consMSerial m r = Stream $ \_ _ _ yld -> m >>= \a -> yld a r

------------------------------------------------------------------------------
-- Asynchronous construction
------------------------------------------------------------------------------

-- | Make an empty stream from a callback function.
nilK :: IsStream t => (forall r. m r -> m r) -> t m a
nilK k = fromStream $ Stream $ \_ stp _ _ -> k stp

-- | Make a singleton stream from a one shot callback function.
yieldK :: IsStream t => (forall r. (a -> m r) -> m r) -> t m a
yieldK k = fromStream $ Stream $ \_ _ sng _ -> k sng

-- | Construct a stream from a callback function.
consK :: IsStream t => (forall r. (a -> m r) -> m r) -> t m a -> t m a
consK k r = fromStream $ Stream $ \_ _ _ yld -> k (\x -> yld x (toStream r))

-- XXX consK with concurrent callbacks
-- XXX Build a stream from a repeating callback function.

-------------------------------------------------------------------------------
-- IsStream Stream
-------------------------------------------------------------------------------

instance IsStream Stream where
    toStream = id
    fromStream = id

    {-# INLINE consM #-}
    {-# SPECIALIZE consM :: IO a -> Stream IO a -> Stream IO a #-}
    consM :: Monad m => m a -> Stream m a -> Stream m a
    consM = consMSerial

    {-# INLINE (|:) #-}
    {-# SPECIALIZE (|:) :: IO a -> Stream IO a -> Stream IO a #-}
    (|:) :: Monad m => m a -> Stream m a -> Stream m a
    (|:) = consMSerial

-------------------------------------------------------------------------------
-- Deconstruction
-------------------------------------------------------------------------------

{-# INLINE uncons #-}
uncons :: (IsStream t, Monad m) => t m a -> m (Maybe (a, t m a))
uncons m =
    let stop = return Nothing
        single a = return (Just (a, nil))
        yieldk a r = return (Just (a, fromStream r))
    in unStream (toStream m) defState stop single yieldk

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

{-# INLINE unfoldr #-}
unfoldr :: IsStream t => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step = fromStream . go
    where
    go s = Stream $ \_ stp _ yld ->
        case step s of
            Nothing -> stp
            Just (a, b) -> yld a (go b)

{-# INLINE unfoldrM #-}
unfoldrM :: (IsStream t, MonadAsync m) => (b -> m (Maybe (a, b))) -> b -> t m a
unfoldrM step = go
    where
    go s = fromStream $ Stream $ \svr stp sng yld -> do
        mayb <- step s
        case mayb of
            Nothing -> stp
            Just (a, b) ->
                unStream (toStream (return a |: go b)) svr stp sng yld

-------------------------------------------------------------------------------
-- Special generation
-------------------------------------------------------------------------------

yield :: IsStream t => a -> t m a
yield a = fromStream $ Stream $ \_ _ single _ -> single a

{-# INLINE yieldM #-}
yieldM :: (Monad m, IsStream t) => m a -> t m a
yieldM m = fromStream $ Stream $ \_ _ single _ -> m >>= single

-- | Same as yieldM
--
-- @since 0.2.0
{-# DEPRECATED once "Please use yieldM instead." #-}
{-# INLINE once #-}
once :: (Monad m, IsStream t) => m a -> t m a
once = yieldM

-- |
-- @
-- repeatM = fix . cons
-- repeatM = cycle1 . yield
-- @
--
-- Generate an infinite stream by repeating a pure value.
--
-- @since 0.4.0
repeat :: IsStream t => a -> t m a
repeat a = let x = cons a x in x

replicateM :: (IsStream t, MonadAsync m) => Int -> m a -> t m a
replicateM n m = go n
    where
    go cnt = if cnt <= 0 then nil else m |: go (cnt - 1)

replicate :: IsStream t => Int -> a -> t m a
replicate n a = go n
    where
    go cnt = if cnt <= 0 then nil else a `cons` go (cnt - 1)

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- |
-- @
-- fromFoldable = 'Prelude.foldr' 'cons' 'nil'
-- @
--
-- Construct a stream from a 'Foldable' containing pure values:
--
-- @since 0.2.0
{-# INLINE fromFoldable #-}
fromFoldable :: (IsStream t, Foldable f) => f a -> t m a
fromFoldable = Prelude.foldr cons nil

{-# INLINE fromList #-}
fromList :: IsStream t => [a] -> t m a
fromList = fromFoldable

{-# INLINE fromStreamK #-}
fromStreamK :: Stream m a -> Stream m a
fromStreamK = id

-------------------------------------------------------------------------------
-- Elimination by Folding
-------------------------------------------------------------------------------

-- | Fold a stream by providing an SVar, a stop continuation, a singleton
-- continuation and a yield continuation.
foldStream
    :: IsStream t
    => State Stream m a
    -> m r
    -> (a -> m r)
    -> (a -> t m a -> m r)
    -> t m a
    -> m r
foldStream st blank single step m =
    let yieldk a x = step a (fromStream x)
     in unStream (toStream m) st blank single yieldk

-- | Lazy right associative fold.
foldr :: (IsStream t, Monad m) => (a -> b -> b) -> b -> t m a -> m b
foldr step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            single a = return (step a acc)
            yieldk a r = go r >>= \b -> return (step a b)
        in unStream m1 defState stop single yieldk

-- | Lazy right fold with a monadic step function.
{-# INLINE foldrM #-}
foldrM :: (IsStream t, Monad m) => (a -> b -> m b) -> b -> t m a -> m b
foldrM step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            single a = step a acc
            yieldk a r = go r >>= step a
        in unStream m1 defState stop single yieldk

{-# INLINE foldr1 #-}
foldr1 :: (IsStream t, Monad m) => (a -> a -> a) -> t m a -> m (Maybe a)
foldr1 step m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> fmap Just (go h (toStream t))
    where
    go p m1 =
        let stp = return p
            single a = return $ step a p
            yieldk a r = fmap (step p) (go a r)
         in unStream m1 defState stp single yieldk

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
--
-- Note that the accumulator is always evaluated including the initial value.
{-# INLINE foldx #-}
foldx :: (IsStream t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> m b
foldx step begin done m = get $ go (toStream m) begin
    where
    {-# NOINLINE get #-}
    get m1 =
        -- XXX we are not strictly evaluating the accumulator here. Is this
        -- okay?
        let single = return . done
         in unStream m1 undefined undefined single undefined

    -- Note, this can be implemented by making a recursive call to "go",
    -- however that is more expensive because of unnecessary recursion
    -- that cannot be tail call optimized. Unfolding recursion explicitly via
    -- continuations is much more efficient.
    go m1 !acc = Stream $ \_ _ sng yld ->
        let stop = sng acc
            single a = sng $ step acc a
            yieldk a r =
                let stream = go r (step acc a)
                in unStream stream defState undefined sng yld
        in unStream m1 defState stop single yieldk

-- | Strict left associative fold.
{-# INLINE foldl' #-}
foldl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> m b
foldl' step begin = foldx step begin id

-- XXX replace the recursive "go" with explicit continuations.
-- | Like 'foldx', but with a monadic step function.
foldxM :: (IsStream t, Monad m)
    => (x -> a -> m x) -> m x -> (x -> m b) -> t m a -> m b
foldxM step begin done m = go begin (toStream m)
    where
    go !acc m1 =
        let stop = acc >>= done
            single a = acc >>= \b -> step b a >>= done
            yieldk a r = acc >>= \b -> step b a >>= \x -> go (return x) r
         in unStream m1 defState stop single yieldk

-- | Like 'foldl'' but with a monadic step function.
foldlM' :: (IsStream t, Monad m) => (b -> a -> m b) -> b -> t m a -> m b
foldlM' step begin = foldxM step (return begin) return

------------------------------------------------------------------------------
-- Specialized folds
------------------------------------------------------------------------------

{-# INLINE runStream #-}
runStream :: (Monad m, IsStream t) => t m a -> m ()
runStream m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            single _ = return ()
            yieldk _ r = go (toStream r)
         in unStream m1 defState stop single yieldk

{-# INLINE null #-}
null :: (IsStream t, Monad m) => t m a -> m Bool
null m =
    let stop      = return True
        single _  = return False
        yieldk _ _ = return False
    in unStream (toStream m) defState stop single yieldk

{-# INLINE head #-}
head :: (IsStream t, Monad m) => t m a -> m (Maybe a)
head m =
    let stop      = return Nothing
        single a  = return (Just a)
        yieldk a _ = return (Just a)
    in unStream (toStream m) defState stop single yieldk

{-# INLINE tail #-}
tail :: (IsStream t, Monad m) => t m a -> m (Maybe (t m a))
tail m =
    let stop      = return Nothing
        single _  = return $ Just nil
        yieldk _ r = return $ Just $ fromStream r
    in unStream (toStream m) defState stop single yieldk

{-# INLINE init #-}
init :: (IsStream t, Monad m) => t m a -> m (Maybe (t m a))
init m = go1 (toStream m)
    where
    go1 m1 = do
        r <- uncons m1
        case r of
            Nothing -> return Nothing
            Just (h, t) -> return . Just . fromStream $ go h t
    go p m1 = Stream $ \_ stp sng yld ->
        let single _ = sng p
            yieldk a x = yld p $ go a x
         in unStream m1 defState stp single yieldk

{-# INLINE elem #-}
elem :: (IsStream t, Monad m, Eq a) => a -> t m a -> m Bool
elem e m = go (toStream m)
    where
    go m1 =
        let stop      = return False
            single a  = return (a == e)
            yieldk a r = if a == e then return True else go r
        in unStream m1 defState stop single yieldk

{-# INLINE notElem #-}
notElem :: (IsStream t, Monad m, Eq a) => a -> t m a -> m Bool
notElem e m = go (toStream m)
    where
    go m1 =
        let stop      = return True
            single a  = return (a /= e)
            yieldk a r = if a == e then return False else go r
        in unStream m1 defState stop single yieldk

all :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m Bool
all p m = go (toStream m)
    where
    go m1 =
        let single a   | p a       = return True
                       | otherwise = return False
            yieldk a r | p a       = go r
                       | otherwise = return False
         in unStream m1 defState (return True) single yieldk

any :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m Bool
any p m = go (toStream m)
    where
    go m1 =
        let single a   | p a       = return True
                       | otherwise = return False
            yieldk a r | p a       = return True
                       | otherwise = go r
         in unStream m1 defState (return False) single yieldk

-- | Extract the last element of the stream, if any.
{-# INLINE last #-}
last :: (IsStream t, Monad m) => t m a -> m (Maybe a)
last = foldx (\_ y -> Just y) Nothing id

{-# INLINE minimum #-}
minimum :: (IsStream t, Monad m, Ord a) => t m a -> m (Maybe a)
minimum m = go Nothing (toStream m)
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in unStream m1 defState stop single yieldk

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  =
                if res <= a
                then return (Just res)
                else return (Just a)
            yieldk a r =
                if res <= a
                then go (Just res) r
                else go (Just a) r
        in unStream m1 defState stop single yieldk

{-# INLINE minimumBy #-}
minimumBy
    :: (IsStream t, Monad m)
    => (a -> a -> Ordering) -> t m a -> m (Maybe a)
minimumBy cmp m = go Nothing (toStream m)
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in unStream m1 defState stop single yieldk

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  = case cmp res a of
                GT -> return (Just a)
                _  -> return (Just res)
            yieldk a r = case cmp res a of
                GT -> go (Just a) r
                _  -> go (Just res) r
        in unStream m1 defState stop single yieldk

{-# INLINE maximum #-}
maximum :: (IsStream t, Monad m, Ord a) => t m a -> m (Maybe a)
maximum m = go Nothing (toStream m)
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in unStream m1 defState stop single yieldk

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  =
                if res <= a
                then return (Just a)
                else return (Just res)
            yieldk a r =
                if res <= a
                then go (Just a) r
                else go (Just res) r
        in unStream m1 defState stop single yieldk

{-# INLINE maximumBy #-}
maximumBy :: (IsStream t, Monad m) => (a -> a -> Ordering) -> t m a -> m (Maybe a)
maximumBy cmp m = go Nothing (toStream m)
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r = go (Just a) r
        in unStream m1 defState stop single yieldk

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  = case cmp res a of
                GT -> return (Just res)
                _  -> return (Just a)
            yieldk a r = case cmp res a of
                GT -> go (Just res) r
                _  -> go (Just a) r
        in unStream m1 defState stop single yieldk

{-# INLINE (!!) #-}
(!!) :: (IsStream t, Monad m) => t m a -> Int -> m (Maybe a)
m !! i = go i (toStream m)
    where
    go n m1 =
      let single a | n == 0 = return $ Just a
                   | otherwise = return Nothing
          yieldk a x | n < 0 = return Nothing
                     | n == 0 = return $ Just a
                     | otherwise = go (n - 1) x
      in unStream m1 defState (return Nothing) single yieldk

{-# INLINE lookup #-}
lookup :: (IsStream t, Monad m, Eq a) => a -> t m (a, b) -> m (Maybe b)
lookup e m = go (toStream m)
    where
    go m1 =
        let single (a, b) | a == e = return $ Just b
                          | otherwise = return Nothing
            yieldk (a, b) x | a == e = return $ Just b
                            | otherwise = go x
        in unStream m1 defState (return Nothing) single yieldk

{-# INLINE findM #-}
findM :: (IsStream t, Monad m) => (a -> m Bool) -> t m a -> m (Maybe a)
findM p m = go (toStream m)
    where
    go m1 =
        let single a = do
                b <- p a
                if b then return $ Just a else return Nothing
            yieldk a x = do
                b <- p a
                if b then return $ Just a else go x
        in unStream m1 defState (return Nothing) single yieldk

{-# INLINE find #-}
find :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m (Maybe a)
find p = findM (return . p)

{-# INLINE findIndices #-}
findIndices :: IsStream t => (a -> Bool) -> t m a -> t m Int
findIndices p = fromStream . go 0 . toStream
    where
    go offset m1 = Stream $ \st stp sng yld ->
        let single a | p a = sng offset
                     | otherwise = stp
            yieldk a x | p a = yld offset $ go (offset + 1) x
                       | otherwise = unStream (go (offset + 1) x) st stp sng yld
        in unStream m1 (rstState st) stp single yieldk

------------------------------------------------------------------------------
-- Map and Fold
------------------------------------------------------------------------------

-- | Apply a monadic action to each element of the stream and discard the
-- output of the action.
mapM_ :: (IsStream t, Monad m) => (a -> m b) -> t m a -> m ()
mapM_ f m = go (toStream m)
    where
    go m1 =
        let stop = return ()
            single a = void (f a)
            yieldk a r = f a >> go r
         in unStream m1 defState stop single yieldk

------------------------------------------------------------------------------
-- Converting folds
------------------------------------------------------------------------------

{-# INLINABLE toList #-}
toList :: (IsStream t, Monad m) => t m a -> m [a]
toList = foldr (:) []

{-# INLINE toStreamK #-}
toStreamK :: Stream m a -> Stream m a
toStreamK = id

-------------------------------------------------------------------------------
-- Transformation by folding (Scans)
-------------------------------------------------------------------------------

{-# INLINE scanx #-}
scanx :: IsStream t => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx step begin done m =
    cons (done begin) $ fromStream $ go (toStream m) begin
    where
    go m1 !acc = Stream $ \st stp sng yld ->
        let single a = sng (done $ step acc a)
            yieldk a r =
                let s = step acc a
                in yld (done s) (go r s)
        in unStream m1 (rstState st) stp single yieldk

{-# INLINE scanl' #-}
scanl' :: IsStream t => (b -> a -> b) -> b -> t m a -> t m b
scanl' step begin = scanx step begin id

-------------------------------------------------------------------------------
-- Filtering
-------------------------------------------------------------------------------

{-# INLINE filter #-}
filter :: IsStream t => (a -> Bool) -> t m a -> t m a
filter p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \st stp sng yld ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                       | otherwise = unStream r (rstState st) stp single yieldk
         in unStream m1 (rstState st) stp single yieldk

{-# INLINE take #-}
take :: IsStream t => Int -> t m a -> t m a
take n m = fromStream $ go n (toStream m)
    where
    go n1 m1 = Stream $ \st stp sng yld ->
        let yieldk a r = yld a (go (n1 - 1) r)
        in if n1 <= 0
           then stp
           else unStream m1 (rstState st) stp sng yieldk

{-# INLINE takeWhile #-}
takeWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \st stp sng yld ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r | p a       = yld a (go r)
                       | otherwise = stp
         in unStream m1 (rstState st) stp single yieldk

drop :: IsStream t => Int -> t m a -> t m a
drop n m = fromStream $ Stream $ \st stp sng yld ->
    unStream (go n (toStream m)) (rstState st) stp sng yld
    where
    go n1 m1 = Stream $ \st stp sng yld ->
        let single _ = stp
            yieldk _ r = (unStream $ go (n1 - 1) r) st stp sng yld
        -- Somehow "<=" check performs better than a ">"
        in if n1 <= 0
           then unStream m1 st stp sng yld
           else unStream m1 st stp single yieldk

{-# INLINE dropWhile #-}
dropWhile :: IsStream t => (a -> Bool) -> t m a -> t m a
dropWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \st stp sng yld ->
        let single a   | p a       = stp
                       | otherwise = sng a
            yieldk a r | p a = unStream r (rstState st) stp single yieldk
                       | otherwise = yld a r
         in unStream m1 (rstState st) stp single yieldk

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE map #-}
map :: (IsStream t, Monad m) => (a -> b) -> t m a -> t m b
map f m = fromStream $ Stream $ \st stp sng yld ->
    let single     = sng . f
        yieldk a r = yld (f a) (fmap f r)
    in unStream (toStream m) (rstState st) stp single yieldk

-- Be careful when modifying this, this uses a consM (|:) deliberately to allow
-- other stream types to overload it.
{-# INLINE mapM #-}
mapM :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m b
mapM f m = go (toStream m)
    where
    go m1 = fromStream $ Stream $ \st stp sng yld ->
        let single a  = f a >>= sng
            yieldk a r = unStream (toStream (f a |: go r)) st stp sng yld
         in unStream m1 (rstState st) stp single yieldk

-- Be careful when modifying this, this uses a consM (|:) deliberately to allow
-- other stream types to overload it.
{-# INLINE sequence #-}
sequence :: (IsStream t, MonadAsync m) => t m (m a) -> t m a
sequence m = go (toStream m)
    where
    go m1 = fromStream $ Stream $ \st stp sng yld ->
        let single ma = ma >>= sng
            yieldk ma r = unStream (toStream $ ma |: go r) st stp sng yld
         in unStream m1 (rstState st) stp single yieldk

-------------------------------------------------------------------------------
-- Inserting
-------------------------------------------------------------------------------

{-# INLINE intersperseM #-}
intersperseM :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseM a m = fromStream $ prependingStart (toStream m)
    where
    prependingStart m1 = Stream $ \st stp sng yld ->
        let yieldk i x = unStream (return i |: go x) st stp sng yld
         in unStream m1 (rstState st) stp sng yieldk
    go m2 = fromStream $ Stream $ \st stp sng yld ->
        let single i = unStream (a |: yield i) st stp sng yld
            yieldk i x = unStream (a |: return i |: go x) st stp sng yld
         in unStream m2 (rstState st) stp single yieldk

{-# INLINE insertBy #-}
insertBy :: IsStream t => (a -> a -> Ordering) -> a -> t m a -> t m a
insertBy cmp x m = fromStream $ go (toStream m)
  where
    go m1 = Stream $ \st _ _ yld ->
        let single a = case cmp x a of
                GT -> yld a (yield x)
                _  -> yld x (yield a)
            stop = yld x nil
            yieldk a r = case cmp x a of
                GT -> yld a (go r)
                _  -> yld x (a `cons` r)
         in unStream m1 (rstState st) stop single yieldk

------------------------------------------------------------------------------
-- Deleting
------------------------------------------------------------------------------

{-# INLINE deleteBy #-}
deleteBy :: IsStream t => (a -> a -> Bool) -> a -> t m a -> t m a
deleteBy eq x m = fromStream $ go (toStream m)
  where
    go m1 = Stream $ \st stp sng yld ->
        let single a = if eq x a then stp else sng a
            yieldk a r = if eq x a
              then unStream r (rstState st) stp sng yld
              else yld a (go r)
         in unStream m1 (rstState st) stp single yieldk

-------------------------------------------------------------------------------
-- Map and Filter
-------------------------------------------------------------------------------

{-# INLINE mapMaybe #-}
mapMaybe :: IsStream t => (a -> Maybe b) -> t m a -> t m b
mapMaybe f m = go (toStream m)
  where
    go m1 = fromStream $ Stream $ \st stp sng yld ->
        let single a = case f a of
                Just b  -> sng b
                Nothing -> stp
            yieldk a r = case f a of
                Just b  -> yld b (toStream $ go r)
                Nothing -> unStream r (rstState st) stp single yieldk
        in unStream m1 (rstState st) stp single yieldk

------------------------------------------------------------------------------
-- Serial Zipping
------------------------------------------------------------------------------

{-# INLINE zipWithS #-}
zipWithS :: (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWithS f = go
    where
    go mx my = Stream $ \st stp sng yld -> do
        let merge a ra =
                let single2 b = sng (f a b)
                    yield2 b rb = yld (f a b) (go ra rb)
                 in unStream my (rstState st) stp single2 yield2
        let single1 a = merge a nil
            yield1 = merge
        unStream mx (rstState st) stp single1 yield1

-- | Zip two streams serially using a pure zipping function.
--
-- @since 0.1.0
{-# INLINABLE zipWith #-}
zipWith :: IsStream t => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f m1 m2 = fromStream $ zipWithS f (toStream m1) (toStream m2)

-- | Zip two streams serially using a monadic zipping function.
--
-- @since 0.1.0
zipWithM :: (IsStream t, Monad m) => (a -> b -> m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \st stp sng yld -> do
        let merge a ra =
                let runIt x = unStream x (rstState st) stp sng yld
                    single2 b   = f a b >>= sng
                    yield2 b rb = f a b >>= \x -> runIt (x `cons` go ra rb)
                 in unStream my (rstState st) stp single2 yield2
        let single1 a = merge a nil
            yield1 = merge
        unStream mx (rstState st) stp single1 yield1

------------------------------------------------------------------------------
-- Merging
------------------------------------------------------------------------------

{-# INLINE mergeByS #-}
mergeByS
    :: Monad m
    => (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByS cmp = go
    where
    go mx my = Stream $ \st stp sng yld -> do
        let mergeWithY a ra =
                let stop2 = unStream mx (rstState st) stp sng yld
                    single2 b = do
                        r <- cmp a b
                        case r of
                            GT -> yld b (go (a `cons` ra) nil)
                            _  -> yld a (go ra (b `cons` nil))
                    yield2 b rb = do
                        r <- cmp a b
                        case r of
                            GT -> yld b (go (a `cons` ra) rb)
                            _  -> yld a (go ra (b `cons` rb))
                 in unStream my (rstState st) stop2 single2 yield2
        let stopX = unStream my (rstState st) stp sng yld
            singleX a = mergeWithY a nil
            yieldX = mergeWithY
        unStream mx (rstState st) stopX singleX yieldX

{-# INLINABLE mergeByM #-}
mergeByM
    :: (IsStream t, Monad m)
    => (a -> a -> m Ordering) -> t m a -> t m a -> t m a
mergeByM f m1 m2 = fromStream $ mergeByS f (toStream m1) (toStream m2)

{-# INLINABLE mergeBy #-}
mergeBy
    :: (IsStream t, Monad m)
    => (a -> a -> Ordering) -> t m a -> t m a -> t m a
mergeBy cmp = mergeByM (\a b -> return $ cmp a b)

------------------------------------------------------------------------------
-- Transformation comprehensions
------------------------------------------------------------------------------

{-# INLINE the #-}
the :: (Eq a, IsStream t, Monad m) => t m a -> m (Maybe a)
the m = do
    r <- uncons m
    case r of
        Nothing -> return Nothing
        Just (h, t) -> go h (toStream t)
    where
    go h m1 =
        let single a   | h == a    = return $ Just h
                       | otherwise = return Nothing
            yieldk a r | h == a    = go h r
                       | otherwise = return Nothing
         in unStream m1 defState (return $ Just h) single yieldk

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Concatenates two streams sequentially i.e. the first stream is
-- exhausted completely before yielding any element from the second stream.
{-# INLINE serial #-}
serial :: Stream m a -> Stream m a -> Stream m a
serial m1 m2 = go m1
    where
    go (Stream m) = Stream $ \st stp sng yld ->
            let stop       = unStream m2 (rstState st) stp sng yld
                single a   = yld a m2
                yieldk a r = yld a (go r)
            in m (rstState st) stop single yieldk

instance Semigroup (Stream m a) where
    (<>) = serial

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (Stream m a) where
    mempty = nil
    mappend = (<>)

-------------------------------------------------------------------------------
-- Functor
-------------------------------------------------------------------------------

instance Monad m => Functor (Stream m) where
    fmap = map

-------------------------------------------------------------------------------
-- Bind utility
-------------------------------------------------------------------------------

{-# INLINE bindWith #-}
bindWith
    :: (forall c. Stream m c -> Stream m c -> Stream m c)
    -> Stream m a
    -> (a -> Stream m b)
    -> Stream m b
bindWith par m f = go m
    where
        go (Stream g) =
            Stream $ \st stp sng yld ->
                let runShared x = unstreamShared x st stp sng yld
                    runIsolated x = unStreamIsolated x st stp sng yld

                    single a   = runIsolated $ f a
                    yieldk a r = runShared $ isolateStream (f a) `par` go r
                in g (rstState st) stp single yieldk

------------------------------------------------------------------------------
-- Alternative & MonadPlus
------------------------------------------------------------------------------

_alt :: Stream m a -> Stream m a -> Stream m a
_alt m1 m2 = Stream $ \st stp sng yld ->
    let stop  = unStream m2 (rstState st) stp sng yld
    in unStream m1 (rstState st) stop sng yld

------------------------------------------------------------------------------
-- MonadReader
------------------------------------------------------------------------------

withLocal :: MonadReader r m => (r -> r) -> Stream m a -> Stream m a
withLocal f m =
    Stream $ \st stp sng yld ->
        let single = local f . sng
            yieldk a r = local f $ yld a (withLocal f r)
        in unStream m (rstState st) (local f stp) single yieldk

------------------------------------------------------------------------------
-- MonadError
------------------------------------------------------------------------------

{-
-- XXX handle and test cross thread state transfer
withCatchError
    :: MonadError e m
    => Stream m a -> (e -> Stream m a) -> Stream m a
withCatchError m h =
    Stream $ \_ stp sng yld ->
        let run x = unStream x Nothing stp sng yieldk
            handle r = r `catchError` \e -> run $ h e
            yieldk a r = yld a (withCatchError r h)
        in handle $ run m
-}

-------------------------------------------------------------------------------
-- Transformers
-------------------------------------------------------------------------------

instance MonadTrans Stream where
    lift = yieldM
