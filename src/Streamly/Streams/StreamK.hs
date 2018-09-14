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
    , consWith
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
    , maximum
    , findIndices
    , lookup
    , find

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

    -- ** Map and Filter
    , mapMaybe

    -- ** Zipping
    , zipWith
    , zipWithM

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
               foldr1)
import qualified Prelude

import Streamly.SVar

------------------------------------------------------------------------------
-- The basic stream type
------------------------------------------------------------------------------

-- If an API exits early it has to cleanup the SVar i.e. any pending threads on
-- the SVar. The API has to release only those resources used by the stream
-- that it is consuming. If we use a register/release in a global state
-- mechanism to track and cleanup resources then we need a way to identify the
-- resources that belong to all the downstream SVars. That could be done by
-- always passing an IORef when calling unstream. The downward stream can
-- register the cleanup functions in the IORef, which can be released by the
-- caller.  However, any API that may abort stream evaluation in middle has to
-- allocate an IORef and pass it down before calling unstream, because we never
-- know when an SVar may get allocated. Furthermore, this IORef has to be
-- linked from the parent IORef for downstream trackability. APIs that consume
-- the stream fully can just pass down the IORef coming from an upstream API.
--
-- The advantages of this approach are:
--
-- cons and uncons need not be aware of the cleanup function
-- only those APIs that may abort have to be aware of the handling. In the
-- other approach all APIs need to be aware of the handling and if some API
-- loses the cleanup function it will affect the handling.
-- In this approach since the upstream IORef is by default passed in the state
-- we can at most have a problem in a particular broken API and it cannot
-- affect other APIs.
--
-- An alternative approach is that the yield continuation takes a resource
-- cleanup action (m ()) as well.
--
-- XXX we can use a start continuation to yield a value and a cleanup function
-- instead of yielding the cleanup funciton on every yield. This can be
-- especially helpful in cases when the yielded values are transferred over the
-- network, we should not be sending an extra value every time.
--
-- Can the cleanup function change in between? Due to composition of different
-- streams which have different cleanup functions?
--
-- Also, do we need the cleanup function to be thread safe?
--
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
            -> (a -> Stream m a -> m () -> m r)  -- yield
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
    -> (a -> Stream m a -> m () -> m r)  -- yield
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
    -> (a -> Stream m a -> m () -> m r)  -- yield
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
    -> (a -> Stream m a -> m () -> m r)  -- yield
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
        -> (a -> t m a -> m () -> m r)
        -> m r)
    -> t m a
mkStream k = fromStream $ Stream $ \st stp sng yld ->
    let yieldk a r kl = yld a (toStream r) kl
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
cons :: (IsStream t, Monad m) => a -> t m a -> t m a
cons a r = fromStream $ Stream $ \_ _ _ yld -> yld a (toStream r) (return ())

-- | Construct with a cleanup function
consWith :: IsStream t => a -> t m a -> m () -> t m a
consWith a r k = fromStream $ Stream $ \_ _ _ yld -> yld a (toStream r) k

infixr 5 .:

-- | Operator equivalent of 'cons'.
--
-- @
-- > toList $ 1 .: 2 .: 3 .: nil
-- [1,2,3]
-- @
--
-- @since 0.1.1
(.:) :: (IsStream t, Monad m) => a -> t m a -> t m a
(.:) = cons

{-# INLINE consMSerial #-}
consMSerial :: Monad m => m a -> Stream m a -> Stream m a
consMSerial m r = Stream $ \_ _ _ yld -> m >>= \a -> yld a r (return ())

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
consK :: (IsStream t, Monad m)
    => (forall r. (a -> m r) -> m r) -> t m a -> t m a
consK k r = fromStream $ Stream $ \_ _ _ yld ->
    k (\x -> yld x (toStream r) (return ()))

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

-- XXX uncons loses the cleanup function
{-# INLINE uncons #-}
uncons :: (IsStream t, Monad m) => t m a -> m (Maybe (a, t m a))
uncons m =
    let stop = return Nothing
        single a = return (Just (a, nil))
        yieldk a r _ = return (Just (a, fromStream r))
    in unStream (toStream m) defState stop single yieldk

-------------------------------------------------------------------------------
-- Generation
-------------------------------------------------------------------------------

{-# INLINE unfoldr #-}
unfoldr :: (IsStream t, Monad m) => (b -> Maybe (a, b)) -> b -> t m a
unfoldr step = fromStream . go
    where
    go s = Stream $ \_ stp _ yld ->
        case step s of
            Nothing -> stp
            Just (a, b) -> yld a (go b) (return ())

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

-- | Generate an infinite stream by repeating a pure value.
-- Can be expressed as @cycle1 . yield@.
--
-- @since 0.4.0
repeat :: (IsStream t, Monad m) => a -> t m a
repeat a = let x = cons a x in x

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

-- | Construct a stream from a 'Foldable' containing pure values. Same as
-- @'Prelude.foldr' 'cons' 'nil'@.
--
-- @since 0.2.0
{-# INLINE fromFoldable #-}
fromFoldable :: (IsStream t, Monad m, Foldable f) => f a -> t m a
fromFoldable = Prelude.foldr cons nil

{-# INLINE fromList #-}
fromList :: (IsStream t, Monad m) => [a] -> t m a
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
    let yieldk a x _ = step a (fromStream x)
     in unStream (toStream m) st blank single yieldk

-- | Lazy right associative fold.
{-# INLINE foldr #-}
foldr :: (IsStream t, Monad m) => (a -> b -> b) -> b -> t m a -> m b
foldr step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            single a = return (step a acc)
            yieldk a r _ = go r >>= \b -> return (step a b)
        in unStream m1 defState stop single yieldk

-- | Lazy right fold with a monadic step function.
{-# INLINE foldrM #-}
foldrM :: (IsStream t, Monad m) => (a -> b -> m b) -> b -> t m a -> m b
foldrM step acc m = go (toStream m)
    where
    go m1 =
        let stop = return acc
            single a = step a acc
            yieldk a r _ = go r >>= step a
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
            yieldk a r _ = fmap (step p) (go a r)
         in unStream m1 defState stp single yieldk

-- | Strict left fold with an extraction function. Like the standard strict
-- left fold, but applies a user supplied extraction function (the third
-- argument) to the folded value at the end. This is designed to work with the
-- @foldl@ library. The suffix @x@ is a mnemonic for extraction.
{-# INLINE foldx #-}
foldx :: (IsStream t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> m b
foldx step begin done m = get $ go (toStream m) begin
    where
    {-# NOINLINE get #-}
    get m1 =
        let single = return . done
         in unStream m1 undefined undefined single undefined

    -- Note, this can be implemented by making a recursive call to "go",
    -- however that is more expensive because of unnecessary recursion
    -- that cannot be tail call optimized. Unfolding recursion explicitly via
    -- continuations is much more efficient.
    go m1 !acc = Stream $ \_ _ sng yld ->
        let stop = sng acc
            single a = sng $ step acc a
            yieldk a r _ =
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
            yieldk a r _ = acc >>= \b -> step b a >>= \x -> go (return x) r
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
            yieldk _ r _ = go (toStream r)
         in unStream m1 defState stop single yieldk

{-# INLINE null #-}
null :: (IsStream t, Monad m) => t m a -> m Bool
null m =
    let stop      = return True
        single _  = return False
        yieldk _ _ k = k >> return False
    in unStream (toStream m) defState stop single yieldk

{-# INLINE head #-}
head :: (IsStream t, Monad m) => t m a -> m (Maybe a)
head m =
    let stop      = return Nothing
        single a  = return (Just a)
        yieldk a _ k = k >> return (Just a)
    in unStream (toStream m) defState stop single yieldk

{-# INLINE tail #-}
tail :: (IsStream t, Monad m) => t m a -> m (Maybe (t m a))
tail m =
    let stop      = return Nothing
        single _  = return $ Just nil
        yieldk _ r _ = return $ Just $ fromStream r
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
            yieldk a x _ = yld p (go a x) (return ())
         in unStream m1 defState stp single yieldk

{-# INLINE elem #-}
elem :: (IsStream t, Monad m, Eq a) => a -> t m a -> m Bool
elem e m = go (toStream m)
    where
    go m1 =
        let stop      = return False
            single a  = return (a == e)
            yieldk a r k = if a == e then k >> return True else go r
        in unStream m1 defState stop single yieldk

{-# INLINE notElem #-}
notElem :: (IsStream t, Monad m, Eq a) => a -> t m a -> m Bool
notElem e m = go (toStream m)
    where
    go m1 =
        let stop      = return True
            single a  = return (a /= e)
            yieldk a r k = if a == e then k >> return False else go r
        in unStream m1 defState stop single yieldk

all :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m Bool
all p m = go (toStream m)
    where
    go m1 =
        let single a     | p a       = return True
                         | otherwise = return False
            yieldk a r k | p a       = go r
                         | otherwise = k >> return False
         in unStream m1 defState (return True) single yieldk

any :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m Bool
any p m = go (toStream m)
    where
    go m1 =
        let single a     | p a       = return True
                         | otherwise = return False
            yieldk a r k | p a       = k >> return True
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
            yieldk a r _ = go (Just a) r
        in unStream m1 defState stop single yieldk

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  =
                if res <= a
                then return (Just res)
                else return (Just a)
            yieldk a r _ =
                if res <= a
                then go (Just res) r
                else go (Just a) r
        in unStream m1 defState stop single yieldk

{-# INLINE maximum #-}
maximum :: (IsStream t, Monad m, Ord a) => t m a -> m (Maybe a)
maximum m = go Nothing (toStream m)
    where
    go Nothing m1 =
        let stop      = return Nothing
            single a  = return (Just a)
            yieldk a r _ = go (Just a) r
        in unStream m1 defState stop single yieldk

    go (Just res) m1 =
        let stop      = return (Just res)
            single a  =
                if res <= a
                then return (Just a)
                else return (Just res)
            yieldk a r _ =
                if res <= a
                then go (Just a) r
                else go (Just res) r
        in unStream m1 defState stop single yieldk

{-# INLINE lookup #-}
lookup :: (IsStream t, Monad m, Eq a) => a -> t m (a, b) -> m (Maybe b)
lookup e m = go (toStream m)
    where
    go m1 =
        let single (a, b) | a == e = return $ Just b
                          | otherwise = return Nothing
            yieldk (a, b) x k | a == e = k >> return (Just b)
                              | otherwise = go x
        in unStream m1 defState (return Nothing) single yieldk

{-# INLINE find #-}
find :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> m (Maybe a)
find p m = go (toStream m)
    where
    go m1 =
        let single a | p a = return $ Just a
                     | otherwise = return Nothing
            yieldk a x k | p a = k >> return (Just a)
                         | otherwise = go x
        in unStream m1 defState (return Nothing) single yieldk

{-# INLINE findIndices #-}
findIndices :: IsStream t => (a -> Bool) -> t m a -> t m Int
findIndices p = fromStream . go 0 . toStream
    where
    go offset m1 = Stream $ \st stp sng yld ->
        let single a | p a = sng offset
                     | otherwise = stp
            yieldk a x k | p a = yld offset (go (offset + 1) x) k
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
            yieldk a r _ = f a >> go r
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
scanx :: (IsStream t, Monad m)
    => (x -> a -> x) -> x -> (x -> b) -> t m a -> t m b
scanx step begin done m =
    cons (done begin) $ fromStream $ go (toStream m) begin
    where
    go m1 !acc = Stream $ \st stp sng yld ->
        let single a = sng (done $ step acc a)
            yieldk a r k =
                let s = step acc a
                in yld (done s) (go r s) k
        in unStream m1 (rstState st) stp single yieldk

{-# INLINE scanl' #-}
scanl' :: (IsStream t, Monad m) => (b -> a -> b) -> b -> t m a -> t m b
scanl' step begin m = scanx step begin id m

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
            yieldk a r k | p a       = yld a (go r) k
                         | otherwise = unStream r (rstState st) stp single yieldk
         in unStream m1 (rstState st) stp single yieldk

{-# INLINE take #-}
take :: (IsStream t, Monad m) => Int -> t m a -> t m a
take n m = fromStream $ go n (toStream m) Nothing
    where
    go n1 m1 cleanup = Stream $ \st stp sng yld ->
        let yieldk a r k = yld a (go (n1 - 1) r (Just k)) k
        in if n1 <= 0
           then maybe (return ()) id cleanup >> stp
           else unStream m1 (rstState st) stp sng yieldk

{-# INLINE takeWhile #-}
takeWhile :: (IsStream t, Monad m) => (a -> Bool) -> t m a -> t m a
takeWhile p m = fromStream $ go (toStream m)
    where
    go m1 = Stream $ \st stp sng yld ->
        let single a   | p a       = sng a
                       | otherwise = stp
            yieldk a r k | p a       = yld a (go r) k
                         | otherwise = k >> stp
         in unStream m1 (rstState st) stp single yieldk

drop :: IsStream t => Int -> t m a -> t m a
drop n m = fromStream $ Stream $ \st stp sng yld ->
    unStream (go n (toStream m)) (rstState st) stp sng yld
    where
    go n1 m1 = Stream $ \st stp sng yld ->
        let single _ = stp
            yieldk _ r _ = (unStream $ go (n1 - 1) r) st stp sng yld
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
            yieldk a r k | p a = unStream r (rstState st) stp single yieldk
                         | otherwise = yld a r k
         in unStream m1 (rstState st) stp single yieldk

-------------------------------------------------------------------------------
-- Mapping
-------------------------------------------------------------------------------

{-# INLINE map #-}
map :: (IsStream t, Monad m) => (a -> b) -> t m a -> t m b
map f m = fromStream $ Stream $ \st stp sng yld ->
    let single     = sng . f
        yieldk a r k = yld (f a) (map f r) k
    in unStream (toStream m) (rstState st) stp single yieldk

-- Be careful when modifying this, this uses a consM (|:) deliberately to allow
-- other stream types to overload it.
{-# INLINE mapM #-}
mapM :: (IsStream t, MonadAsync m) => (a -> m b) -> t m a -> t m b
mapM f m = go (toStream m)
    where
    go m1 = fromStream $ Stream $ \st stp sng yld ->
        let single a  = f a >>= sng
            yieldk a r k =
                -- We know that |: does not return a cleanup function
                let yk a1 r1 _ = yld a1 r1 k
                in unStream (toStream (f a |: (go r))) st stp sng yk
         in unStream m1 (rstState st) stp single yieldk

-- Be careful when modifying this, this uses a consM (|:) deliberately to allow
-- other stream types to overload it.
{-# INLINE sequence #-}
sequence :: (IsStream t, MonadAsync m) => t m (m a) -> t m a
sequence m = go (toStream m)
    where
    go m1 = fromStream $ Stream $ \st stp sng yld ->
        let single ma = ma >>= sng
            yieldk ma r k =
                -- We know that |: does not return a cleanup function
                let yk a1 r1 _ = yld a1 r1 k
                in unStream (toStream $ ma |: go r) st stp sng yk
         in unStream m1 (rstState st) stp single yieldk

-------------------------------------------------------------------------------
-- Inserting
-------------------------------------------------------------------------------

{-# INLINE intersperseM #-}
intersperseM :: (IsStream t, MonadAsync m) => m a -> t m a -> t m a
intersperseM a m = fromStream $ prependingStart (toStream m)
    where
    prependingStart m1 = Stream $ \st stp sng yld ->
        let yieldk i x k =
                let yk a1 r1 _ = yld a1 r1 k
                in unStream (return i |: go x) st stp sng yk
         in unStream m1 (rstState st) stp sng yieldk

    go m2 = fromStream $ Stream $ \st stp sng yld ->
        let single i = unStream (a |: yield i) st stp sng yld
            yieldk i x k =
                let yk a1 r1 _ = yld a1 r1 k
                in unStream (a |: return i |: go x) st stp sng yk
         in unStream m2 (rstState st) stp single yieldk

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
            yieldk a r k = case f a of
                Just b  -> yld b (toStream $ go r) k
                Nothing -> unStream r (rstState st) stp single yieldk
        in unStream m1 (rstState st) stp single yieldk

------------------------------------------------------------------------------
-- Serial Zipping
------------------------------------------------------------------------------

{-# INLINE zipWithS #-}
zipWithS :: Monad m => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWithS f m1 m2 = go m1 m2
    where
    go mx my = Stream $ \st stp sng yld -> do
        let merge a ra kl =
                let single2 b = sng (f a b)
                    yield2 b rb k = yld (f a b) (go ra rb) (k >> kl)
                 in unStream my (rstState st) stp single2 yield2
        let single1 a   = merge a nil (return ())
            yield1 a ra k = merge a ra k
        unStream mx (rstState st) stp single1 yield1

-- | Zip two streams serially using a pure zipping function.
--
-- @since 0.1.0
{-# INLINABLE zipWith #-}
zipWith :: (IsStream t, Monad m) => (a -> b -> c) -> t m a -> t m b -> t m c
zipWith f m1 m2 = fromStream $ zipWithS f (toStream m1) (toStream m2)

-- | Zip two streams serially using a monadic zipping function.
--
-- @since 0.1.0
zipWithM :: (IsStream t, Monad m) => (a -> b -> m c) -> t m a -> t m b -> t m c
zipWithM f m1 m2 = fromStream $ go (toStream m1) (toStream m2)
    where
    go mx my = Stream $ \st stp sng yld -> do
        let merge a ra kl =
                let runIt x k = unStream x (rstState st) stp sng
                                    $ \a1 r _ -> yld a1 r k
                    single2 b   = f a b >>= sng
                    yield2 b rb k = f a b >>= \x -> runIt (x `cons` go ra rb)
                                                          (k >> kl)
                 in unStream my (rstState st) stp single2 yield2
        let single1 a  = merge a nil (return ())
            yield1 a ra k = merge a ra k
        unStream mx (rstState st) stp single1 yield1

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Concatenates two streams sequentially i.e. the first stream is
-- exhausted completely before yielding any element from the second stream.
{-# INLINE serial #-}
serial :: Monad m => Stream m a -> Stream m a -> Stream m a
serial m1 m2 = go m1
    where
    go (Stream m) = Stream $ \st stp sng yld ->
            let stop       = unStream m2 (rstState st) stp sng yld
                single a   = yld a m2 (return ())
                yieldk a r k = yld a (go r) k
            in m (rstState st) stop single yieldk

instance Monad m => Semigroup (Stream m a) where
    (<>) = serial

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monad m => Monoid (Stream m a) where
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
    :: Monad m
    => (forall c. Stream m c -> Stream m c -> Stream m c)
    -> Stream m a
    -> (a -> Stream m b)
    -> Stream m b
bindWith par m f = go m
    where
        go (Stream g) =
            Stream $ \st stp sng yld ->
                let runShared x k = unstreamShared x st stp sng
                        $ \a r k1 -> yld a r (k >> k1)
                    runIsolated x k = unStreamIsolated x st stp sng
                        $ \a r k1 -> yld a r (k >> k1)

                    single a   = runIsolated (f a) (return ())
                    yieldk a r k = runShared (isolateStream (f a) `par` go r) k
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
            yieldk a r k = local f $ yld a (withLocal f r) k
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
