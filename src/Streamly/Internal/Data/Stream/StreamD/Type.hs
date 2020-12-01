#include "inline.hs"

-- |
-- Module      : Streamly.Internal.Data.Stream.StreamD.Type
-- Copyright   : (c) 2018 Composewell Technologies
--               (c) Roman Leshchinskiy 2008-2010
--
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Internal.Data.Stream.StreamD.Type
    (
    -- * The stream type
      Step (..)
    -- XXX UnStream is exported to avoid a performance issue in concatMap if we
    -- use the pattern synonym "Stream".
    , Stream (Stream, UnStream)

    , fromStreamK
    , toStreamK
    , fromStreamD
    , map
    , mapM
    , yield
    , yieldM
    , concatMap
    , concatMapM

    , foldrT
    , foldrM
    , foldrMx
    , foldr
    , foldrS

    , foldl'
    , foldlM'
    , foldlx'
    , foldlMx'

    , toList
    , fromList

    , eqBy
    , cmpBy
    , take
    , GroupState (..) -- for inspection testing
    , foldMany
    , foldMany1
    , groupsOf
    , groupsOf2
    )
where

import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Data.Functor.Identity (Identity(..))
import GHC.Base (build)
import GHC.Types (SPEC(..))
import Prelude hiding (map, mapM, foldr, take, concatMap)
import Fusion.Plugin.Types (Fuse(..))

import Streamly.Internal.Data.SVar (State(..), adaptState, defState)
import Streamly.Internal.Data.Fold.Types (Fold(..), Fold2(..))

import qualified Streamly.Internal.Data.Stream.StreamK as K
import qualified Streamly.Internal.Data.Fold.Types as FL

------------------------------------------------------------------------------
-- The direct style stream type
------------------------------------------------------------------------------

-- | A stream is a succession of 'Step's. A 'Yield' produces a single value and
-- the next state of the stream. 'Stop' indicates there are no more values in
-- the stream.
{-# ANN type Step Fuse #-}
data Step s a = Yield a s | Skip s | Stop

instance Functor (Step s) where
    {-# INLINE fmap #-}
    fmap f (Yield x s) = Yield (f x) s
    fmap _ (Skip s) = Skip s
    fmap _ Stop = Stop

-- gst = global state
-- | A stream consists of a step function that generates the next step given a
-- current state, and the current state.
data Stream m a =
    forall s. UnStream (State K.Stream m a -> s -> m (Step s a)) s

unShare :: Stream m a -> Stream m a
unShare (UnStream step state) = UnStream step' state
    where step' gst = step (adaptState gst)

pattern Stream :: (State K.Stream m a -> s -> m (Step s a)) -> s -> Stream m a
pattern Stream step state <- (unShare -> UnStream step state)
    where Stream = UnStream

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Stream #-}
#endif

{-# INLINE_LATE fromStreamK #-}
fromStreamK :: Monad m => K.Stream m a -> Stream m a
fromStreamK = Stream step
    where
    step gst m1 =
        let stop       = return Stop
            single a   = return $ Yield a K.nil
            yieldk a r = return $ Yield a r
         in K.foldStreamShared gst yieldk single stop m1

-- Convert a direct stream to and from CPS encoded stream
{-# INLINE_LATE toStreamK #-}
toStreamK :: Monad m => Stream m a -> K.Stream m a
toStreamK (Stream step state) = go state
    where
    go st = K.mkStream $ \gst yld _ stp ->
      let go' ss = do
           r <- step gst ss
           case r of
               Yield x s -> yld x (go s)
               Skip  s   -> go' s
               Stop      -> stp
      in go' st

#ifndef DISABLE_FUSION
{-# RULES "fromStreamK/toStreamK fusion"
    forall s. toStreamK (fromStreamK s) = s #-}
{-# RULES "toStreamK/fromStreamK fusion"
    forall s. fromStreamK (toStreamK s) = s #-}
#endif

------------------------------------------------------------------------------
-- Converting folds
------------------------------------------------------------------------------

{-# INLINE fromStreamD #-}
fromStreamD :: (K.IsStream t, Monad m) => Stream m a -> t m a
fromStreamD = K.fromStream . toStreamK

------------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------------

-- | Map a monadic function over a 'Stream'
{-# INLINE_NORMAL mapM #-}
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
mapM f (Stream step state) = Stream step' state
  where
    {-# INLINE_LATE step' #-}
    step' gst st = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> f x >>= \a -> return $ Yield a s
            Skip s    -> return $ Skip s
            Stop      -> return Stop

{-# INLINE map #-}
map :: Monad m => (a -> b) -> Stream m a -> Stream m b
map f = mapM (return . f)

instance Functor m => Functor (Stream m) where
    {-# INLINE fmap #-}
    fmap f (Stream step state) = Stream step' state
      where
        {-# INLINE_LATE step' #-}
        step' gst st = fmap (fmap f) (step (adaptState gst) st)

    {-# INLINE (<$) #-}
    (<$) = fmap . const

------------------------------------------------------------------------------
-- concatMap
------------------------------------------------------------------------------

{-# INLINE_NORMAL concatMapM #-}
concatMapM :: Monad m => (a -> m (Stream m b)) -> Stream m a -> Stream m b
concatMapM f (Stream step state) = Stream step' (Left state)
  where
    {-# INLINE_LATE step' #-}
    step' gst (Left st) = do
        r <- step (adaptState gst) st
        case r of
            Yield a s -> do
                b_stream <- f a
                return $ Skip (Right (b_stream, s))
            Skip s -> return $ Skip (Left s)
            Stop -> return Stop

    -- XXX flattenArrays is 5x faster than "concatMap fromArray". if somehow we
    -- can get inner_step to inline and fuse here we can perhaps get the same
    -- performance using "concatMap fromArray".
    --
    -- XXX using the pattern synonym "Stream" causes a major performance issue
    -- here even if the synonym does not include an adaptState call. Need to
    -- find out why. Is that something to be fixed in GHC?
    step' gst (Right (UnStream inner_step inner_st, st)) = do
        r <- inner_step (adaptState gst) inner_st
        case r of
            Yield b inner_s ->
                return $ Yield b (Right (Stream inner_step inner_s, st))
            Skip inner_s ->
                return $ Skip (Right (Stream inner_step inner_s, st))
            Stop -> return $ Skip (Left st)

{-# INLINE concatMap #-}
concatMap :: Monad m => (a -> Stream m b) -> Stream m a -> Stream m b
concatMap f = concatMapM (return . f)

-- XXX The idea behind this rule is to rewrite any calls to "concatMap
-- fromArray" automatically to flattenArrays which is much faster.  However, we
-- need an INLINE_EARLY on concatMap for this rule to fire. But if we use
-- INLINE_EARLY on concatMap or fromArray then direct uses of
-- "concatMap fromArray" (without the RULE) become much slower, this means
-- "concatMap f" in general would become slower. Need to find a solution to
-- this.
--
-- {-# RULES "concatMap Array.toStreamD"
--      concatMap Array.toStreamD = Array.flattenArray #-}

-- | Create a singleton 'Stream' from a pure value.
{-# INLINE_NORMAL yield #-}
yield :: Applicative m => a -> Stream m a
yield x = Stream (\_ s -> pure $ step undefined s) True
  where
    {-# INLINE_LATE step #-}
    step _ True  = Yield x False
    step _ False = Stop

{-# INLINE_NORMAL concatAp #-}
concatAp :: Functor f => Stream f (a -> b) -> Stream f a -> Stream f b
concatAp (Stream stepa statea) (Stream stepb stateb) = Stream step' (Left statea)
  where
    {-# INLINE_LATE step' #-}
    step' gst (Left st) = fmap
        (\r -> case r of
            Yield f s -> Skip (Right (f, s, stateb))
            Skip    s -> Skip (Left s)
            Stop      -> Stop)
        (stepa (adaptState gst) st)
    step' gst (Right (f, os, st)) = fmap
        (\r -> case r of
            Yield a s -> Yield (f a) (Right (f, os, s))
            Skip s    -> Skip (Right (f,os, s))
            Stop      -> Skip (Left os))
        (stepb (adaptState gst) st)

{-# INLINE_NORMAL apSequence #-}
apSequence :: Functor f => Stream f a -> Stream f b -> Stream f b
apSequence (Stream stepa statea) (Stream stepb stateb) =
    Stream step (Left statea)

    where

    {-# INLINE_LATE step #-}
    step gst (Left st) =
        fmap
            (\r ->
                 case r of
                     Yield _ s -> Skip (Right (s, stateb))
                     Skip s -> Skip (Left s)
                     Stop -> Stop)
            (stepa (adaptState gst) st)
    step gst (Right (ostate, st)) =
        fmap
            (\r ->
                 case r of
                     Yield b s -> Yield b (Right (ostate, s))
                     Skip s -> Skip (Right (ostate, s))
                     Stop -> Skip (Left ostate))
            (stepb gst st)

{-# INLINE_NORMAL apDiscardSnd #-}
apDiscardSnd :: Functor f => Stream f a -> Stream f b -> Stream f a
apDiscardSnd (Stream stepa statea) (Stream stepb stateb) =
    Stream step (Left statea)

    where

    {-# INLINE_LATE step #-}
    step gst (Left st) =
        fmap
            (\r ->
                 case r of
                     Yield b s -> Skip (Right (s, stateb, b))
                     Skip s -> Skip (Left s)
                     Stop -> Stop)
            (stepa gst st)
    step gst (Right (ostate, st, b)) =
        fmap
            (\r ->
                 case r of
                     Yield _ s -> Yield b (Right (ostate, s, b))
                     Skip s -> Skip (Right (ostate, s, b))
                     Stop -> Skip (Left ostate))
            (stepb (adaptState gst) st)

instance Applicative f => Applicative (Stream f) where
    {-# INLINE pure #-}
    pure = yield

    {-# INLINE (<*>) #-}
    (<*>) = concatAp

#if MIN_VERSION_base(4,10,0)
    {-# INLINE liftA2 #-}
    liftA2 f x = (<*>) (fmap f x)
#endif

    {-# INLINE (*>) #-}
    (*>) = apSequence

    {-# INLINE (<*) #-}
    (<*) = apDiscardSnd

-- NOTE: even though concatMap for StreamD is 4x faster compared to StreamK,
-- the monad instance does not seem to be significantly faster.
instance Monad m => Monad (Stream m) where
    {-# INLINE return #-}
    return = pure

    {-# INLINE (>>=) #-}
    (>>=) = flip concatMap

    {-# INLINE (>>) #-}
    (>>) = (*>)

instance MonadTrans Stream where
    {-# INLINE lift #-}
    lift = yieldM

instance (MonadThrow m) => MonadThrow (Stream m) where
    throwM = lift . throwM

-- XXX Use of SPEC constructor in folds causes 2x performance degradation in
-- one shot operations, but helps immensely in operations composed of multiple
-- combinators or the same combinator many times. There seems to be an
-- opportunity to optimize here, can we get both, better perf for single ops
-- as well as composed ops? Without SPEC, all single operation benchmarks
-- become 2x faster.

-- The way we want a left fold to be strict, dually we want the right fold to
-- be lazy.  The correct signature of the fold function to keep it lazy must be
-- (a -> m b -> m b) instead of (a -> b -> m b). We were using the latter
-- earlier, which is incorrect. In the latter signature we have to feed the
-- value to the fold function after evaluating the monadic action, depending on
-- the bind behavior of the monad, the action may get evaluated immediately
-- introducing unnecessary strictness to the fold. If the implementation is
-- lazy the following example, must work:
--
-- S.foldrM (\x t -> if x then return t else return False) (return True)
--  (S.fromList [False,undefined] :: SerialT IO Bool)
--
{-# INLINE_NORMAL foldrM #-}
foldrM :: Monad m => (a -> m b -> m b) -> m b -> Stream m a -> m b
foldrM f z (Stream step state) = go SPEC state
  where
    {-# INLINE_LATE go #-}
    go !_ st = do
          r <- step defState st
          case r of
            Yield x s -> f x (go SPEC s)
            Skip s    -> go SPEC s
            Stop      -> z

{-# INLINE_NORMAL foldrMx #-}
foldrMx :: Monad m
    => (a -> m x -> m x) -> m x -> (m x -> m b) -> Stream m a -> m b
foldrMx fstep final convert (Stream step state) = convert $ go SPEC state
  where
    {-# INLINE_LATE go #-}
    go !_ st = do
          r <- step defState st
          case r of
            Yield x s -> fstep x (go SPEC s)
            Skip s    -> go SPEC s
            Stop      -> final

-- Note that foldr works on pure values, therefore it becomes necessarily
-- strict when the monad m is strict. In that case it cannot terminate early,
-- it would evaluate all of its input.  Though, this should work fine with lazy
-- monads. For example, if "any" is implemented using "foldr" instead of
-- "foldrM" it performs the same with Identity monad but performs 1000x slower
-- with IO monad.
--
{-# INLINE_NORMAL foldr #-}
foldr :: Monad m => (a -> b -> b) -> b -> Stream m a -> m b
foldr f z = foldrM (\a b -> liftA2 f (return a) b) (return z)

-- | Create a singleton 'Stream' from a monadic action.
{-# INLINE_NORMAL yieldM #-}
yieldM :: Monad m => m a -> Stream m a
yieldM m = Stream step True
  where
    {-# INLINE_LATE step #-}
    step _ True  = m >>= \x -> return $ Yield x False
    step _ False = return Stop

-- this performs horribly, should not be used
{-# INLINE_NORMAL foldrS #-}
foldrS
    :: Monad m
    => (a -> Stream m b -> Stream m b)
    -> Stream m b
    -> Stream m a
    -> Stream m b
foldrS f final (Stream step state) = go SPEC state
  where
    {-# INLINE_LATE go #-}
    go !_ st = do
        -- defState??
        r <- yieldM $ step defState st
        case r of
          Yield x s -> f x (go SPEC s)
          Skip s    -> go SPEC s
          Stop      -> final

-- Right fold to some transformer (T) monad.  This can be useful to implement
-- stateless combinators like map, filtering, insertions, takeWhile, dropWhile.
--
{-# INLINE_NORMAL foldrT #-}
foldrT :: (Monad m, Monad (t m), MonadTrans t)
    => (a -> t m b -> t m b) -> t m b -> Stream m a -> t m b
foldrT f final (Stream step state) = go SPEC state
  where
    {-# INLINE_LATE go #-}
    go !_ st = do
          r <- lift $ step defState st
          case r of
            Yield x s -> f x (go SPEC s)
            Skip s    -> go SPEC s
            Stop      -> final

{-# INLINE_NORMAL toList #-}
toList :: Monad m => Stream m a -> m [a]
toList = foldr (:) []

-- Use foldr/build fusion to fuse with list consumers
-- This can be useful when using the IsList instance
{-# INLINE_LATE toListFB #-}
toListFB :: (a -> b -> b) -> b -> Stream Identity a -> b
toListFB c n (Stream step state) = go state
  where
    go st = case runIdentity (step defState st) of
             Yield x s -> x `c` go s
             Skip s    -> go s
             Stop      -> n

{-# RULES "toList Identity" toList = toListId #-}
{-# INLINE_EARLY toListId #-}
toListId :: Stream Identity a -> Identity [a]
toListId s = Identity $ build (\c n -> toListFB c n s)

-- XXX run begin action only if the stream is not empty.
{-# INLINE_NORMAL foldlMx' #-}
foldlMx' :: Monad m => (x -> a -> m x) -> m x -> (x -> m b) -> Stream m a -> m b
foldlMx' fstep begin done (Stream step state) =
    begin >>= \x -> go SPEC x state
  where
    -- XXX !acc?
    {-# INLINE_LATE go #-}
    go !_ acc st = acc `seq` do
        r <- step defState st
        case r of
            Yield x s -> do
                acc' <- fstep acc x
                go SPEC acc' s
            Skip s -> go SPEC acc s
            Stop   -> done acc

{-# INLINE foldlx' #-}
foldlx' :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Stream m a -> m b
foldlx' fstep begin done m =
    foldlMx' (\b a -> return (fstep b a)) (return begin) (return . done) m

-- XXX implement in terms of foldlMx'?
{-# INLINE_NORMAL foldlM' #-}
foldlM' :: Monad m => (b -> a -> m b) -> m b -> Stream m a -> m b
foldlM' fstep mbegin (Stream step state) = do
    begin <- mbegin
    go SPEC begin state
  where
    {-# INLINE_LATE go #-}
    go !_ acc st = acc `seq` do
        r <- step defState st
        case r of
            Yield x s -> do
                acc' <- fstep acc x
                go SPEC acc' s
            Skip s -> go SPEC acc s
            Stop   -> return acc

{-# INLINE foldl' #-}
foldl' :: Monad m => (b -> a -> b) -> b -> Stream m a -> m b
foldl' fstep begin = foldlM' (\b a -> return (fstep b a)) (return begin)

-- | Convert a list of pure values to a 'Stream'
{-# INLINE_LATE fromList #-}
fromList :: Applicative m => [a] -> Stream m a
fromList = Stream step
  where
    {-# INLINE_LATE step #-}
    step _ (x:xs) = pure $ Yield x xs
    step _ []     = pure Stop

------------------------------------------------------------------------------
-- Comparisons
------------------------------------------------------------------------------

{-# INLINE_NORMAL eqBy #-}
eqBy :: Monad m => (a -> b -> Bool) -> Stream m a -> Stream m b -> m Bool
eqBy eq (Stream step1 t1) (Stream step2 t2) = eq_loop0 SPEC t1 t2
  where
    eq_loop0 !_ s1 s2 = do
      r <- step1 defState s1
      case r of
        Yield x s1' -> eq_loop1 SPEC x s1' s2
        Skip    s1' -> eq_loop0 SPEC   s1' s2
        Stop        -> eq_null s2

    eq_loop1 !_ x s1 s2 = do
      r <- step2 defState s2
      case r of
        Yield y s2'
          | eq x y    -> eq_loop0 SPEC   s1 s2'
          | otherwise -> return False
        Skip    s2'   -> eq_loop1 SPEC x s1 s2'
        Stop          -> return False

    eq_null s2 = do
      r <- step2 defState s2
      case r of
        Yield _ _ -> return False
        Skip s2'  -> eq_null s2'
        Stop      -> return True

-- | Compare two streams lexicographically
{-# INLINE_NORMAL cmpBy #-}
cmpBy
    :: Monad m
    => (a -> b -> Ordering) -> Stream m a -> Stream m b -> m Ordering
cmpBy cmp (Stream step1 t1) (Stream step2 t2) = cmp_loop0 SPEC t1 t2
  where
    cmp_loop0 !_ s1 s2 = do
      r <- step1 defState s1
      case r of
        Yield x s1' -> cmp_loop1 SPEC x s1' s2
        Skip    s1' -> cmp_loop0 SPEC   s1' s2
        Stop        -> cmp_null s2

    cmp_loop1 !_ x s1 s2 = do
      r <- step2 defState s2
      case r of
        Yield y s2' -> case x `cmp` y of
                         EQ -> cmp_loop0 SPEC s1 s2'
                         c  -> return c
        Skip    s2' -> cmp_loop1 SPEC x s1 s2'
        Stop        -> return GT

    cmp_null s2 = do
      r <- step2 defState s2
      case r of
        Yield _ _ -> return LT
        Skip s2'  -> cmp_null s2'
        Stop      -> return EQ

{-# INLINE_NORMAL take #-}
take :: Monad m => Int -> Stream m a -> Stream m a
take n (Stream step state) = n `seq` Stream step' (state, 0)
  where
    {-# INLINE_LATE step' #-}
    step' gst (st, i) | i < n = do
        r <- step gst st
        return $ case r of
            Yield x s -> Yield x (s, i + 1)
            Skip s    -> Skip (s, i)
            Stop      -> Stop
    step' _ (_, _) = return Stop

------------------------------------------------------------------------------
-- Grouping/Splitting
------------------------------------------------------------------------------

-- s = stream state, fs = fold state
{-# ANN type GroupState Fuse #-}
data GroupState s fs b a
    = GroupStart s
    | GroupConsume s fs a
    | GroupBuffer s fs
    | GroupYield b (GroupState s fs b a)
    | GroupFinish

-- XXX Remove GroupConsume
{-# INLINE_NORMAL foldMany #-}
foldMany :: Monad m => Fold m a b -> Stream m a -> Stream m b
foldMany (Fold fstep initial extract) (Stream step state) =
    Stream step' (GroupStart state)

    where

    {-# INLINE_LATE step' #-}
    step' _ (GroupStart st) = do
        -- fs = fold state
        fs <- initial
        return $ Skip (GroupBuffer st fs)
    step' _ (GroupConsume st fs x) = do
        fs' <- fstep fs x
        case fs' of
            FL.Done b -> return $ Skip (GroupYield b (GroupStart st))
            FL.Partial ps -> return $ Skip (GroupBuffer st ps)
    step' gst (GroupBuffer st fs) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> return $ Skip $ GroupConsume s fs x
            Skip s -> return $ Skip (GroupBuffer s fs)
            Stop -> do
                b <- extract fs
                return $ Skip (GroupYield b GroupFinish)
    step' _ (GroupYield b next) = return $ Yield b next
    step' _ GroupFinish = return Stop

{-# INLINE_NORMAL foldMany1 #-}
foldMany1 :: Monad m => Fold m a b -> Stream m a -> Stream m b
foldMany1 (Fold fstep initial extract) (Stream step state) =
    Stream step' (GroupStart state)

    where

    {-# INLINE_LATE step' #-}
    step' gst (GroupStart st) = do
        -- fs = fold state
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                fi <- initial
                return $ Skip $ GroupConsume s fi x
            Skip s -> return $ Skip (GroupStart s)
            Stop -> return $ Stop
    step' _ (GroupConsume st fs x) = do
        fs' <- fstep fs x
        case fs' of
            FL.Done b -> return $ Skip (GroupYield b (GroupStart st))
            FL.Partial ps -> return $ Skip (GroupBuffer st ps)
    step' gst (GroupBuffer st fs) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> return $ Skip $ GroupConsume s fs x
            Skip s -> return $ Skip (GroupBuffer s fs)
            Stop -> do
                b <- extract fs
                return $ Skip (GroupYield b GroupFinish)
    step' _ (GroupYield b next) = return $ Yield b next
    step' _ GroupFinish = return Stop

-- XXX Investigate performance
{-# INLINE groupsOf #-}
groupsOf :: Monad m => Int -> Fold m a b -> Stream m a -> Stream m b
groupsOf n fld = foldMany (FL.ltake n fld)

data GroupState2 s fs
    = GroupStart2 s
    | GroupBuffer2 s fs Int
    | GroupYield2 fs (GroupState2 s fs)
    | GroupFinish2

{-# INLINE_NORMAL groupsOf2 #-}
groupsOf2
    :: Monad m
    => Int
    -> m c
    -> Fold2 m c a b
    -> Stream m a
    -> Stream m b
groupsOf2 n input (Fold2 fstep inject extract) (Stream step state) =
    n `seq` Stream step' (GroupStart2 state)

    where

    {-# INLINE_LATE step' #-}
    step' _ (GroupStart2 st) = do
        -- XXX shall we use the Natural type instead? Need to check performance
        -- implications.
        when (n <= 0) $
            -- XXX we can pass the module string from the higher level API
            error $ "Streamly.Internal.Data.Stream.StreamD.Type.groupsOf: the size of "
                 ++ "groups [" ++ show n ++ "] must be a natural number"
        -- fs = fold state
        fs <- input >>= inject
        return $ Skip (GroupBuffer2 st fs 0)

    step' gst (GroupBuffer2 st fs i) = do
        r <- step (adaptState gst) st
        case r of
            Yield x s -> do
                !fs' <- fstep fs x
                let i' = i + 1
                return $
                    if i' >= n
                    then Skip (GroupYield2 fs' (GroupStart2 s))
                    else Skip (GroupBuffer2 s fs' i')
            Skip s -> return $ Skip (GroupBuffer2 s fs i)
            Stop -> return $ Skip (GroupYield2 fs GroupFinish2)

    step' _ (GroupYield2 fs next) = do
        r <- extract fs
        return $ Yield r next

    step' _ GroupFinish2 = return Stop
