-- |
-- Module      : Streamly.Internal.Data.Stream.Expand
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Expand a stream by combining two or more streams or by combining streams
-- with unfolds.

module Streamly.Internal.Data.Stream.Expand
    (
    -- * Binary Combinators (Linear)
    -- | Functions ending in the shape:
    --
    -- @Stream m a -> Stream m a -> Stream m a@.
    --
    -- The functions in this section have a linear or flat n-ary combining
    -- characterstics. It means that when combined @n@ times (e.g. @a `serial`
    -- b `serial` c ...@) the resulting expression will have an @O(n)@
    -- complexity (instead O(n^2) for pair wise combinators described in the
    -- next section. These functions can be used efficiently with
    -- 'concatMapWith' et. al.  combinators that combine streams in a linear
    -- fashion (contrast with 'concatPairsWith' which combines streams as a
    -- binary tree).

      append
    -- * Binary Combinators (Pair Wise)
    -- | Like the functions in the section above these functions also combine
    -- two streams into a single stream but when used @n@ times linearly they
    -- exhibit O(n^2) complexity. They are best combined in a binary tree
    -- fashion using 'concatPairsWith' giving a @n * log n@ complexity.  Avoid
    -- using these with 'concatMapWith' when combining a large or infinite
    -- number of streams.

    -- ** Append
    , append2

    -- ** Zip
    , zipWith
    , zipWithM

    -- ** Merge
    , mergeBy
    , mergeByM
    , mergeByM2
    , mergeMinBy
    , mergeFstBy

    -- * Combine Streams and Unfolds
    -- |
    -- Expand a stream by repeatedly using an unfold and merging the resulting
    -- streams.  Functions generally ending in the shape:
    --
    -- @Unfold m a b -> Stream m a -> Stream m b@

    -- ** Append Many (Unfold)
    -- | Unfold and flatten streams.
    , unfoldMany

    -- ** Interpose
    -- | Insert effects between streams. Like unfoldMany but intersperses an
    -- effect between the streams. A special case of gintercalate.
    , interpose
    , interposeSuffix
    -- , interposeBy

    -- ** Intercalate
    -- | Insert Streams between Streams.
    -- Like unfoldMany but intersperses streams from another source between
    -- the streams from the first source.
    , intercalate
    , intercalateSuffix
    , gintercalate
    , gintercalateSuffix

    -- * Append Many (concatMap)
    -- | Map and serially append streams. 'concatMapM' is a generalization of
    -- the binary append operation to append many streams.
    , concatMapM
    , concatMap
    , concatM
    , concat

    -- * ConcatMapWith
    -- | Map and flatten a stream like 'concatMap' but using a custom binary
    -- stream merging combinator instead of just appending the streams.  The
    -- merging occurs sequentially, it works efficiently for 'serial', 'async',
    -- 'ahead' like merge operations where we consume one stream before the
    -- next or in case of 'wAsync' or 'parallel' where we consume all streams
    -- simultaneously anyway.
    --
    -- However, in cases where the merging consumes streams in a round robin
    -- fashion, a pair wise merging using 'concatPairsWith' would be more
    -- efficient. These cases include operations like 'mergeBy' or 'zipWith'.

    , concatMapWith
    , bindWith
    , concatSmapMWith

    -- * ConcatPairsWith
    -- | See the notes about suitable merge functions in the 'concatMapWith'
    -- section.
    , concatPairsWith

    -- * IterateMap
    -- | Map and flatten Trees of Streams
    , iterateMapWith
    , iterateSmapMWith
    , iterateMapLeftsWith
    , iterateUnfold
    )
where

#include "inline.hs"

import Streamly.Internal.Data.Stream.Bottom
    ( concatM, concatMapM, concatMap, smapM, fromPure, fromEffect
    , zipWith, zipWithM)
import Streamly.Internal.Data.Stream.Type
    ( Stream, fromStreamD, fromStreamK, toStreamD, toStreamK
    , bindWith, concatMapWith, nil)
import Streamly.Internal.Data.Unfold.Type (Unfold)

import qualified Streamly.Internal.Data.Stream.StreamD as D
import qualified Streamly.Internal.Data.Stream.StreamK as K (mergeBy, mergeByM)
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K

import Prelude hiding (concat, concatMap, zipWith)

-- $setup
-- >>> :m
-- >>> import Data.Either (either)
-- >>> import Data.IORef
-- >>> import Prelude hiding (zipWith, concatMap, concat)
-- >>> import qualified Streamly.Data.Array.Unboxed as Array
-- >>> import qualified Streamly.Internal.Data.Fold as Fold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Parser as Parser
-- >>> import qualified Streamly.Internal.FileSystem.Dir as Dir
--

------------------------------------------------------------------------------
-- Appending
------------------------------------------------------------------------------

infixr 6 `append2`

-- | Append the outputs of two streams, yielding all the elements from the
-- first stream and then yielding all the elements from the second stream.
--
-- IMPORTANT NOTE: This could be 100x faster than @serial/<>@ for appending a
-- few (say 100) streams because it can fuse via stream fusion. However, it
-- does not scale for a large number of streams (say 1000s) and becomes
-- qudartically slow. Therefore use this for custom appending of a few streams
-- but use 'concatMap' or 'concatMapWith serial' for appending @n@ streams or
-- infinite containers of streams.
--
-- /Pre-release/
{-# INLINE append2 #-}
append2 ::Monad m => Stream m b -> Stream m b -> Stream m b
append2 m1 m2 = fromStreamD $ D.append (toStreamD m1) (toStreamD m2)

infixr 6 `append`

-- | Appends two streams sequentially, yielding all elements from the first
-- stream, and then all elements from the second stream.
--
-- >>> s1 = Stream.unfold Unfold.fromList [1,2]
-- >>> s2 = Stream.unfold Unfold.fromList [3,4]
-- >>> Stream.fold Fold.toList $ s1 `Stream.append` s2
-- [1,2,3,4]
--
-- This operation can be used to fold an infinite lazy container of streams.
--
-- /Pre-release/
--
{-# INLINE append #-}
append :: Stream m a -> Stream m a -> Stream m a
append = (<>)

------------------------------------------------------------------------------
-- Merging (sorted streams)
------------------------------------------------------------------------------

-- | Merge two streams using a comparison function. The head elements of both
-- the streams are compared and the smaller of the two elements is emitted, if
-- both elements are equal then the element from the first stream is used
-- first.
--
-- If the streams are sorted in ascending order, the resulting stream would
-- also remain sorted in ascending order.
--
-- >>> s1 = Stream.unfold Unfold.fromList [1,3,5]
-- >>> s2 = Stream.unfold Unfold.fromList [2,4,6,8]
-- >>> Stream.fold Fold.toList $ Stream.mergeBy compare s1 s2
-- [1,2,3,4,5,6,8]
--
-- See also: 'mergeByM2'
--
{-# INLINE mergeBy #-}
mergeBy :: (a -> a -> Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeBy f m1 m2 = fromStreamK $ K.mergeBy f (toStreamK m1) (toStreamK m2)

-- | Like 'mergeBy' but with a monadic comparison function.
--
-- Merge two streams randomly:
--
-- @
-- > randomly _ _ = randomIO >>= \x -> return $ if x then LT else GT
-- > Stream.toList $ Stream.mergeByM randomly (Stream.fromList [1,1,1,1]) (Stream.fromList [2,2,2,2])
-- [2,1,2,2,2,1,1,1]
-- @
--
-- Merge two streams in a proportion of 2:1:
--
-- >>> :{
-- do
--  let s1 = Stream.unfold Unfold.fromList [1,1,1,1,1,1]
--      s2 = Stream.unfold Unfold.fromList [2,2,2]
--  let proportionately m n = do
--       ref <- newIORef $ cycle $ Prelude.concat [Prelude.replicate m LT, Prelude.replicate n GT]
--       return $ \_ _ -> do
--          r <- readIORef ref
--          writeIORef ref $ Prelude.tail r
--          return $ Prelude.head r
--  f <- proportionately 2 1
--  xs <- Stream.fold Fold.toList $ Stream.mergeByM f s1 s2
--  print xs
-- :}
-- [1,1,2,1,1,2,1,1,2]
--
-- See also: 'mergeByM2'
--
{-# INLINE mergeByM #-}
mergeByM
    :: Monad m
    => (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByM f m1 m2 = fromStreamK $ K.mergeByM f (toStreamK m1) (toStreamK m2)

-- | Like 'mergeByM' but much faster, works best when merging statically known
-- number of streams. When merging more than two streams try to merge pairs and
-- pair of pairs in a tree like structure.'mergeByM' works better with variable
-- number of streams being merged using 'concatPairsWith'.
--
-- /Internal/
{-# INLINE mergeByM2 #-}
mergeByM2
    :: Monad m
    => (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeByM2 f m1 m2 =
    fromStreamD $ D.mergeByM f (toStreamD m1) (toStreamD m2)

-- | Like 'mergeByM' but stops merging as soon as any of the two streams stops.
--
-- /Unimplemented/
{-# INLINABLE mergeMinBy #-}
mergeMinBy :: -- Monad m =>
    (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeMinBy _f _m1 _m2 = undefined
    -- fromStreamD $ D.mergeMinBy f (toStreamD m1) (toStreamD m2)

-- | Like 'mergeByM' but stops merging as soon as the first stream stops.
--
-- /Unimplemented/
{-# INLINABLE mergeFstBy #-}
mergeFstBy :: -- Monad m =>
    (a -> a -> m Ordering) -> Stream m a -> Stream m a -> Stream m a
mergeFstBy _f _m1 _m2 = undefined
    -- fromStreamK $ D.mergeFstBy f (toStreamD m1) (toStreamD m2)

------------------------------------------------------------------------------
-- Combine N Streams - unfoldMany
------------------------------------------------------------------------------

-- | Like 'concatMap' but uses an 'Unfold' for stream generation. Unlike
-- 'concatMap' this can fuse the 'Unfold' code with the inner loop and
-- therefore provide many times better performance.
--
{-# INLINE unfoldMany #-}
unfoldMany ::Monad m => Unfold m a b -> Stream m a -> Stream m b
unfoldMany u m = fromStreamD $ D.unfoldMany u (toStreamD m)

------------------------------------------------------------------------------
-- Combine N Streams - interpose
------------------------------------------------------------------------------

-- > interpose x unf str = gintercalate unf str UF.identity (repeat x)

-- | Unfold the elements of a stream, intersperse the given element between the
-- unfolded streams and then concat them into a single stream.
--
-- >>> unwords = Stream.interpose ' '
--
-- /Pre-release/
{-# INLINE interpose #-}
interpose :: Monad m
    => c -> Unfold m b c -> Stream m b -> Stream m c
interpose x unf str =
    fromStreamD $ D.interpose (return x) unf (toStreamD str)

-- interposeSuffix x unf str = gintercalateSuffix unf str UF.identity (repeat x)

-- | Unfold the elements of a stream, append the given element after each
-- unfolded stream and then concat them into a single stream.
--
-- >>> unlines = Stream.interposeSuffix '\n'
--
-- /Pre-release/
{-# INLINE interposeSuffix #-}
interposeSuffix :: Monad m
    => c -> Unfold m b c -> Stream m b -> Stream m c
interposeSuffix x unf str =
    fromStreamD $ D.interposeSuffix (return x) unf (toStreamD str)

------------------------------------------------------------------------------
-- Combine N Streams - intercalate
------------------------------------------------------------------------------

-- XXX we can swap the order of arguments to gintercalate so that the
-- definition of unfoldMany becomes simpler? The first stream should be
-- infixed inside the second one. However, if we change the order in
-- "interleave" as well similarly, then that will make it a bit unintuitive.
--
-- > unfoldMany unf str =
-- >     gintercalate unf str (UF.nilM (\_ -> return ())) (repeat ())

-- | 'interleaveInfix' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalate #-}
gintercalate
    :: Monad m
    => Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
gintercalate unf1 str1 unf2 str2 =
    fromStreamD $ D.gintercalate
        unf1 (toStreamD str1)
        unf2 (toStreamD str2)

-- > intercalate unf seed str = gintercalate unf str unf (repeatM seed)

-- | 'intersperse' followed by unfold and concat.
--
-- >>> intercalate u a = Stream.unfoldMany u . Stream.intersperse a
-- >>> intersperse = Stream.intercalate Unfold.identity
-- >>> unwords = Stream.intercalate Unfold.fromList " "
--
-- >>> input = Stream.unfold Unfold.fromList ["abc", "def", "ghi"]
-- >>> Stream.fold Fold.toList $ Stream.intercalate Unfold.fromList " " input
-- "abc def ghi"
--
{-# INLINE intercalate #-}
intercalate :: Monad m
    => Unfold m b c -> b -> Stream m b -> Stream m c
intercalate unf seed str = fromStreamD $
    D.unfoldMany unf $ D.intersperse seed (toStreamD str)

-- | 'interleaveSuffix' followed by unfold and concat.
--
-- /Pre-release/
{-# INLINE gintercalateSuffix #-}
gintercalateSuffix
    :: Monad m
    => Unfold m a c -> Stream m a -> Unfold m b c -> Stream m b -> Stream m c
gintercalateSuffix unf1 str1 unf2 str2 =
    fromStreamD $ D.gintercalateSuffix
        unf1 (toStreamD str1)
        unf2 (toStreamD str2)

-- > intercalateSuffix unf seed str = gintercalateSuffix unf str unf (repeatM seed)

-- | 'intersperseSuffix' followed by unfold and concat.
--
-- >>> intercalateSuffix u a = Stream.unfoldMany u . Stream.intersperseSuffix a
-- >>> intersperseSuffix = Stream.intercalateSuffix Unfold.identity
-- >>> unlines = Stream.intercalateSuffix Unfold.fromList "\n"
--
-- >>> input = Stream.unfold Unfold.fromList ["abc", "def", "ghi"]
-- >>> Stream.fold Fold.toList $ Stream.intercalateSuffix Unfold.fromList "\n" input
-- "abc\ndef\nghi\n"
--
{-# INLINE intercalateSuffix #-}
intercalateSuffix :: Monad m
    => Unfold m b c -> b -> Stream m b -> Stream m c
intercalateSuffix unf seed str = fromStreamD $ D.unfoldMany unf
    $ D.intersperseSuffix (return seed) (toStreamD str)

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | Flatten a stream of streams to a single stream.
--
-- >>> concat = Stream.concatMap id
--
-- /Pre-release/
{-# INLINE concat #-}
concat :: Monad m => Stream m (Stream m a) -> Stream m a
concat = concatMap id

------------------------------------------------------------------------------
-- Combine N Streams - concatMap
------------------------------------------------------------------------------

-- | Like 'concatMapWith' but carries a state which can be used to share
-- information across multiple steps of concat.
--
-- >>> concatSmapMWith combine f initial = Stream.concatMapWith combine id . Stream.smapM f initial
--
-- /Pre-release/
--
{-# INLINE concatSmapMWith #-}
concatSmapMWith
    :: Monad m
    => (Stream m b -> Stream m b -> Stream m b)
    -> (s -> a -> m (s, Stream m b))
    -> m s
    -> Stream m a
    -> Stream m b
concatSmapMWith combine f initial =
    concatMapWith combine id . smapM f initial

-- XXX Implement a StreamD version for fusion.

-- | Combine streams in pairs using a binary stream combinator, then combine
-- the resulting streams in pairs recursively until we get to a single combined
-- stream.
--
-- For example, you can sort a stream using merge sort like this:
--
-- >>> s = Stream.unfold Unfold.fromList [5,1,7,9,2]
-- >>> generate = Stream.fromPure
-- >>> combine = Stream.mergeBy compare
-- >>> Stream.fold Fold.toList $ Stream.concatPairsWith combine generate s
-- [1,2,5,7,9]
--
-- /Caution: the stream of streams must be finite/
--
-- /Pre-release/
--
{-# INLINE concatPairsWith #-}
concatPairsWith ::
       (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> Stream m a
    -> Stream m b
concatPairsWith par f m =
    fromStreamK
        $ K.concatPairsWith
            (\s1 s2 -> toStreamK $ fromStreamK s1 `par` fromStreamK s2)
            (toStreamK . f)
            (toStreamK m)

------------------------------------------------------------------------------
-- IterateMap - Map and flatten Trees of Streams
------------------------------------------------------------------------------

-- | Like 'iterateM' but iterates after mapping a stream generator on the
-- output.
--
-- Yield an input element in the output stream, map a stream generator on it
-- and then do the same on the resulting stream. This can be used for a depth
-- first traversal of a tree like structure.
--
-- Note that 'iterateM' is a special case of 'iterateMapWith':
--
-- >>> iterateM f = Stream.iterateMapWith Stream.append (Stream.fromEffect . f) . Stream.fromEffect
--
-- It can be used to traverse a tree structure.  For example, to list a
-- directory tree:
--
-- >> input = Stream.fromPure (Left ".")
-- >> f = either Dir.toEither (const Stream.nil)
-- >> ls = Stream.iterateMapWith Stream.append f input
--
-- /Pre-release/
--
{-# INLINE iterateMapWith #-}
iterateMapWith ::
       (Stream m a -> Stream m a -> Stream m a)
    -> (a -> Stream m a)
    -> Stream m a
    -> Stream m a
iterateMapWith combine f = concatMapWith combine go
    where
    go x = fromPure x `combine` concatMapWith combine go (f x)

-- | Same as @iterateMapWith Stream.serial@ but more efficient due to stream
-- fusion.
--
-- /Unimplemented/
{-# INLINE iterateUnfold #-}
iterateUnfold :: -- (MonadAsync m) =>
    Unfold m a a -> Stream m a -> Stream m a
iterateUnfold = undefined

------------------------------------------------------------------------------
-- Flattening Graphs
------------------------------------------------------------------------------

-- To traverse graphs we need a state to be carried around in the traversal.
-- For example, we can use a hashmap to store the visited status of nodes.

-- | Like 'iterateMap' but carries a state in the stream generation function.
-- This can be used to traverse graph like structures, we can remember the
-- visited nodes in the state to avoid cycles.
--
-- Note that a combination of 'iterateMap' and 'usingState' can also be used to
-- traverse graphs. However, this function provides a more localized state
-- instead of using a global state.
--
-- See also: 'mfix'
--
-- /Pre-release/
--
{-# INLINE iterateSmapMWith #-}
iterateSmapMWith
    :: Monad m
    => (Stream m a -> Stream m a -> Stream m a)
    -> (b -> a -> m (b, Stream m a))
    -> m b
    -> Stream m a
    -> Stream m a
iterateSmapMWith combine f initial stream =
    concatMap
        (\b -> concatMapWith combine (go b) stream)
        (fromEffect initial)

    where

    go b a = fromPure a `combine` feedback b a

    feedback b a =
        concatMap
            (\(b1, s) -> concatMapWith combine (go b1) s)
            (fromEffect $ f b a)

------------------------------------------------------------------------------
-- Either streams
------------------------------------------------------------------------------

-- Keep concating either streams as long as rights are generated, stop as soon
-- as a left is generated and concat the left stream.
--
-- See also: 'handle'
--
-- /Unimplemented/
--
{-
concatMapEitherWith
    :: -- (MonadAsync m) =>
       (forall x. t m x -> t m x -> t m x)
    -> (a -> t m (Either (Stream m b) b))
    -> Stream m a
    -> Stream m b
concatMapEitherWith = undefined
-}

-- | In an 'Either' stream iterate on 'Left's.  This is a special case of
-- 'iterateMapWith':
--
-- >>> iterateMapLeftsWith combine f = Stream.iterateMapWith combine (either f (const Stream.nil))
--
-- To traverse a directory tree:
--
-- >> input = fromPure (Left ".")
-- >> ls = Stream.iterateMapLeftsWith Stream.append Dir.toEither input
--
-- /Pre-release/
--
{-# INLINE iterateMapLeftsWith #-}
iterateMapLeftsWith
    :: (b ~ Either a c)
    => (Stream m b -> Stream m b -> Stream m b)
    -> (a -> Stream m b)
    -> Stream m b
    -> Stream m b
iterateMapLeftsWith combine f =
    iterateMapWith combine (either f (const nil))
