-- |
-- Module      : Streamly.Internal.Data.Stream.Generate
-- Copyright   : (c) 2017 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
module Streamly.Internal.Data.Stream.Generate
    (
    -- * Primitives
      Stream.nil
    , Stream.nilM
    , Stream.cons
    , Stream.consM

    -- * From 'Unfold'
    , unfold

    -- * From Values
    , Stream.fromPure
    , Stream.fromEffect

    -- * Cyclic Elements
    , mfix

    -- * From Containers
    , fromFoldable
    , fromFoldableM

    -- * From memory
    , fromPtr
    )
where

import Control.Monad.IO.Class (MonadIO)
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)
import Streamly.Internal.Data.Stream.Type (Stream, fromStreamK, toStreamK)
import Streamly.Internal.Data.Unfold.Type (Unfold)

import qualified Streamly.Internal.Data.Stream.StreamD.Generate as D
import qualified Streamly.Internal.Data.Stream.StreamK.Type as K
import qualified Streamly.Internal.Data.Stream.Type as Stream

-- $setup
-- >>> :m
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Unfold as Unfold
-- >>> import qualified Streamly.Internal.Data.Stream as Stream


------------------------------------------------------------------------------
-- From Unfold
------------------------------------------------------------------------------

-- | Convert an 'Unfold' into a stream by supplying it an input seed.
--
-- >>> s = Stream.unfold (Unfold.replicateM 3) (putStrLn "hello")
-- >>> Stream.fold Fold.drain s
-- hello
-- hello
-- hello
--
-- /Pre-release/
{-# INLINE unfold #-}
unfold :: Monad m => Unfold m a b -> a -> Stream m b
unfold unf = Stream.fromStreamD . D.unfold unf

-- | We can define cyclic structures using @let@:
--
-- >>> let (a, b) = ([1, b], head a) in (a, b)
-- ([1,1],1)
--
-- The function @fix@ defined as:
--
-- >>> fix f = let x = f x in x
--
-- ensures that the argument of a function and its output refer to the same
-- lazy value @x@ i.e.  the same location in memory.  Thus @x@ can be defined
-- in terms of itself, creating structures with cyclic references.
--
-- >>> f ~(a, b) = ([1, b], head a)
-- >>> fix f
-- ([1,1],1)
--
-- 'Control.Monad.mfix' is essentially the same as @fix@ but for monadic
-- values.
--
-- Using 'mfix' for streams we can construct a stream in which each element of
-- the stream is defined in a cyclic fashion. The argument of the function
-- being fixed represents the current element of the stream which is being
-- returned by the stream monad. Thus, we can use the argument to construct
-- itself.
--
-- In the following example, the argument @action@ of the function @f@
-- represents the tuple @(x,y)@ returned by it in a given iteration. We define
-- the first element of the tuple in terms of the second.
--
-- >>> import Streamly.Internal.Data.Stream as Stream
-- >>> import System.IO.Unsafe (unsafeInterleaveIO)
--
-- >>> :{
-- main = Stream.fold (Fold.drainBy print) $ Stream.mfix f
--     where
--     f action = do
--         let incr n act = fmap ((+n) . snd) $ unsafeInterleaveIO act
--         x <- Stream.unfold Unfold.fromListM [incr 1 action, incr 2 action]
--         y <- Stream.unfold Unfold.fromList [4,5]
--         return (x, y)
-- :}
--
-- Note: you cannot achieve this by just changing the order of the monad
-- statements because that would change the order in which the stream elements
-- are generated.
--
-- Note that the function @f@ must be lazy in its argument, that's why we use
-- 'unsafeInterleaveIO' on @action@ because IO monad is strict.
--
-- /Pre-release/
{-# INLINE mfix #-}
mfix :: Monad m => (m a -> Stream m a) -> Stream m a
mfix f = fromStreamK $ K.mfix (toStreamK . f)

------------------------------------------------------------------------------
-- Conversions
------------------------------------------------------------------------------

-- |
-- >>> fromFoldable = Prelude.foldr Stream.cons Stream.nil
--
-- Construct a stream from a 'Foldable' containing pure values:
--
{-# INLINE fromFoldable #-}
fromFoldable :: Foldable f => f a -> Stream m a
fromFoldable = fromStreamK . K.fromFoldable

-- |
-- >>> fromFoldableM = Prelude.foldr Stream.consM Stream.nil
--
-- Construct a stream from a 'Foldable' containing monadic actions.
--
-- >>> Stream.fold Fold.toList $ Stream.fromFoldableM $ map return [1,2,3]
-- [1,2,3]
--
{-# INLINE fromFoldableM #-}
fromFoldableM :: (Monad m, Foldable f) => f (m a) -> Stream m a
fromFoldableM = Prelude.foldr Stream.consM Stream.nil

------------------------------------------------------------------------------
-- From pointers
------------------------------------------------------------------------------

{-# INLINE fromPtr #-}
fromPtr :: (MonadIO m, Storable a) => Int -> Ptr a -> Stream m a
fromPtr n = Stream.fromStreamD . D.fromPtr n
