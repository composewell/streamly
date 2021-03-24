#include "inline.hs"

-- |
-- Module      : Streamly.Data.Unfold
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- 'Unfold' type represents an effectful action that generates a stream of
-- values from a single starting value often called a seed value. Values can be
-- generated and /pulled/ from the 'Unfold' one at a time. It can also be
-- called a producer or a source of stream.  It is a data representation of the
-- standard 'Streamly.Prelude.unfoldr' function.  An 'Unfold' can be converted
-- into a stream type using 'Streamly.Prelude.unfold' by supplying the seed.
--
-- = Performance Notes
--
-- 'Unfold' representation is more efficient than using streams when combining
-- streams.  'Unfold' type allows multiple unfold actions to be composed into a
-- single unfold function in an efficient manner by enabling the compiler to
-- perform stream fusion optimization.
-- @Unfold m a b@ can be considered roughly equivalent to an action @a -> t m
-- b@ (where @t@ is a stream type). Instead of using an 'Unfold' one could just
-- use a function of the shape @a -> t m b@. However, working with stream types
-- like t'Streamly.SerialT' does not allow the compiler to perform stream fusion
-- optimization when merging, appending or concatenating multiple streams.
-- Even though stream based combinator have excellent performance, they are
-- much less efficient when compared to combinators using 'Unfold'.  For
-- example, the 'Streamly.Prelude.concatMap' combinator which uses @a -> t m b@
-- (where @t@ is a stream type) to generate streams is much less efficient
-- compared to 'Streamly.Prelude.unfoldMany'.
--
-- On the other hand, transformation operations on stream types are as
-- efficient as transformations on 'Unfold'.
--
-- We should note that in some cases working with stream types may be more
-- convenient compared to working with the 'Unfold' type.  However, if extra
-- performance boost is important then 'Unfold' based composition should be
-- preferred compared to stream based composition when merging or concatenating
-- streams.
--
-- = Programmer Notes
--
-- > import qualified Streamly.Data.Unfold as UF
--
-- More, not yet exposed, unfold combinators can be found in
-- "Streamly.Internal.Data.Unfold".

-- The stream types (e.g. t'Streamly.SerialT') can be considered as a special
-- case of 'Unfold' with no starting seed.
--
module Streamly.Data.Unfold
    (
    -- * Unfold Type
      Unfold

    -- * Folding
    , fold

    -- * Unfolds
    -- One to one correspondence with
    -- "Streamly.Internal.Data.Stream.IsStream.Generate"

    -- ** Basic Constructors
    , unfoldrM
    , function
    , functionM

    -- ** Generators
    -- | Generate a monadic stream from a seed.
    , repeatM
    , replicateM
    , iterateM

    -- ** From Containers
    , fromList
    , fromListM
    , fromStream

    -- * Combinators
    -- ** Mapping on Input
    , lmap
    , lmapM

    -- ** Mapping on Output
    , mapM

    -- ** Filtering
    , takeWhileM
    , takeWhile
    , take
    , filter
    , filterM
    , drop
    , dropWhile
    , dropWhileM

    -- ** Zipping
    , zipWith

    -- ** Cross Product
    , crossWith

    -- ** Nesting
    , many

    -- ** Resource Management
    , finally
    , bracket

    -- ** Exceptions
    , onException
    , handle
    )
where

import Prelude hiding
    ( concat, map, mapM, takeWhile, take, filter, const, drop, dropWhile
    , zipWith
    )
import Streamly.Internal.Data.Unfold
