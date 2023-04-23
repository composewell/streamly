-- |
-- Module      : Streamly.Internal.Data.Fold.Concurrent
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Using 'parEval' a fold can be decoupled from the driver and evaluated
-- concurrently with the driver. The driver just pushes an element to the
-- fold's buffer and waits for async evaluation to finish.
--
-- Avoid scanning a stream using a concurrent fold. When scanning a stream
-- using a concurrent fold we need to keep in mind that the result of the scan
-- may be delayed because of the asynchronous execution. The results may not be
-- same as in the case of a synchronous fold.
--
-- Stages in a fold pipeline can be made concurrent using 'parEval'.
--
-- The 'demux' combinator can be made concurrent by using 'parEval' on the fold
-- returned by the fold-generating function. Thus, we can fold values for each
-- key in the input stream concurrently.
--
-- Similarly, we can use 'parEval' with other cobminators like 'toMap',
-- 'demuxToMap', 'classify', 'tee', 'distribute', 'partition' etc. Basically,
-- any combinator that composes multiple folds or multiple instances of a fold
-- is a good candidate for running folds concurrently.

-- TODO:
--
-- Before a fold returns "done" it has to drain the child folds. For example,
-- consider a "take" operation on a `parEval` fold, the take would return as
-- soon as it has taken required number of elements irrespective of whether the
-- child fold has yet finished or not.
--
-- parLmapM on a fold.
--
-- Use multiple worker folds to fold serial chunks of a stream and collect the
-- results using another fold, combine using a monoid. The results can be
-- collected out-of-order or in-order. This would be easier if each input
-- element is a streamable chunk and each fold consumes one at a time. This is
-- like parConcatMap in streams. We also need to have a lconcatMap to expand
-- the chunks in the input to streams before folding. This will require an
-- input Skip constructor. In fact, parLmapM would be implemented in terms of
-- this like in streams.
--
-- Concurrent append: if one fold's buffer becomes full then use the next one
-- Concurrent interleave/partition: Round robin to n folds.
-- Concurrent distribute to multiple folds.

module Streamly.Internal.Data.Fold.Concurrent
    (
    -- * Configuration
      Config
    , maxBuffer
    , bound
    , inspect

    -- * Combinators
    -- | Stream combinators using Async channel

    , parEval
    )
where

-- import Streamly.Internal.Control.Concurrent (MonadAsync)
-- import Streamly.Internal.Data.Fold (Fold)

import Streamly.Internal.Data.Fold.Concurrent.Channel

-- $setup
--
-- Imports for example snippets in this module.
--
-- >>> :m
-- >>> :set -XFlexibleContexts
-- >>> import Control.Concurrent (threadDelay)
-- >>> import qualified Streamly.Data.Array as Array
-- >>> import qualified Streamly.Data.Fold as Fold
-- >>> import qualified Streamly.Data.Parser as Parser
-- >>> import qualified Streamly.Data.Stream as Stream
-- >>> import qualified Streamly.Internal.Data.Fold.Concurrent as Fold
-- >>> import Prelude hiding (concatMap, concat)
-- >>> :{
--  delay n = do
--      threadDelay (n * 1000000)   -- sleep for n seconds
--      putStrLn (show n ++ " sec") -- print "n sec"
--      return n                    -- IO Int
-- :}

{-
-- | Evaluate a stream asynchronously using a channel and serve the consumer
-- from the evaluation buffer.
--
-- >>> eval = Fold.parEval id
--
{-# INLINE eval #-}
eval :: MonadAsync m => Fold m a b -> Fold m a b
eval = parEval id
-}
