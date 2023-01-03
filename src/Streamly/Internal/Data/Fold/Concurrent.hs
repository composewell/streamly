-- |
-- Module      : Streamly.Internal.Data.Fold.Concurrent
-- Copyright   : (c) 2022 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Eval a fold asynchronously in a separate thread.
--
-- Use multiple worker folds to fold a stream and collect the results using
-- another fold, combine using a monoid. The results can be collected
-- out-of-order or in-order.
--
-- Concurrent append: if one fold's buffer becomes full then use the next one
-- Concurrent interleave/partition: Round robin to n folds.
-- Concurrent distribute to multiple folds.

module Streamly.Internal.Data.Fold.Concurrent
    (
    -- * Imports
    -- $setup

    -- * Configuration
      Config
    , maxBuffer
    , inspect

    -- * Combinators
    -- | Stream combinators using Async channel

    , eval
    , parEval
    )
where

import Streamly.Internal.Control.Concurrent (MonadAsync)
import Streamly.Internal.Data.Fold (Fold)

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

-- | Evaluate a stream asynchronously using a channel and serve the consumer
-- from the evaluation buffer.
--
-- >>> eval = Fold.parEval id
--
{-# INLINE eval #-}
eval :: MonadAsync m => Fold m a b -> Fold m a b
eval = parEval id
