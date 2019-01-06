-- |
-- Module      : Streamly.Transform.Traversable
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Transform.Traversable
    (
      scan
    , prescan
    , postscan
    )
where

import Streamly.Fold.Types (Fold(..))
import Streamly.Transform.Types ()
import qualified Data.Traversable as T

-- | Convert a strict left 'Fold' into a scan
{-# INLINE scan #-}
scan :: Fold a b -> [a] -> [b]
scan (Fold step begin done) as = foldr cons nil as begin
  where
    nil      x = done x:[]
    cons a k x = done x:(k $! step x a)

{-| Convert a `Fold` into a prescan for any `Traversable` type

    \"Prescan\" means that the last element of the scan is not included
-}
{-# INLINE prescan #-}
prescan :: Traversable t => Fold a b -> t a -> t b
prescan (Fold step begin done) as = bs
  where
    step' x a = (x', b)
      where
        x' = step x a
        b  = done x
    (_, bs) = T.mapAccumL step' begin as

{-| Convert a `Fold` into a postscan for any `Traversable` type

    \"Postscan\" means that the first element of the scan is not included
-}
{-# INLINE postscan #-}
postscan :: Traversable t => Fold a b -> t a -> t b
postscan (Fold step begin done) as = bs
  where
    step' x a = (x', b)
      where
        x' = step x a
        b  = done x'
    (_, bs) = T.mapAccumL step' begin as
