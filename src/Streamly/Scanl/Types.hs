-- |
-- Module      : Streamly.Transform.Types
-- Copyright   : (c) 2019 Composewell Technologies
--               (c) 2013 Gabriel Gonzalez
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--

module Streamly.Transform.Types
    (
    )
where

-- For Scans the instances should behave just the way they behave for source
-- streams, we can name the scan types the same as the stream types:
--
-- 1) zip the corresponding elements from each scan
-- 2) nest (DFS/BFS)
--
-- Semigroup for scans:
-- 1) merge all the scans
-- 2) append (will require buffering the whole stream)
--
--
-- How would scans compose with streams and folds? We can already split a
-- stream into multiple parts and apply a different fold to each part. We can
-- also premap or prescan these folds, so we have all the possible facilities
-- that a scan can provide.
--
-- The only extra thing a scan provides is to split a stream into multiple
-- scans, apply a function to each part and then merge all the parts and then
-- apply a fold to the merged part. We can achieve this by applying different
-- folds to different parts of the stream and then merge the folds?
--
-- A Scan type may be useful to perform partial scans to implement
-- take/takeWhile primitives using a generalized scan primitive.
-- But since a stream itself is essentially a scan type, implementing
-- take/takeWhile in terms of stream itself is not bad.
