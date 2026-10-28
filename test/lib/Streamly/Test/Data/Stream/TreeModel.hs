-- |
-- Module      : Streamly.Test.Data.Stream.TreeModel
-- Copyright   : (c) 2026 Composewell Technologies
-- License     : BSD-3-Clause
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Reference models, on lists, of the tree traversal orders in
-- docs/Developer/tree-traversal.md, and trees to test them on.

module Streamly.Test.Data.Stream.TreeModel
    (
    -- * Trees
      exampleChildren
    , chainChildren
    , genForest
    , forestChildren

    -- * Reference models
    , isLeaf
    , refPre
    , refBatchPre
    , refPost
    , refBatchPost
    , refWide
    , refWidePost
    , refWideBatchPre
    , refWideBatchPost
    )
where

import Data.IntMap.Strict (IntMap)
import Test.QuickCheck (Gen, choose, frequency)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

-------------------------------------------------------------------------------
-- Trees
-------------------------------------------------------------------------------

-- | The example tree of docs/Developer/tree-traversal.md:
-- 1->[2,3], 2->[4,5], 3->[6,7], 4->[8], 6->[9]; 5, 7, 8 and 9 are leaves.
exampleChildren :: Int -> [Int]
exampleChildren n =
    case n of
        1 -> [2, 3]
        2 -> [4, 5]
        3 -> [6, 7]
        4 -> [8]
        6 -> [9]
        _ -> []

-- | Node 2 has the chain 4 -> 5 -> 6 -> 7, node 3 has the leaves 31 to 36.
chainChildren :: Int -> [Int]
chainChildren n =
    case n of
        1 -> [2, 3]
        2 -> [4]
        3 -> [31 .. 36]
        4 -> [5]
        5 -> [6]
        6 -> [7]
        _ -> []

-------------------------------------------------------------------------------
-- Reference models, on lists
-------------------------------------------------------------------------------

isLeaf :: (Int -> [Int]) -> Int -> Bool
isLeaf children = null . children

-- | Depth first pre-order, the order of concatIterate.
refPre :: (Int -> [Int]) -> [Int] -> [Int]
refPre children = concatMap (\x -> x : refPre children (children x))

-- | All the nodes of a stream, then its non-leaf nodes expanded one after
-- another, the order of wideDfsConcatIterate[Rev].
refBatchPre :: Bool -> (Int -> [Int]) -> [Int] -> [Int]
refBatchPre rev children xs =
    xs ++ concatMap (refBatchPre rev children . children) (order nonLeaves)

    where

    nonLeaves = filter (not . isLeaf children) xs
    order = if rev then reverse else id

refPost :: (Int -> [Int]) -> [Int] -> [Int]
refPost children = concatMap go

    where

    go x
        | isLeaf children x = [x]
        | otherwise = refPost children (children x) ++ [x]

-- | The leaves of a stream are emitted as it runs, then its non-leaf nodes
-- are expanded one after another, each followed by the node itself.
refBatchPost :: Bool -> (Int -> [Int]) -> [Int] -> [Int]
refBatchPost rev children xs =
    filter (isLeaf children) xs ++ concatMap go (order nonLeaves)

    where

    nonLeaves = filter (not . isLeaf children) xs
    order = if rev then reverse else id
    go c = refBatchPost rev children (children c) ++ [c]

-- | A queue of lists. A list yields its head, then its tail and the children
-- of the head go to the back of the queue.
refWide :: (Int -> [Int]) -> [Int] -> [Int]
refWide children roots = go [roots]

    where

    go [] = []
    go ([] : q) = go q
    go ((x : xs) : q) =
        x : go (q ++ [xs] ++ [children x | not (isLeaf children x)])

-- | Like refWide, but the queue entries are tagged with the id of the parent,
-- an empty list takes a turn in the queue to signal the end of the stream,
-- and a non-leaf node is emitted when its pending count drops to zero.
refWidePost :: (Int -> [Int]) -> [Int] -> [Int]
refWidePost children roots = go (1 :: Int) Map.empty [(0, roots)]

    where

    go _ _ [] = []
    go n m ((p, []) : q) =
        let (out, m1) = done p m
         in out ++ go n m1 q
    go n m ((p, x : xs) : q)
        | isLeaf children x = x : go n m (q ++ [(p, xs)])
        | otherwise =
            go (n + 1)
                (Map.insert n (x, p, 1 :: Int) (incr p m))
                (q ++ [(p, xs), (n, children x)])

    incr 0 m = m
    incr p m = Map.adjust (\(x, pp, c) -> (x, pp, c + 1)) p m

    done 0 m = ([], m)
    done p m =
        case m Map.! p of
            (x, pp, 1) ->
                let (out, m1) = done pp (Map.delete p m)
                 in (x : out, m1)
            (x, pp, c) -> ([], Map.insert p (x, pp, c - 1) m)

-- | One level at a time, the order of bfsConcatIterate[Rev]. With rev, a
-- level is expanded in the reverse of the order it was emitted.
refWideBatchPre :: Bool -> (Int -> [Int]) -> [Int] -> [Int]
refWideBatchPre rev children = go

    where

    order = if rev then reverse else id
    nonLeaves = filter (not . isLeaf children)
    go [] = []
    go lvl = lvl ++ go (concatMap children (order (nonLeaves lvl)))

-- | The leaves of each level as the level is expanded, then the non-leaf nodes
-- level by level from the deepest.
refWideBatchPost :: Bool -> (Int -> [Int]) -> [Int] -> [Int]
refWideBatchPost rev children roots =
    leaves roots ++ out ++ concat levels

    where

    order = if rev then reverse else id
    leaves = filter (isLeaf children)
    nonLeaves = filter (not . isLeaf children)
    (out, levels) = loop (order (nonLeaves roots)) []

    loop [] acc = ([], acc)
    loop lvl acc =
        let kids = concatMap children lvl
            (o, acc1) = loop (order (nonLeaves kids)) (lvl : acc)
         in (leaves kids ++ o, acc1)

-------------------------------------------------------------------------------
-- Random trees
-------------------------------------------------------------------------------

-- | A random forest of at most about 60 nodes, as the children of each node
-- and the roots. Nodes are numbered from 1 in the order they are created.
genForest :: Gen (IntMap [Int], [Int])
genForest = do
    nRoots <- choose (0, 3)
    let roots = [1 .. nRoots]
    m <- go (nRoots + 1) roots IntMap.empty
    return (m, roots)

    where

    go :: Int -> [Int] -> IntMap [Int] -> Gen (IntMap [Int])
    go _ [] m = return m
    go next (x : xs) m = do
        k <-
            if next > 60
            then return 0
            else frequency
                    [(4, return 0), (3, return 1), (2, return 2)
                    , (1, return 3), (1, return 5)]
        let cs = [next .. next + k - 1]
        go (next + k) (xs ++ cs) (IntMap.insert x cs m)

forestChildren :: IntMap [Int] -> Int -> [Int]
forestChildren m n = IntMap.findWithDefault [] n m

