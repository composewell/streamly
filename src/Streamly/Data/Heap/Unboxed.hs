{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Streamly.Data.Heap.Unboxed
( heapInsert
, heapify
, pop
, sort
, sortBy
, test
, main
)
where

import Data.Bits
import Streamly.Internal.Data.Unboxed (Unbox)
import Streamly.Internal.Data.Array.Mut.Type (Array)
import qualified Streamly.Internal.Data.Array.Mut.Type as MA
import Prelude hiding (read, length)
import Data.Word (Word8)

type Comparison a = a -> a -> Ordering

{-# INLINE siftByOffset #-}
siftByOffset :: (Unbox a)
    => Comparison a
    -> Array a
    -> a            -- ^ value
    -> Int          -- ^ start index
    -> Int          -- ^ array length
    -> IO ()
siftByOffset cmp arr val0 start len0 = siftDown val0 start len0
    where

    siftDown val root len
        | child < len = do
            (child', ac) <- maximumChild cmp arr child len
            case cmp val ac of
                LT -> MA.putIndexUnsafe root arr ac >> siftDown val child' len
                _  -> MA.putIndexUnsafe root arr val
        | otherwise = MA.putIndexUnsafe root arr val
        where child = root `shiftL` 2 + 1

-- Finds the maximum child of a heap node, given the indx of the first child.
{-# INLINE maximumChild #-}
maximumChild :: (Unbox a)
             => Comparison a -> Array a -> Int -> Int -> IO (Int,  a)
maximumChild cmp a child1 len
  | child4 < len = do ac1 <- MA.getIndexUnsafe child1  a
                      ac2 <- MA.getIndexUnsafe child2 a
                      ac3 <- MA.getIndexUnsafe child3 a
                      ac4 <- MA.getIndexUnsafe child4 a
                      return $ case cmp ac1 ac2 of
                                 LT -> case cmp ac2 ac3 of
                                         LT -> case cmp ac3 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child3, ac3)
                                         _  -> case cmp ac2 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child2, ac2)
                                 _  -> case cmp ac1 ac3 of
                                         LT -> case cmp ac3 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child3, ac3)
                                         _  -> case cmp ac1 ac4 of
                                                 LT -> (child4, ac4)
                                                 _  -> (child1, ac1)
  | child3 < len = do ac1 <- MA.getIndexUnsafe child1  a
                      ac2 <- MA.getIndexUnsafe child2 a
                      ac3 <- MA.getIndexUnsafe child3 a
                      return $ case cmp ac1 ac2 of
                                 LT -> case cmp ac2 ac3 of
                                         LT -> (child3, ac3)
                                         _  -> (child2, ac2)
                                 _  -> case cmp ac1 ac3 of
                                         LT -> (child3, ac3)
                                         _  -> (child1, ac1)
  | child2 < len = do ac1 <- MA.getIndexUnsafe child1  a
                      ac2 <- MA.getIndexUnsafe child2 a
                      return $ case cmp ac1 ac2 of
                                 LT -> (child2, ac2)
                                 _  -> (child1, ac1)
  | otherwise    = do ac1 <- MA.getIndexUnsafe child1  a ; return (child1, ac1)
 where
 child2 = child1 + 1
 child3 = child1 + 2
 child4 = child1 + 3

{-# INLINE heapify #-}
heapify :: (Unbox a)
    => Comparison a
    -> Array a
    -> Int -- ^ lower index, l (offset)
    -> Int -- ^ upper index, u
    -> IO ()
heapify cmp arr l u = loop $ (len - 1) `shiftR` 2
    where
    len = u - l
    loop k          -- k is the parent index
        | k < 0     = return ()
        | otherwise = MA.getIndexUnsafe (l+k) arr >>= \e ->
                        siftByOffset cmp arr e k len >> loop (k - 1)

-- | Given a heap stored in a portion of an array [l,u) swaps the top
-- of the heap with the element at position t, and rebuilds the heap.
{-# INLINE popTo #-}
popTo :: forall a.(Unbox a)
  => Comparison a
  -> Array a
  -> Int        -- ^ lower heap index, l
  -> Int        -- ^ upper heap index, u
  -> Int        -- ^ index to pop to, t
  -> IO ()
popTo cmp arr l u t = do
    al <- MA.getIndexUnsafe l arr
    at <- MA.getIndexUnsafe t arr
    MA.putIndexUnsafe t arr al
    siftByOffset cmp arr at 0 (u - l)

{-# INLINE pop #-}
pop
  :: (Unbox a)
  => Comparison a
  -> Array a
  -> Int -- ^ lower heap index, l
  -> Int -- ^ upper heap index, u
  -> IO ()
pop cmp a l u = popTo cmp a l u u

{-# INLINE sortHeap #-}
sortHeap
  :: forall a. (Unbox a)
  => Comparison a
  -> Array a
  -> Int -- ^ lower heap index, l
  -> Int -- ^ lower bound of final sorted portion, m
  -> Int -- ^ upper heap index, u
  -> IO ()
sortHeap cmp a l0 m0 u = loop (u-1) >> unsafeSwap a l0 m0
 where
 loop k
   | m0 < k     = pop cmp a l0 k >> loop (k-1)
   | otherwise = return ()
 unsafeSwap arr l m = do
    al <- MA.getIndexUnsafe  l arr
    at <- MA.getIndexUnsafe  m arr
    MA.putIndexUnsafe l arr at
    MA.putIndexUnsafe m arr al

-- | Sorts an entire array using the default ordering.
{-# INLINABLE sort #-}
sort :: (Unbox a, Ord a) => Array a -> Int -> IO ()
sort = sortBy compare

-- | Sorts an entire array using a custom ordering.
{-# INLINE sortBy #-}
sortBy :: (Unbox a) => Comparison a -> Array a -> Int -> IO ()
sortBy cmp arr = sortByBounds cmp arr 0

-- | Sorts the elements at the two given indices using the comparison. This
-- is essentially a compare-and-swap, although the first index is assumed to
-- be the 'lower' of the two.
{-# INLINABLE sort2ByIndex #-}
sort2ByIndex :: (Unbox a)
    => Comparison a -> Array a -> Int -> Int -> IO ()
sort2ByIndex cmp a i j = do
    a0 <- MA.getIndexUnsafe i a
    a1 <- MA.getIndexUnsafe j a
    case cmp a0 a1 of
        GT -> MA.putIndexUnsafe i a a1 >> MA.putIndexUnsafe j a a0
        _  -> return ()

sort3ByIndex :: (Unbox a)
    => Comparison a -> Array a -> Int -> Int -> Int -> IO ()
sort3ByIndex cmp a i j k = do
    a0 <- MA.getIndexUnsafe i a
    a1 <- MA.getIndexUnsafe j a
    a2 <- MA.getIndexUnsafe k a
    case cmp a0 a1 of
        GT -> case cmp a0 a2 of
                GT -> case cmp a2 a1 of
                        LT -> do
                            MA.putIndexUnsafe i a a2
                            MA.putIndexUnsafe k a a0
                        _  -> do
                            MA.putIndexUnsafe i a a1
                            MA.putIndexUnsafe j a a2
                            MA.putIndexUnsafe k a a0
                _  -> do
                    MA.putIndexUnsafe i a a1
                    MA.putIndexUnsafe j a a0
        _  -> case cmp a1 a2 of
                GT -> case cmp a0 a2 of
                        GT -> do
                            MA.putIndexUnsafe i a a2
                            MA.putIndexUnsafe j a a0
                            MA.putIndexUnsafe k a a1
                        _  -> do
                            MA.putIndexUnsafe j a a2
                            MA.putIndexUnsafe k a a1
                _  -> return ()

sort4ByIndex :: (Unbox a)
    => Comparison a -> Array a -> Int -> Int -> Int -> Int -> IO ()
sort4ByIndex cmp a i j k l  = do
  a0 <- MA.getIndexUnsafe i a
  a1 <- MA.getIndexUnsafe j a
  a2 <- MA.getIndexUnsafe k a
  a3 <- MA.getIndexUnsafe l a
  case cmp a0 a1 of
    GT -> case cmp a0 a2 of
            GT -> case cmp a1 a2 of
                    GT -> case cmp a1 a3 of
                            GT -> case cmp a2 a3 of
                                    GT -> do MA.putIndexUnsafe i a a3
                                             MA.putIndexUnsafe j a a2
                                             MA.putIndexUnsafe k a a1
                                             MA.putIndexUnsafe l a a0
                                    _  -> do MA.putIndexUnsafe i a a2
                                             MA.putIndexUnsafe j a a3
                                             MA.putIndexUnsafe k a a1
                                             MA.putIndexUnsafe l a a0
                            _  -> case cmp a0 a3 of
                                    GT -> do MA.putIndexUnsafe i a a2
                                             MA.putIndexUnsafe j a a1
                                             MA.putIndexUnsafe k a a3
                                             MA.putIndexUnsafe l a a0
                                    _  -> do MA.putIndexUnsafe i a a2
                                             MA.putIndexUnsafe j a a1
                                             MA.putIndexUnsafe k a a0
                                             MA.putIndexUnsafe l a a3
                    _ -> case cmp a2 a3 of
                           GT -> case cmp a1 a3 of
                                   GT -> do MA.putIndexUnsafe i a a3
                                            MA.putIndexUnsafe j a a1
                                            MA.putIndexUnsafe k a a2
                                            MA.putIndexUnsafe l a a0
                                   _  -> do MA.putIndexUnsafe i a a1
                                            MA.putIndexUnsafe j a a3
                                            MA.putIndexUnsafe k a a2
                                            MA.putIndexUnsafe l a a0
                           _  -> case cmp a0 a3 of
                                   GT -> do MA.putIndexUnsafe i a a1
                                            MA.putIndexUnsafe j a a2
                                            MA.putIndexUnsafe k a a3
                                            MA.putIndexUnsafe l a a0
                                   _  -> do MA.putIndexUnsafe i a a1
                                            MA.putIndexUnsafe j a a2
                                            MA.putIndexUnsafe k a a0
                                            -- MA.putIndexUnsafe l a a3
            _  -> case cmp a0 a3 of
                    GT -> case cmp a1 a3 of
                            GT -> do MA.putIndexUnsafe i a a3
                                     -- MA.putIndexUnsafe j a a1
                                     MA.putIndexUnsafe k a a0
                                     MA.putIndexUnsafe l a a2
                            _  -> do MA.putIndexUnsafe i a a1
                                     MA.putIndexUnsafe j a a3
                                     MA.putIndexUnsafe k a a0
                                     MA.putIndexUnsafe l a a2
                    _  -> case cmp a2 a3 of
                            GT -> do MA.putIndexUnsafe i a a1
                                     MA.putIndexUnsafe j a a0
                                     MA.putIndexUnsafe k a a3
                                     MA.putIndexUnsafe l a a2
                            _  -> do MA.putIndexUnsafe i a a1
                                     MA.putIndexUnsafe j a a0
                                     -- MA.putIndexUnsafe k a a2
                                     -- MA.putIndexUnsafe l a a3
    _  -> case cmp a1 a2 of
            GT -> case cmp a0 a2 of
                    GT -> case cmp a0 a3 of
                            GT -> case cmp a2 a3 of
                                    GT -> do MA.putIndexUnsafe i a a3
                                             MA.putIndexUnsafe j a a2
                                             MA.putIndexUnsafe k a a0
                                             MA.putIndexUnsafe l a a1
                                    _  -> do MA.putIndexUnsafe i a a2
                                             MA.putIndexUnsafe j a a3
                                             MA.putIndexUnsafe k a a0
                                             MA.putIndexUnsafe l a a1
                            _  -> case cmp a1 a3 of
                                    GT -> do MA.putIndexUnsafe i a a2
                                             MA.putIndexUnsafe j a a0
                                             MA.putIndexUnsafe k a a3
                                             MA.putIndexUnsafe l a a1
                                    _  -> do MA.putIndexUnsafe i a a2
                                             MA.putIndexUnsafe j a a0
                                             MA.putIndexUnsafe k a a1
                                             -- MA.putIndexUnsafe l a a3
                    _  -> case cmp a2 a3 of
                            GT -> case cmp a0 a3 of
                                    GT -> do MA.putIndexUnsafe i a a3
                                             MA.putIndexUnsafe j a a0
                                             -- MA.putIndexUnsafe k a a2
                                             MA.putIndexUnsafe l a a1
                                    _  -> do -- MA.putIndexUnsafe i a a0
                                             MA.putIndexUnsafe j a a3
                                             -- MA.putIndexUnsafe k a a2
                                             MA.putIndexUnsafe l a a1
                            _  -> case cmp a1 a3 of
                                    GT -> do -- MA.putIndexUnsafe i a a0
                                             MA.putIndexUnsafe j a a2
                                             MA.putIndexUnsafe k a a3
                                             MA.putIndexUnsafe l a a1
                                    _  -> do -- MA.putIndexUnsafe i a a0
                                             MA.putIndexUnsafe j a a2
                                             MA.putIndexUnsafe k a a1
                                             -- MA.putIndexUnsafe l a a3
            _  -> case cmp a1 a3 of
                    GT -> case cmp a0 a3 of
                            GT -> do MA.putIndexUnsafe i a a3
                                     MA.putIndexUnsafe j a a0
                                     MA.putIndexUnsafe k a a1
                                     MA.putIndexUnsafe l a a2
                            _  -> do -- MA.putIndexUnsafe i a a0
                                     MA.putIndexUnsafe j a a3
                                     MA.putIndexUnsafe k a a1
                                     MA.putIndexUnsafe l a a2
                    _  -> case cmp a2 a3 of
                            GT -> do -- MA.putIndexUnsafe i a a0
                                     -- MA.putIndexUnsafe j a a1
                                     MA.putIndexUnsafe k a a3
                                     MA.putIndexUnsafe l a a2
                            _  -> do -- MA.putIndexUnsafe i a a0
                                     -- MA.putIndexUnsafe j a a1
                                     -- MA.putIndexUnsafe k a a2
                                     -- MA.putIndexUnsafe l a a3
                                     return ()

-- | Sorts a portion of an array [l,u) using a custom ordering
sortByBounds
  :: (Unbox a)
  => Comparison a
  -> Array a
  -> Int -- ^ lower index, l
  -> Int -- ^ upper index, u
  -> IO ()
sortByBounds cmp a l u
    | len < 2   = return ()
    | len == 2  = sort2ByIndex cmp a l (l+1)
    | len == 3  = sort3ByIndex cmp a l (l+1) (l+2)
    | len == 4  = sort4ByIndex cmp a l (l+1) (l+2) (l+3)
    | otherwise =
        heapify cmp a l u
        >> sortHeap cmp a l (l+4) u
        >> sort4ByIndex cmp a l (l+1) (l+2) (l+3)
    where len = u - l
{-# INLINE sortByBounds #-}

heapInsert
  :: (Unbox a)
  => Comparison a
  -> Array a
  -> Int                -- ^ lower heap index, l
  -> Int                -- ^ upper heap index, u
  -> a                  -- ^ element to be inserted, e
  -> IO (Array a)
heapInsert cmp arr l u e = do
    aNext <- MA.snoc arr e
    siftUp (u - l) aNext
    where
    siftUp k arr0
        | k <= 0    = MA.putIndexUnsafe 0 arr0 e >> return arr0
        | otherwise = do
            let pIx = shiftR (k-1) 2        -- parent node index
            p <- MA.getIndexUnsafe (l + pIx) arr0
            case cmp p e of
                LT -> MA.putIndexUnsafe (l + k) arr0 p >> siftUp pIx arr0
                _  -> MA.putIndexUnsafe (l + k) arr0 e >> return arr0

display :: (Unbox a, Show a, Ord a)
    => Array a -> IO ()
display arr = do
    sort arr (MA.length arr)                -- total array length
    MA.toList arr >>= print

test :: IO ()
test = do
    arr <- MA.fromList (enumFromThenTo 100 99 (-99) :: [Int])
    MA.toList arr >>= print
    {-
    a1 <- MA.snoc @IO @Word8 arr 10
    a2 <- MA.snoc a1 4
    a3 <- MA.snoc a2 3
    a4 <- MA.snoc a3 2
    a5 <- MA.snoc a4 9
    a6 <- MA.snoc a5 20

    heapify compare a6 0 (MA.length a6)

    al0 <- MA.toList a6
    print al0

    a7 <- heapInsert compare a6 0 6 40     -- target index of inserted element
    a8 <- heapInsert compare a7 0 7 5
    a9 <- heapInsert compare a8 0 8 1
    -}
    display arr

test2 :: IO ()
test2 = do
    let arr = MA.nil
    a1 <- MA.snoc @IO @Word8 arr 10
    a2 <- MA.snoc a1 4
    a3 <- MA.snoc a2 3
    a4 <- MA.snoc a3 2
    a5 <- MA.snoc a4 9
    a6 <- MA.snoc a5 20
    heapify compare a6 0 (MA.length a6)
    a7 <- heapInsert compare a6 0 6 40     -- target index of inserted element
    a8 <- heapInsert compare a7 0 7 5
    a9 <- heapInsert compare a8 0 8 1
    sortHeap compare a9 0 0 (MA.length a9)
    MA.toList a9 >>= print

main :: IO ()
main = test >> test2
