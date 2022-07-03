module Streamly.Internal.Data.Array.Tree.Type
  ( TreeArray (count, depth),
    newArray,
    getIndex,
    snoc,
    appendN,
    read,
  )
where

#include "inline.hs"

import Control.Monad.IO.Class (MonadIO (..))
import qualified Streamly.Internal.Data.Array.Foreign.Mut as UArray
import qualified Streamly.Internal.Data.Array.Mut.Type as BArray
import qualified Streamly.Internal.Data.Fold.Type as FL
import qualified Streamly.Internal.Data.Producer as Producer
import Streamly.Internal.Data.Producer.Type (Producer (..))
import qualified Streamly.Internal.Data.Stream.StreamD as D
import Streamly.Internal.Data.Unboxed (Storable)
import Streamly.Internal.Data.Unfold.Type (Unfold (..))
import Prelude hiding (read)

branchWidth :: Int
branchWidth = 2

leafWidth :: Int
leafWidth = 2

type Count = Int

type Depth = Int

type Index = Int

-- A tree of unboxed, mutable arrays with a pre-defined
-- fixed width each
data TreeArray a = TreeArray {count :: Count, depth :: Depth, _root :: TreeArrayInternal a}

data TreeArrayInternal a = Branch {branchArray :: BArray.Array (TreeArrayInternal a)} | Leaf {_leafArray :: UArray.Array a}

{-# INLINE newArray #-}
newArray :: MonadIO m => m (TreeArray a)
newArray = TreeArray 0 1 <$> newBranch

{-# INLINE newBranch #-}
newBranch :: MonadIO m => m (TreeArrayInternal a)
newBranch = Branch <$> BArray.newArrayUninitialized branchWidth

{-# INLINE newLeaf #-}
newLeaf :: (MonadIO m, Storable a) => m (TreeArrayInternal a)
newLeaf = Leaf <$> UArray.newArrayUnitialized leafWidth

{-# INLINE snoc #-}
snoc :: (MonadIO m, Storable a, Show a) => TreeArray a -> a -> m (TreeArray a)
snoc t@(TreeArray count d root) v =
  -- tree is full, add one layer of depth to it.
  if count == branchWidth ^ d * leafWidth
    then
      ( do
          newRoot <- newBranch
          _ <- BArray.putIndex (branchArray newRoot) 0 root
          childBranch <- newBranch
          _ <- BArray.putIndex (branchArray newRoot) 1 childBranch
          _ <- go d 0 v childBranch
          return $ TreeArray (count + 1) (d + 1) newRoot
      )
    else
      ( do
          _ <- go d count v root
          return $ t {count = count + 1}
      )
  where
    go :: (MonadIO m, Storable a, Show a) => Depth -> Index -> a -> TreeArrayInternal a -> m ()
    -- INVARIANT: go should never be called on leaf with
    -- an index exceeding leaf width
    go _ i v (Leaf leafArray) = UArray.putIndex i v leafArray
    go d i v b@(Branch branchArray) =
      let -- branch will always have a depth greater than 0
          -- total elements that can be stored by a full child tree
          fullChildTree = branchWidth ^ (d - 1) * leafWidth
          -- index of next branch to visit
          childIndex = i `quot` fullChildTree
          -- index spilled over from skipping children
          spillOver = i `rem` fullChildTree
       in case (d == 1, spillOver == 0) of
            -- reached last level and existing leafs are full
            -- allocate new leaf and snoc element to it
            (True, True) -> do
              leaf <- newLeaf
              _ <- BArray.putIndex branchArray childIndex leaf
              go 0 0 v leaf
            -- reached last level but existing leaf has space
            -- recurse into child
            (True, False) ->
              go 0 spillOver v =<< BArray.getIndex branchArray childIndex
            -- did not reach last level and existing leaf has space
            (False, False) ->
              go (d - 1) spillOver v =<< BArray.getIndex branchArray childIndex
            -- did not reach last level and existing leaf does not have
            -- space. Allocate a new child branch and recurse into it
            (False, True) -> do
              childBranch <- newBranch
              _ <- BArray.putIndex branchArray childIndex childBranch
              go (d - 1) 0 v childBranch

-- | Lookup the element at the given index. Index starts from 0.
--  /O(d)/ where d is the depth of the tree. depth of tree is proportional
--  to log of number of elements to the base of `childWidth`.
{-# INLINE getIndex #-}
getIndex :: (MonadIO m, Storable a) => TreeArray a -> Index -> m a
getIndex (TreeArray count depth treeArray) index = do
  if index < count then go depth index treeArray else invalidIndex "findAtOffset" index
  where
    go :: (MonadIO m, Storable a) => Depth -> Index -> TreeArrayInternal a -> m a
    go _ i (Leaf leafArray) = UArray.getIndex i leafArray
    go d i (Branch branchArray) =
      let -- branch will always have a depth greater than 0
          fullChildTree = branchWidth ^ (d - 1) * leafWidth
          -- index of next branch to visit
          childIndex = i `quot` fullChildTree
          -- index spilled over from skipping children
          spillOver = i `rem` fullChildTree
       in go (d - 1) spillOver =<< BArray.getIndex branchArray childIndex

{-# INLINE_NORMAL appendN #-}
appendN ::
  forall m a.
  (MonadIO m, Storable a, Show a) =>
  m (TreeArray a) ->
  Int ->
  FL.Fold m a (TreeArray a)
appendN action n = FL.take n $ FL.foldlM' step initial
  where
    initial = action
    step = snoc

invalidIndex :: String -> Int -> a
invalidIndex label i =
  error $ label ++ " : invalid array index " ++ show i

-- | Resumable unfold of a tree array.

{-# INLINE_NORMAL producer #-}
producer :: (MonadIO m, Storable a) => Producer m (TreeArray a) a
producer = Producer step inject extract
  where
    {-# INLINE inject #-}
    inject arr = return (arr, 0)

    {-# INLINE extract #-}
    extract (arr, _) = return arr

    {-# INLINE_LATE step #-}
    step (arr, i)
      | i >= count arr = return D.Stop
    step (arr, i) = do
      x <- getIndex arr i
      return $ D.Yield x (arr, i + 1)

-- | Unfold an array into a stream.

{-# INLINE_NORMAL read #-}
read :: (MonadIO m, Storable a) => Unfold m (TreeArray a) a
read = Producer.simplify producer
