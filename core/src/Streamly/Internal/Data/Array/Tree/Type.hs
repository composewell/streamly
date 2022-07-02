module Streamly.Internal.Data.Array.Tree.Type (TreeArray (count, depth), newArray, getIndex, snoc, appendN) where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Streamly.Internal.Data.Array.Foreign.Mut as UArray
import qualified Streamly.Internal.Data.Array.Mut.Type as BArray
import qualified Streamly.Internal.Data.Fold.Type as FL
import Streamly.Internal.Data.Unboxed (Storable)
import Debug.Trace (trace)

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

data TreeArrayInternal a = Branch (BArray.Array (TreeArrayInternal a)) | Leaf (UArray.Array a)

newArray :: MonadIO m => m (TreeArray a)
newArray = TreeArray 0 1 <$> newBranch

newBranch :: MonadIO m => m (TreeArrayInternal a)
newBranch = Branch <$> BArray.newArrayUninitialized branchWidth

newLeaf :: (MonadIO m, Storable a) => m (TreeArrayInternal a)
newLeaf = Leaf <$> UArray.newArrayUnitialized leafWidth

-- TODO: Handle case where leaf array becomes full
snoc :: (MonadIO m, Storable a, Show a) => a -> TreeArray a -> m (TreeArray a)
snoc v t@(TreeArray count depth b@(Branch branchArray)) =
  -- tree is full, add one layer of depth to it.
  if count == branchWidth ^ depth * leafWidth
    then
      ( do
          let copyBranch = Branch $ BArray.getSlice 0 branchWidth branchArray
          _ <- BArray.putIndex branchArray 0 copyBranch
          branch <- newBranch
          _ <- BArray.putIndex branchArray 1 branch
          _ <- go depth 0 v branch
          return $ TreeArray (count + 1) (depth + 1) b
      )
    else
      ( do
          _ <- go depth count v b
          return $ TreeArray (count + 1) depth b
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
          a = trace $ "depth " ++ show d ++ " index " ++ show i
       in case (depth == 1, spillOver == 0) of
            -- reached last level and existing leafs are full
            -- allocate new leaf and snoc element to it
            (True, True) -> do
              leaf <- newLeaf
              _ <- BArray.putIndex branchArray childIndex leaf
              go 0 0 v leaf
            (True, False) -> do
              leaf <- BArray.getIndex branchArray childIndex
              go 0 spillOver v leaf
            (False, False) -> do
              go (d - 1) spillOver v b
            (False, True) -> do
              branch <- newBranch
              _ <- BArray.putIndex branchArray childIndex branch
              go (d - 1) 0 v branch
snoc _ _ = error "TreeArray root should always be a Branch constructor"

-- | Lookup the element at the given index. Index starts from 0.
--  /O(d)/ where d is the depth of the tree. depth of tree is proportional
--  to log of number of elements to the base of `childWidth`.
getIndex :: (MonadIO m, Storable a) => Index -> TreeArray a -> m a
getIndex i (TreeArray count depth treeArray) = do
  if i < count then go depth i treeArray else invalidIndex "findAtOffset" i
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
       in do
            child <- BArray.getIndex branchArray childIndex
            go (d - 1) spillOver child

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
    step = flip snoc

invalidIndex :: String -> Int -> a
invalidIndex label i =
  error $ label ++ " : invalid array index " ++ show i
