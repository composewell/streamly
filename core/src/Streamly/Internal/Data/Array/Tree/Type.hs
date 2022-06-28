module Streamly.Internal.Data.Array.Tree.Type where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Streamly.Internal.Data.Array as BArray
import qualified Streamly.Internal.Data.Array.Foreign.Mut as UArray
import qualified Streamly.Internal.Data.Fold as F
import Streamly.Internal.Data.Unboxed (Storable)

branchWidth :: Int
branchWidth = 8

leafWidth :: Int
leafWidth = 1024

type Count = Int

type Depth = Int

type Index = Int

-- A tree of unboxed, mutable arrays with a pre-defined
-- fixed width each
data TreeArray a = TreeArray Count Depth (TreeArrayInternal a)

data TreeArrayInternal a = Branch (BArray.Array (TreeArrayInternal a)) | Leaf (UArray.Array a)

newTreeArray :: (MonadIO m, Storable a) => m (TreeArray a)
newTreeArray = do
  leafArray <- UArray.newArray leafWidth
  return $ TreeArray 0 0 $ Leaf leafArray

-- TODO: Handle case where leaf array becomes full
append :: (MonadIO m, Storable a) => a -> TreeArray a -> m ()
append v (TreeArray count depth treeArray) =
  go depth (count + 1) v treeArray
  where
    go :: (MonadIO m, Storable a) => Depth -> Index -> a -> TreeArrayInternal a -> m ()
    go _ i v (Leaf leafArray) = UArray.putIndex i v leafArray
    go d i v (Branch branchArray) =
      let -- branch will always have a depth greater than 0
          fullChildTree = (d - 1) ^ branchWidth * leafWidth
          -- index of next branch to visit
          childIndex = i `quot` fullChildTree
          -- index spilled over from skipping children
          spillOver = i `rem` fullChildTree
       in go (d - 1) spillOver v $ BArray.getIndexUnsafe branchArray childIndex

getIndex :: (MonadIO m, Storable a) => Index -> TreeArray a -> m a
getIndex i (TreeArray count depth treeArray) = do
  if i < count then go depth i treeArray else invalidIndex "findAtOffset" i
  where
    go :: (MonadIO m, Storable a) => Depth -> Index -> TreeArrayInternal a -> m a
    go _ i (Leaf leafArray) = UArray.getIndex i leafArray
    go d i (Branch branchArray) =
      let -- branch will always have a depth greater than 0
          fullChildTree = (d - 1) ^ branchWidth * leafWidth
          -- index of next branch to visit
          childIndex = i `quot` fullChildTree
          -- index spilled over from skipping children
          spillOver = i `rem` fullChildTree
       in go (d - 1) spillOver $ BArray.getIndexUnsafe branchArray childIndex

invalidIndex :: String -> Int -> a
invalidIndex label i =
  error $ label ++ " : invalid array index " ++ show i

deleteAtOffset :: Int -> Int -> TreeArray a -> TreeArray a
deleteAtOffset = undefined
