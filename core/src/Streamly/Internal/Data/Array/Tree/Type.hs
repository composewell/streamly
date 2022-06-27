module Streamly.Internal.Data.Array.Tree.Type where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Streamly.Internal.Data.Array.Foreign.Mut as U
import qualified Streamly.Internal.Data.Array as B
import qualified Streamly.Internal.Data.Fold as F

treeWidth :: Int
treeWidth = 8

-- A tree of unboxed, mutable arrays with a pre-defined
-- fixed width each
data TreeArray a = Branch {offset :: U.Array Int, children :: B.Array (Maybe (TreeArray a))} | Leaf (U.Array a)

newTreeArray :: MonadIO m => m (TreeArray a)
newTreeArray = do
    offset <- U.newArray treeWidth
    children <- F.finish $ B.writeN treeWidth
    return $ Branch offset children

insertAtOffset :: a -> Int -> TreeArray a -> TreeArray a
insertAtOffset = undefined

findAtOffset :: TreeArray a -> Int -> Maybe a
findAtOffset = undefined

deleteAtOffset :: Int -> Int -> TreeArray a -> TreeArray a
deleteAtOffset = undefined
