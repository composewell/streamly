{-# LANGUAGE ScopedTypeVariables #-}
import Streamly
import Streamly.FileIO
import Data.Word (Word8)

import qualified Streamly.Foldl as FL
import qualified Streamly.Array as A
import qualified Streamly.Prelude as S
import System.IO (IOMode(..), openFile)

-- import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
    handle <- openFile "in.txt" ReadMode
    {-
    -- This is the equivalent of bytestring's length operation
    -- let s = A.readHandleChunksOf defaultChunkSize handle
    -- S.sum (S.map A.length s) >>= print

    -- This is the equivalent of bytestring's last operation
    let s = A.readHandleChunksOf defaultChunkSize handle
    lc <- S.last s
    let l = case lc of
                Nothing -> Nothing
                Just c -> A.last c
    print l
    -}

    {-
    -- This is the equivalent of bytestring's foldl' operation
    let s = A.readHandleChunksOf defaultChunkSize handle
    let f _ = Just
    x <- S.foldl' (\_ arr -> A.foldl' f Nothing arr) Nothing s
    print x
    -}

    {-
    -- Direct stream operations
    let s = fromHandle handle
    -- x <- S.head s
    -- x <- S.length s
    -- x <- S.last s
    -- x <- S.foldl' (\_ a -> Just a) Nothing s
    x <- S.foldl' (+) 0 s
    print x
    -}

    -- out <- openFile "out.txt" WriteMode
    -- Writing chunks, equivalent to bytestring
    -- let s = A.readHandleChunksOf defaultChunkSize handle
    -- A.concatToHandle out s

    -- turning into a stream and then back to chunks and then writing
    -- toHandle out (fromHandle handle)
    -- toHandleChunksOf defaultChunkSize out (fromHandle handle)
    {-
    x <- S.length $
         S.foldGroupsOf (FL.toArrayN defaultChunkSize)
                        defaultChunkSize
                        (fromHandle handle)
                        -}
    -- x <- S.length $ S.arrayGroupsOf defaultChunkSize (fromHandle handle)
    -- XXX need a test where we allocate 10 MB chunk and the stream is much
    -- bigger than that to check touchForeignPtr.
    x <- FL.foldl (FL.toArrayN 1000000004) (fromHandle handle)
    print (A.length x)

    {-
    bytes <- BS.readFile "in.txt"
    -- let x = BS.head bytes
    -- let x = BS.length bytes
    -- let x = BS.last bytes
    let x = BS.foldl' (\_ a -> Just a) Nothing bytes
    print x
    -- BS.writeFile "out.txt" bytes
    -}
