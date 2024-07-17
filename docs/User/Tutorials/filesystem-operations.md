## File IO

THIS DOC IS NOT READY.

The following code snippets implement some common Unix command line utilities
using streamly.  You can compile these with `ghc -O2 -fspec-constr-recursive=16
-fmax-worker-args=16` and compare the performance with regular GNU coreutils
available on your system.  Though many of these are not most optimal solutions
to keep them short and elegant. Source file
[CoreUtilsHandle.hs](https://github.com/composewell/streamly-examples/blob/master/examples/CoreUtilsHandle.hs)
in the examples directory includes these examples.

``` haskell
module Main where

import qualified Streamly.Prelude as S
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Data.Array.Foreign as A
import qualified Streamly.FileSystem.Handle as FH
import qualified System.IO as FH

import Data.Char (ord)
import System.Environment (getArgs)
import System.IO (openFile, IOMode(..), stdout)

withArg f = do
    (name : _) <- getArgs
    src <- openFile name ReadMode
    f src

withArg2 f = do
    (sname : dname : _) <- getArgs
    src <- openFile sname ReadMode
    dst <- openFile dname WriteMode
    f src dst
```

### cat

``` haskell
cat = S.fold (FH.writeChunks stdout) . S.unfold FH.readChunks
main = withArg cat
```

### cp

``` haskell
cp src dst = S.fold (FH.writeChunks dst) $ S.unfold FH.readChunks src
main = withArg2 cp
```

### wc -l

``` haskell
wcl = S.length . S.splitOn (== 10) FL.drain . S.unfold FH.read
main = withArg wcl >>= print
```

### Average Line Length

``` haskell
avgll =
      S.fold avg
    . S.splitOn (== 10) FL.length
    . S.unfold FH.read

    where avg      = (/) <$> toDouble FL.sum <*> toDouble FL.length
          toDouble = fmap (fromIntegral :: Int -> Double)

main = withArg avgll >>= print
```

### Line Length Histogram

`classify` is not released yet, and is available in
`Streamly.Internal.Data.Fold`

``` haskell
llhisto =
      S.fold (FL.classify FL.length)
    . S.map bucket
    . S.splitOn (== 10) FL.length
    . S.unfold FH.read

    where
    bucket n = let i = n `mod` 10 in if i > 9 then (9,n) else (i,n)

main = withArg llhisto >>= print
```

