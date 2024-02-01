# I know conduit, how do I use streamly?

## Modules

Relevant streamly modules for fused streams.  All these modules have CPS
versions as well, those are suffixed with K.

```
Streamly.Data.Stream (simple producers)
Streamly.Data.Scan (simple stateful transformations)
Streamly.Data.Pipe (more powerful stateful transformations)
Streamly.Data.Fold (simple consumers)
Streamly.Data.Parser (more powerful consumers)
```

Nested stream fusion is supported by:

```
Streamly.Data.Unfold (nested streams)
Streamly.Data.Refold (nested folds)
```

Streamly supports stream fusion providing performance comparable to C for all
types of use cases. Conduit uses CPS which is many times slower than fused
streams, comparable to the CPS versions of streamly modules.

## Types

There is a fundamental difference in the model that streamly uses and
the one that other streaming libraries use. Other libraries use the same
type for a producer stream and a consumer stream whereas streamly uses
different types. For example, conduit uses the following type, I guess
the same applies to Pipes and Streaming as well:

```haskell
data ConduitT i o m r
```

Streamly has different types for producers, pipes and consumers. The
first half of the story is the producer type:

```haskell
data Stream m o -- producer
```

The other half of the story is the consumer types. The output type `o`
above would become the input type of the consumers when you consume the
generated stream:

```haskell
data Fold m o r -- consumer
data Parser o m r -- more powerful fold
```

The Pipe type is not yet implemented properly, but this is how it would
look like:

```haskell
data Pipe m i o -- pipe
```

If you put all these together you can imagine getting something like
`ConduitT i o m r`.

In streamly you produce streams or consume streams. Producing streams
is just like the list operations, and by appending or concating
streams. Consuming is simply parsing. Traditionally parsers are
considered slow for this purpose so you have custom ways to consume
streams, but in streamly it is simple, just parse it. Parsers are fused
and fast, so you can use the same parsers everywhere.

## Linear Pipelines

Examples from [the conduit README](https://github.com/snoyberg/conduit/)
translated to streamly:

```haskell
    print $ runConduitPure $ yieldMany [1..10] .| sumC
    runConduitRes $ sourceFileBS "input.txt" .| sinkFile "output.txt"
    print $ runConduitPure $ yieldMany [1..10] .| mapC (+ 1) .| sinkList
```

```haskell
    print $ runIdentity $ Stream.fromList [1..10] & Stream.fold Fold.sum
    File.readChunks "input.txt" & File.writeChunks "output.txt"
    print $ runIdentity $ Stream.fromList [1..10] & fmap (+ 1) & Stream.toList
```

Streamly is better than lists:

```haskell
```

## Monadic Composition

These streaming libraries use the monad instance for two purposes, (1)
appending the producers, (2) splitting the consumers. Both of these are
possible with streamly and in a very similar fashion.

For appending, conduit would do something like this, example from the
conduit README:

```haskell
source :: Monad m => ConduitT i Int m ()
source = do
    yieldMany [1..10]
    yieldMany [11..20]
```

With streamly you do this without using the monad instance using the
`append` operation.

```haskell
source :: Stream m Int
source = 
    Stream.fromList [1..10] `Stream.append`
    Stream.fromList [11..20]
```

When consuming, the monad instance is used to split the consumers and
this is where the return type you mentioned originally comes into
picture. Consider this conduit example from the conduit README:

```haskell
sink :: Monad m => ConduitT Int o m (String, Int)
sink = do
    x <- takeC 5 .| mapC show .| foldC
    y <- sumC
    return (x, y)
```

Streamly also supports the return type but you find it in consumers, not
in producers. The `r` in `Fold m i r` type can be considered equivalent
to the stream return type in Conduit and Pipes. The above conduit
example directly translates to folds in streamly. Since folds do not
have a monad instance, I will use the Parser type which is essentially a
more powerful fold. Folds and parsers can be inter-converted:

```haskell
import Data.Function ((&))
import Streamly.Data.Parser (Parser)
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Parser as Parser
import qualified Streamly.Data.Stream as Stream

sink :: Monad m => Parser Int m (String, Int)
sink = do
    x <- Parser.fromFold (Fold.take 5 Fold.toList & fmap show)
    y <- Parser.fromFold (Fold.sum)
    return (x, y)

main :: IO ()
main = Stream.fold (Parser.toFold sink) (Stream.fromList [1..10]) >>= print
```

We can add a monad instance to Fold as well and then we won't need to
convert it to and from Parser. But that is just boilerplate difference.

Running the above example would give:

```
$ ./example
("[1,2,3,4,5]",40)
```

## ZipSource

```
Stream.zipWith
```

## ZipSink

```
Fold.Tee
Fold.teeWith
```

## ZipConduit

```
Stream.teeScanR
```
