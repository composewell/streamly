# Types and Modules at a Glance

## Streams

The following table lists the modules and types for monadic stream producers
(streams), stream transformers (scans), stream consumers (folds and parsers).

| Module                | Type          | Description                                                |
|-----------------------|---------------|------------------------------------------------------------|
| Streamly.Data.Stream  | Stream m a    | Streams using stream fusion, for static composition        |
| Streamly.Data.Unfold  | Unfold m a b  | Streams using nested stream fusion, for static composition |
| Streamly.Data.StreamK | StreamK m a   | Streams using CPS, for dynamic composition                 |
| Streamly.Data.Scanl   | Scanl m a b   | Scans using stream fusion, for static composition          |
| Streamly.Data.Fold    | Fold m a b    | Folds using stream fusion, for static composition          |
| Streamly.Data.Parser  | Parser a m b  | Parsers using stream fusion, for static composition        |
| Streamly.Data.ParserK | ParserK a m b | Parsers using CPS, for dynamic composition                 |

## Arrays

| Module                                   | Type         | Description                                    |
|------------------------------------------|--------------|------------------------------------------------|
| Streamly.Data.Array                      | Array a      | Immutable, unboxed, pinned and unpinned arrays |
| Streamly.Data.MutArray                   | MutArray a   | Mutable, unboxed, pinned and unpinned arrays   |
| Streamly.Data.Array.Generic              | Array a      | Immutable, boxed arrays                        |
| Streamly.Data.MutArray.Generic           | MutArray a   | Mutable, boxed arrays                          |
| Streamly.Data.MutByteArray               | MutByteArray | Mutable byte arrays                            |
|                                          | Unbox a      | Fixed length data serialization                |
|                                          | Serialize a  | Variable length data serialization             |
| Streamly.Data.RingArray                  | RingArray a  | Unboxed ring buffer                            |
| Streamly.Data.RingArray.Generic          | RingArray a  | Boxed ring buffer                              |

## Unicode Operations

| Module                  | Description                                |
|-------------------------|--------------------------------------------|
| Streamly.Unicode.Stream | Unicode stream operations, encode, decode  |
| Streamly.Unicode.Parser | Parsers for Unicode characters and strings |
| Streamly.Unicode.String | String interpolation                       |

## Concurrency Operations

High level stream operations including concurrent, time and lifted functions:

| Module                       | Description                                         |
|------------------------------|-----------------------------------------------------|
| Streamly.Data.Stream.Prelude | Concurrent stream sources, time related operations  |
| Streamly.Data.Scanl.Prelude  | Concurrent, composable stream pipes                 |
| Streamly.Data.Fold.Prelude   | Concurrent, composable stream consumers             |
| Streamly.Data.Stream.MkType  | Make custom monad and applicative types for streams |

## File System Operations

Console and file system operations:

| Module                     | Description                                      |
|----------------------------|--------------------------------------------------|
| Streamly.Console.Stdio     | Console standard input, output, error operations |
| Streamly.FileSystem.Handle | Handle based read/write operations               |
| Streamly.FileSystem.FileIO | File path based read, write operations           |
| Streamly.FileSystem.DirIO  | Directory read operations                        |
| Streamly.FileSystem.Path   | File path representation and operations          |

## Network Operations

| Module                       | Description                                   |
|------------------------------|-----------------------------------------------|
| Streamly.Network.Socket      | Socket level stream operations                |
| Streamly.Network.Inet.TCP    | TCP level stream operations (accept, connect) |
