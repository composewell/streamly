# Types and Modules at a Glance

## Streams

The following table lists the modules and types for monadic stream producers
(streams), stream transformers (scans), stream consumers (folds and parsers).

| Module                | Type          | Description                                                |
|-----------------------|---------------|------------------------------------------------------------|
| Streamly.Data.Stream  | Stream m a    | Streams using stream fusion, for static composition        |
| Streamly.Data.Unfold  | Unfold m a b  | Streams using nested stream fusion, for static composition |
| Streamly.Data.StreamK | StreamK m a   | Streams using CPS, for dynamic composition                 |
| Streamly.Data.Scan    | Scan m a b    | Scans using stream fusion, for static composition          |
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
| Streamly.Data.MutByteArray               | Unbox a      | Fixed length data serialization                |
| Streamly.Data.MutByteArray               | Serialize a  | Variable length data serialization             |
| Streamly.Internal.Data.RingArray         | RingArray a  | Unboxed ring buffer                            |
| Streamly.Internal.Data.RingArray.Generic | RingArray a  | Boxed ring buffer                              |

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
| Streamly.Data.Stream.Prelude | Concurrent, time and lifted functions for streams   |
| Streamly.Data.Stream.MkType  | Make custom monad and applicative types for streams |
| Streamly.Data.Fold.Prelude   | Concurrent, time and lifted functions for folds     |

## File System Operations

File system path representation:

| Module                            | Type               | Description                                                    |
|-----------------------------------|--------------------|----------------------------------------------------------------|
| Streamly.FileSystem.Path          | Path               | Untyped path for the current OS type                           |
| Streamly.FileSystem.Path.Node     | File a, Dir a      | Typed path with File, Dir distinction                          |
| Streamly.FileSystem.Path.Seg      | Rooted a, Branch a | Typed path with rooted and branch distinction                  |
| Streamly.FileSystem.Path.SegNode  |                    | Fully typed path with file, dir, rooted and branch distinction |
| Streamly.FileSystem.PosixPath.*   |                    | Posix specific path modules                                    |
| Streamly.FileSystem.WindowsPath.* |                    | Windows specific path modules                                  |

Console and file system operations:

| Module                                     | Description                                      |
|--------------------------------------------|--------------------------------------------------|
| Streamly.Console.Stdio                     | Console standard input, output, error operations |
| Streamly.FileSystem.Handle                 | Handle based read/write operations               |
| Streamly.FileSystem.FileIO                 | File path based read, write operations           |
| Streamly.FileSystem.DirIO                  | Directory read operations                        |
| Streamly.Internal.FileSystem.Event         | File system event notification streams           |
| Streamly.Internal.FileSystem.Event.Windows | Event notification streams for Windows           |
| Streamly.Internal.FileSystem.Event.Linux   | Event notification streams for Linux             |
| Streamly.Internal.FileSystem.Event.Darwin  | Event notification streams for macOS             |

## Network Operations

| Module                       | Description                                   |
|------------------------------|-----------------------------------------------|
| Streamly.Network.Socket      | Socket level stream operations                |
| Streamly.Network.Inet.TCP    | TCP level stream operations (accept, connect) |
