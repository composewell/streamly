## Splitting streamly into smaller packages

If we use streamly- prefix in package naming, it will be easier to find all
packages on hackage or otherwise. The module prefixes are largely the same as
package name with "-" replaced by ".". To keep the imports shorter the Streamly
prefix can be dropped. or the Stream infix can be dropped. Stream can also be
repalced by "Machine" as all these are nothing but state machines. If we use
that then scan naming could be simpler e.g. Machine.Scan, Machine.Moore,
Machine.Mealy.

streamly-base:
    Streamly.Data.Tuple.Strict
    Streamly.Data.Maybe.Strict
    Streamly.Data.Either.Strict
    ...

streamly-streams-fused:
    Streamly.Data.Unfold
    Streamly.Data.Stream
    Streamly.Data.Scan
    Streamly.Data.Scanl
    Streamly.Data.Fold
    Streamly.Data.Parser
    Streamly.Data.Pipe

streamly-streams-cps
    Streamly.Data.UnfoldK
    Streamly.Data.StreamK
    Streamly.Data.ScanK
    Streamly.Data.FoldK
    Streamly.Data.ParserK
    Streamly.Data.PipeK

Should we combine the above two packages into a single "streamly-streams"?

streamly-arrays:
    Streamly.Data.Array
    Streamly.Data.Array.Generic
    Streamly.Data.MutArray
    Streamly.Data.MutArray.Generic
    Streamly.Data.MutByteArray
    Streamly.Data.RingArray

We need a package for combinators that use streams and arrays both but do not
belong to arrays package. Or it could be part of streamly-core.

streamly-unicode:
    Streamly.Unicode.Parser
    Streamly.Unicode.Stream
    Streamly.Unicode.String

streamly-filesystem:
    Streamly.FileSystem.Dir
    Streamly.FileSystem.File
    Streamly.FileSystem.Handle
    Streamly.FileSystem.Path

streamly-network:
    Streamly.Network.Inet.TCP
    Streamly.Network.Socket

streamly-serial-types, newtype wrappers:
    Streamly.Data.MkType
    Streamly.Data.List
    Streamly.Data.ListK
    Streamly.Data.Serial
    Streamly.Data.Nested
    Streamly.Data.FairNested
    Streamly.Data.Interleaved

Concurrent means concurrent data structures, so Data is implicit.

streamly-concurrent:
    Streamly.Concurrent.Stream
    Streamly.Concurrent.Scan
    Streamly.Concurrent.Scanl
    Streamly.Concurrent.Fold

Should we use DFS and BFS namespaces?

streamly-concurrent-types, newtype wrappers:
    Streamly.Concurrent.Async
    Streamly.Concurrent.WAsync
    Streamly.Concurrent.Ahead
    Streamly.Concurrent.WAhead
    Streamly.Concurrent.Parallel

streamly-concurrent-arrays:
    Streamly.Concurrent.Array
    Streamly.Concurrent.Array.Generic
    Streamly.Concurrent.MutArray
    Streamly.Concurrent.MutArray.Generic
