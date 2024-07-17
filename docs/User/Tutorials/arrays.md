# Arrays

THIS DOC IS NOT READY.

<!-- TODO add interworking examples -->

The `Streamly.Data.Array.Foreign` module provides immutable arrays.  Arrays are the
computing duals of streams. Streams are good at sequential access and immutable
transformations of in-transit data whereas arrays are good at random access and
in-place transformations of buffered data. Unlike streams which are potentially
infinite, arrays are necessarily finite. Arrays can be used as an efficient
interface between streams and external storage systems like memory, files and
network. Streams and arrays complete each other to provide a general purpose
computing system. The design of streamly as a general purpose computing
framework is centered around these two fundamental aspects of computing and
storage.

`Streamly.Data.Array.Foreign` uses pinned memory outside GC and therefore avoid any
GC overhead for the storage in arrays. Streamly allows efficient
transformations over arrays using streams. It uses arrays to transfer data to
and from the operating system and to store data in memory.
