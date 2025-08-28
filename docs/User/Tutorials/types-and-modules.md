# Streamly: Types & Modules Cheat Sheet

This guide gives you a quick overview of Streamly’s core modules,
types, and their typical use cases. Use it as a reference map when
navigating the library.

## streamly-core package

![Streamly Core Modules](./streamly-core.svg)

## streamly package

![Streamly Modules](./streamly.svg)

## Streams

### Sources
- `Stream m a` — statically fused, composable source streams<br>
  **Module:** `Streamly.Data.Stream`
- `Unfold m a b` — statically fused streams, for nested fusion<br>
  **Module:** `Streamly.Data.Unfold`
- `StreamK m a` — CPS based source streams for dynamic composition<br>
  **Module:** `Streamly.Data.StreamK`

### Transformations

- `Scanl m a b` — statically fused scans, for composable stateful transformation<br>
  **Module:** `Streamly.Data.Scanl`

### Consumers

- `Fold m a b` — statically fused, composable stream consumers<br>
  **Module:** `Streamly.Data.Fold`
- `Parser a m b` — statically fused, composable parsers<br>
  **Module:** `Streamly.Data.Parser`
- `ParserK a m b` — CPS based parsers for dynamic composition<br>
  **Module:** `Streamly.Data.ParserK`

---

## Arrays

### Immutable

- `Array a => Unbox a` — immutable, unboxed (pinned/unpinned)<br>
  **Module:** `Streamly.Data.Array`
- `Array a` — unconstrained type<br>
  **Module:** `Streamly.Data.Array.Generic`

### Mutable

- `MutArray a => Unbox a` — mutable, unboxed (pinned/unpinned)<br>
  **Module:** `Streamly.Data.MutArray`
- `MutArray a` — unconstrained type<br>
  **Module:** `Streamly.Data.MutArray.Generic`
- `RingArray a => Unbox a` — unboxed, circular buffer (pinned/unpinned)<br>
  **Module:** `Streamly.Data.RingArray`

### Serialization

- `Unbox a` — type class for fixed length binary serialization<br>
  **Module:** `Streamly.Data.MutByteArray`
- `Serialize a` — type class for variable length binary serialization<br>
  **Module:** `Streamly.Data.MutByteArray`
- `MutByteArray` — raw mutable byte arrays<br>
  **Module:** `Streamly.Data.MutByteArray`

---

## Unicode Operations

- `Streamly.Unicode.Stream` — encode/decode streams of text
- `Streamly.Unicode.Parser` — parsers for Unicode chars/strings
- `Streamly.Unicode.String` — string interpolation utilities

---

## Concurrent Operations

High-level concurrent, time-based, and lifted operations.

- `Streamly.Data.Stream.Prelude` — concurrent operations for `Stream` type
- `Streamly.Data.Scanl.Prelude` — concurrent operations for `Scanl` type
- `Streamly.Data.Fold.Prelude` — concurrent operations for `Fold` type
- `Streamly.Data.Stream.MkType` — define custom monad/applicative stream types  

---

## File System

- `Streamly.Console.Stdio` — console (stdin/stdout/stderr) streams
- `Streamly.FileSystem.Handle` — handle-based I/O streams
- `Streamly.FileSystem.FileIO` — path-based file I/O streams
- `Streamly.FileSystem.DirIO` — directory read streams
- `Streamly.FileSystem.Path` — file path operations

---

## Network

- `Streamly.Network.Socket` — socket-level stream operations
- `Streamly.Network.Inet.TCP` — TCP accept streams/connect
