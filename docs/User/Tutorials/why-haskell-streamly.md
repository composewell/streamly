<!--
(c) 2022, Composewell Technologies.
SPDX-License-Identifer: BSD-3-Clause
-->

# Why Haskell and Streamly?

## Why Haskell?

Haskell provides a rock-solid foundation for building reliable,
maintainable, and efficient systems. It combines the rigor of a strongly
typed, purely functional language with the practicality of modern
tooling.

### Interactive Development (GHCi)

Haskell is compiled for performance, but it also ships with an
interpreter (GHCi). This gives you the convenience of interactive
exploration—like Python or Ruby—while still compiling to fast native
code when needed. Types are inferred automatically, so you often don’t
need to write them explicitly, but they are always there to guarantee
safety.

### Fearless Refactoring

Large systems inevitably evolve, and in many languages refactoring
introduces new bugs. In Haskell, immutability and a strong static type
system keep programs consistent, while the compiler catches errors early.
This makes refactoring safe and reliable—a quality that experienced
Haskell developers consistently vouch for in production systems.

### Correctness by Design

Many common bugs in other languages—null pointer exceptions, data
races, accidental mutation—simply don’t exist in Haskell. This frees
you to focus on business logic instead of defensive programming.

### Portability and Interoperability

Haskell is portable across Linux, macOS, Windows, and even the browser
(via JavaScript or WebAssembly). It can seamlessly interoperate with C,
Python, and many other languages, letting you reuse existing code and
libraries.

---

## Why Streamly?

Streamly builds on Haskell’s strengths to provide a single framework
for everything from quick automation scripts to high-performance
servers. It combines **Python’s ease of use, C’s performance,
Rust’s safety, and Go’s concurrency model**—all in one framework,
and goes even further.

### Performance Matching Low-Level Languages

High-level scripting languages like Python or Bash can be **10x–100x
slower** than optimized low-level languages such as C, Rust, or
Java. For tasks where efficiency matters—data pipelines, servers,
or numerical processing—this overhead is unacceptable.

Streamly leverages GHC’s advanced optimizations and stream fusion to
deliver performance on par with C, without sacrificing expressiveness.
It gives you the best of both worlds: the productivity of high-level
scripting languages like Python or Bash, with the performance of
low-level languages like C and Rust.

### More Expressive than Scripting Languages

For scripting, system management, data wrangling, or even spinning up
quick servers, Streamly offers high-level combinators that let you
compose programs declaratively. Its design philosophy emphasizes
**modularity**: a hierarchy of composable building blocks, where
higher-level constructs are built from lower-level ones. This degree
of modularity is possible in Haskell due to purity, which enables
the compiler to reason about and combine components safely, creating
higher-level functionality without compromising performance. The result
is much more power and expressiveness than Python or shell scripting—
while still compiling to efficient native code.

### Fearless Concurrency

Streamly is designed for concurrency, allowing you to write concurrent
code in a declarative style. Haskell’s purity lends itself
particularly well to concurrency, and Streamly leverages this unique
feature in Haskell. Its high-level abstractions free you from low-level
concerns such as threads, locks, and synchronization. There is little
distinction between concurrent and non-concurrent code—if your program
follows the streaming paradigm, it can be made concurrent effortlessly.

### Standard Library for Real-World Needs

The streaming model is the truest form of functional programming,
delivering the high composability that functional programming promises.
Streamly extends Haskell’s base library with unified, streaming-capable
APIs for everyday programming:

- Streams, folds, and parsers for data processing
- Arrays for random-access storage with streaming APIs
- Binary serialization and deserialization
- Streaming file and directory I/O
- Streaming Unicode text processing
- Streaming network APIs
- Time-based streaming APIs for FRP
- Concurrency built into all APIs

With these tools, you can move seamlessly from quick prototypes to
production-grade systems—without ever switching languages.

### Streamly Ecosystem

Beyond the core library, Streamly provides an ecosystem of packages:

- **`streamly-process`**: Launch and compose executables, connect their
  input and output streams, run shell commands, and integrate them
  seamlessly into Haskell streaming pipelines.
- **`streamly-coreutils`**: Replace shell or Python scripts with safe,
  modular, high-performance Haskell equivalents.
- **`streamly-statistics`**: Perform incremental statistical analysis on
  sliding windows of data within streaming pipelines.
- **`streamly-fsevents`**: Watch files or directories for file system
  events (inotify) and generate event streams.

More packages are continually evolving. Combined with the thousands of
Haskell libraries on [Hackage](https://hackage.haskell.org), Streamly
gives you a complete toolkit for building powerful applications.

---

## One Language for Everything

Modern software projects demand many different kinds of programs:  

- Quick scripts for automation  
- High-performance data processing  
- Concurrent or parallel applications  
- Network servers and distributed systems  

Traditionally, this forces programmers to juggle different tools: Python
or shell for scripting, C/C++/Rust for performance, Java/Go for servers,
and special frameworks for concurrency. This fragmentation leads to
duplicated effort, larger teams, and higher maintenance costs.

**Haskell Streamly provides a single framework for all of these
tasks.** You can write concise, high-level programs that are safe,
scalable, and easy to reason about—yet still achieve performance
competitive with C.

Haskell gives you correctness, safety, and interactive development.
Streamly adds performance, concurrency, modularity, and practical
libraries. Together, they remove the need to juggle multiple languages
for different tasks.
