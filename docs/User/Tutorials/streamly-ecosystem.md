# Streamly Ecosystem

The Streamly ecosystem is a collection of packages built on top of
Streamly, extending its core with higher-level functionality for
real-world programming.

For a combined view of the reference documentation across all ecosystem
packages, visit:
👉 [Streamly Module Listing](https://streamly.composewell.com/module-listing.html)

## streamly-process

**`streamly-process`** lets you use operating system (OS) commands in
Haskell programs as if they were native functions, by treating their
inputs and outputs as Haskell streams. This makes it easy to write
high-level Haskell scripts that perform tasks similar to shell pipelines
but with **C-like performance, strong safety guarantees, modularity, and
refactorability**.

For example, the shell command:

```bash
echo "hello world" | tr [a-z] [A-Z]
```

can be expressed in Haskell as:

<!--
Not exactly sure what the clean way is to import packages. We need to launch the
ghci session with the required packages.
-->
```
>>> :{
   Command.toBytes [str|echo "hello world"|] -- Stream IO Word8
 & Command.pipeBytes [str|tr [a-z] [A-Z]|]   -- Stream IO Word8
 & Stream.fold Stdio.write                   -- IO ()
:}
HELLO WORLD
```

👉 [streamly-process on GitHub](https://github.com/composewell/streamly-process)

## streamly-coreutils

**`streamly-coreutils`** reimplements GNU coreutils utilities as
composable Haskell functions—concurrent where possible. Examples include
`test`, `cp`, `ls`, `ln`, `mv`, `rm`, `touch`, `mkdir`, `pwd`, `cd`,
`stat`, `readlink`, `which`, `sleep`, and more.

This allows you to use familiar Unix-style tools directly within
streaming Haskell programs, combining the convenience of shell scripting
with the safety, modularity, and performance of Haskell.

👉 [streamly-coreutils on GitHub](https://github.com/composewell/streamly-coreutils)

## streamly-statistics

**`streamly-statistics`** offers functionality similar to the Haskell
`statistics` package, but with streaming APIs. Its unique strength is
support for **incremental statistical analysis on sliding windows of
data**, seamlessly integrated into streaming pipelines. This makes it
ideal for real-time analytics, monitoring systems, or any application
that processes continuous data streams.

👉 [streamly-statistics on GitHub](https://github.com/composewell/streamly-statistics)

## streamly-fsevents

**`streamly-fsevents`** provides a streaming interface to file system
events. It allows you to **watch files or directories for changes**—
such as creation, modification, deletion, or renaming—and consume those
events as Haskell streams. Internally, it uses efficient system
facilities like `inotify` on Linux for scalable event monitoring.

This makes it well-suited for building **real-time file watchers,
synchronization tools, live-reload servers, or monitoring pipelines**.

👉 [streamly-fsevents on GitHub](https://github.com/composewell/streamly-fsevents)

<!--
## streamly-lz4

Streaming APIs for lz4 compression and decompression.
-->

## Compatibility Packages

Streamly can interwork with other packages in the Haskell ecosystem
providing similar functionality. These packages enable the
interconversion.

### streamly-bytestring

Package for converting streamly `Array` type to the bytestring package's
`ByteString` type and vice-versa.

👉 [streamly-bytestring on GitHub](https://github.com/psibi/streamly-bytestring)

### streamly-text

Package for converting streamly `Array` type to the `text` package's
`Text` type and vice-versa.

👉 [streamly-text on GitHub](https://github.com/composewell/streamly-text)

### streamly-filepath

Package for converting streamly `Path` type to the `filepath` package's
`OsPath` type and vice-versa.

👉 [streamly-filepath on GitHub](https://github.com/composewell/streamly-filepath)
