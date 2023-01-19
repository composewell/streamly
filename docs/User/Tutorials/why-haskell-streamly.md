<!--
(c) 2022, Composewell Technologies.
SPDX-License-Identifer: BSD-3-Clause
-->

# Why Haskell Streamly?

A computer programmer is faced with different types of programming tasks. A
software project may involve programming tasks that have different
characteristics. Usually programmers are forced to choose different programming
tools for different tasks.

For example, you may be writing shell or python scripts to automate system
administration or deployment tasks, or for managing cloud deployments,
configuration. For such tasks you may not be worried about the performance but
you may be worried about correctness and how quickly you can achieve what you
want to do, and maintainability of the programs. For such tasks you would
usually reach out for python, shell scripting or other such high level
scripting languages.

On the other hand you may be writing a program that require very high
performance and optimal use of resources so that the task can be finished
faster and requires less resources to save cost. High level scripting languages
(e.g. python or shell) may be unsuitable for such tasks because they could be
10x-100x slower compared to lower level languages like `Rust`, `C`, `C++` or
`Java`.

If you would like to use the same language and tools for all your
programming tasks, be it a quick script or a high performance system,
or if you do not want to build different set of teams for programming
tasks of different nature then Haskell Streamly is just the programming
framework for you.

Whether you are writing scripts to manage your systems, or to automate mundane
tasks like organizing photos or files on your computer, or to write tests for
your software systems, or crawling the network to glean some information, or
writing a quick network server, or writing algorithmic trading systems, or
doing high performance intensive data processing, Haskell Streamly is just the
right tool for you.

Haskell Streamly is a very high level programming framework, like Python or
Shell or Sed or AWK, in fact much higher level than those, but at the same time
provides high performance like C or Rust. The ability to compose programs using
high level combinators in Haskell is unmatched. Not just efficient programs but
you can write programs that scale on multiple CPUs with the same ease. Streamly
uses the full power of Haskell to make concurrency composable in declarative
style, so that you can use concurrent programming fearlessly with the same ease
as non-concurrent programming.

Although Haskell is a compiled programming language it comes with an
interpreter (GHCi) which allows you to run Haskell code interactively like an
interpreted language. Haskell has a strong static type system, yet in most
cases you do not have to annotate the types because it infers the types
automatically.

You can keep systems written with Haskell Streamly extremely modular using
reusable building blocks, modularity is the fundamental characteristic and
a design goal of Streamly. At the same time Haskell allows you to do fearless
refactoring of your program to continuously evolve it without the fear of
introducing more bugs or breaking it every time you touch it.

The type safety and immutability by default in Haskell avoids a lot of bugs
that would otherwise find their way into your programs if you are using less
safe languages. You focus on your business logic rather than the correctness
issues imposed by the programming paradigm.

Haskell is extensible, you can integrate C programs with Haskell very easily.
Programs written in Haskell work on `macOS`, `Windows`, `Linux` or any Unix
like system and even on the browser using the Java Script and Web assembly
backends.

Haskell Streamly is a standard library for Haskell and supplements the Haskell
base package. It comes equipped with basic data structures and functions like
Streams, Arrays, Folds, Parsers, file system facilities like directory IO, file
IO, Unicode text processing, network processing using low level sockets or high
level operations. Concurrent programming facilities or time domain programming
facilities.

The collection of high level building blocks provided by the Streamly ecosystem
allow you to write a variety of programs quickly. You can turn your shell or
python scripts into safe, highly modular and high performance programs using
`streamly-coreutils`. Moreover, you do not have to depend on any native OS
packages installed on the system. Or if you would like to use native
executables in your programs you can use `streamly-process` to invoke the
executables and connect the data streams generated like pipes in a seamless
manner. Or if you would like to use the shell to invoke and compose the
executables and integrate it seamless in your program you can do that too using
`streamly-shell`. If you would like to use statistical computation in your
programs try out `streamly-statistics`. Streamly ecosystem is evolving and
there are more useful packages in the works. In addition, you have thousands of
Haskell packages that are available on Hackage.
