# Asyncly

[![Build Status](https://travis-ci.org/harendra-kumar/asyncly.svg?branch=master)](https://travis-ci.org/harendra-kumar/asyncly)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/h2bcgw7xa2jxe222?svg=true)](https://ci.appveyor.com/project/harendra-kumar/asyncly)
[![Coverage Status](https://coveralls.io/repos/harendra-kumar/asyncly/badge.svg?branch=master&service=github)](https://coveralls.io/github/harendra-kumar/asyncly?branch=master)

Asyncly is best described as a superset of list transformer (`ListT`) or logic
programming monad (`LogicT`) with additional support for concurrent operation.
Monadic streams of data can be composed using serial or parallel compositions
with or without fair interleaving, enabling a high level composition of
concurrent tasks without requiring knowledge of low level concurrency
primitives. The programmer just expresses whether a task can run in parallel
with another. Threads, synchronization and concurrency rate control is handled
automatically.

This library was originally inspired by the `transient` package authored by
Alberto G. Corona.
