# Compilation Options

Recommended GHC options are: 

  `-O2 -fspec-constr-recursive=10 -fmax-worker-args=16`

At the very least `-O` compilation option is required. In some cases, the
program may exhibit memory hog with default optimization options.  For example,
the following program, if compiled without an optimization option, is known to
hog memory:

```
main = S.drain $ S.concatMap S.fromList $ S.repeat []
```

The `-fspec-constr-recursive=10` option may not be necessary in most cases but
may help boost performance in some cases.

# Multi-core Parallelism

Concurrency without a threaded runtime may be a bit more efficient. Do not use
threaded runtime unless you really need multi-core parallelism. To get
multi-core parallelism use the following GHC options:

  `-threaded -with-rtsopts "-N"`

# Compiler Versions

GHC 8.2.2 may hog memory and hang when building certain application using
streamly (particularly the benchmark programs in the streamly package).
Therefore we recommend avoiding using the GHC version 8.2.x.
