# Roadmap

Some of these items may be present in the issues as well, but this file
lists larger areas, experimental areas and organizes items by areas. We can
also use the areas listed here as labels (aspect:topic) on issues.

## Scheduling

* Free applicative and Free Alternative, batched Alternative (push
  scheduling instead of pull scheduling)
* N-ary operations e.g. real balanced interleave
* Scheduling: Coalescing of tasks based on programmer defined criteria
    * Batching based on the target host
    * Batching/chunking for parallel/distributed execution
    * Batching iterations

## Concurerncy

* Controlled parallelism:
    * Control based on the level in the tree
    * Control based on the CPU/IO utilization based pacing
    * Utilize non-blocking IO
* Performance: Measure lock/CAS contention overhead, an option to
  dequeue work and run in batches instead of one at a time to reduce lock
  contention?
* Cross thread recursion

## Persistence

* Pause and resume using something like
  [monad-recorder](https://hackage.haskell.org/package/monad-recorder)
* Save internal buffered state during a pause?

## Testing

* improve coverage
