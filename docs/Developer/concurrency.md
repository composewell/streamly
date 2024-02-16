Junctions:

Specify a graph of computations by specifying the nodes/junctions connected
with edges which are directional pipes/scans. Specific junction defintions
can also help in breaking fusion to keep the fused network smaller.

Locking:

We can specify thread groups that are exclusive to each other i.e. run on the
same capability. Channels that utilize these threads need not take a lock and
can be much faster. The purpose of such channels could just be breaking fusion
in a serial program.

Exclusion domains:

In parConcatMap have two level queues. The first level would have multiple
streams, each belonging to one concatMap. Only one of the first level streams
can be scheduled at a time. While at the second level (inner streams of
concatMap) we can fan out as usual. We basically have multiple exclusion
domains where in each domain we can have multiple threads running in parallel,
but the domains themselves cannot run in parallel. A high level exclusion lock.
