# Directory traversal API design notes

## Filesystem functionality modules

The DirIO module mainly provides "readdir" functionality. File stat
functionality is coupled to readdir because we may return file stats along
with filepaths, or may provide functionality to filter based on stats.

The FileTest module in streamly-coreutils package provides the file stat read
operations, we may have to bring that here if some coupling with readdir is
needed.

FileIO module provides regular file create operation.

## Traversal vs Output control

There are two dimensions to recursive directory reading APIs, traversal and
filtering. Traversal controls how we recurse further down in a directory
tree; we may need to allow certain traversals and prune others. On the
other hand, output filtering controls what we want to return in the output
stream e.g. we can filter based on type (dir, file, symlink) or based on
the filename etc. These two dimensions can be completely independent of
each other.

## Returning metadata or using config options

A design point is whether to return metadata for files and let consumers do
filtering based on the metadata, or to use config options/predicates inside
readdir itself and return only transformed results as output. Using config
predicates keeps the API and stream representation cleaner but is not as
powerful as emitting the metadata.

If we want to expose metadata, we may need different APIs: without
metadata, with fast metadata, and with full metadata. In each case the fold
input would be different.

* readMinimal: read only path names, no metadata
* readStandard: read path and minimal metadata
* readFull: read full metadata

Full metadata can always be read by mapping a stat call on a stream of paths
rather than integrating it into readdir itself. The advantage of doing it in
readdir is in the case when we need to do a stat in readdir anyway e.g. when
the file system does not provide the type, or when followSymlinks is "on".

## Passing a scan to readdir or using config options

Even if we want filtering inside readdir, there are two options:

* pass config predicates/options
* pass a scan/fold which can directly consume metadata and return output

Passing a scan is similar to exposing metadata, but only to a scan that is
passed directly to readdir.

By passing a scan we can process output right at the source and produce a
cooked output. Otherwise we may have to produce a stream of intermediate
structures which may have more per-item overhead and that overhead may not
get eliminated by fusion. For example, a fold can directly write the
CString from readdir to the output buffer whereas if we output a Path then
we incur overhead of an intermediate structure.

However, such a fold must not be allowed to retain the CString pointer from
readdir because that memory is owned internally and may be released after
readdir returns. This can be enforced by fixing the output type to Path,
which copies data into managed memory (e.g. MutByteArray-backed Path).

Another drawback of passing a fold is that it exposes low-level details
like CString and Dirent from readdir for a relatively small performance
gain.

If we want only filtering, a cleaner solution is to use configuration
predicates/options in readdir and keep the emitted stream representation
simple. However, a scan can be more powerful e.g. if we want to count how
many files of each type are present in a directory we can do that with a
scan or by emitting the metadata.

## Traversal State vs Output

There are two ways to handle traversal. One approach is to segregate the output
into two parts: one for terminal nodes and another for directory nodes to be
traversed further. The final output then combines these two parts to produce
the complete result. The other approach is to output all child nodes in the
output part, while also placing the nodes to be traversed further in the
traversal part. In this case, the output is duplicated, which could become
significant if there are a large number of directories.

However, there are a couple of problems with the segregation approach. During
concurrent processing, if a worker finds that it is busy generating a large
amount of output from one directory, it may return the remaining work so that
other workers can pick it up. As a result, the same directories can appear
multiple times in the traversal part. If we also include them in the output,
this will lead to duplicate entries in the final output. Additionally, for
concurrent evaluation we may divide the traversal into arbitrarily small parts,
which can nullify the chunking benefit because we ultimately have to recombine
all these small parts into larger chunks again.

Therefore, it may be better to place everything in the output from the
beginning and keep the traversal output completely independent of the normal
output.

The readdir APIs conceptually produce two independent outputs:

* traversal state (directories to recurse into)
* observable output

The output is essentially two streams via an Either type:

* Left  values are traversal state (directories selected for recursion)
* Right values are observable output (files and dirs both)

The Left channel controls traversal and can have independent filters in
readdir options. The Right channel controls emitted output and is filtered
independently.

This separation keeps traversal control orthogonal to output filtering and
allows the low-level scanner to emit both efficiently in a single pass.

## Traversal Control

There are two levels of traversal control possible.

One level depends on state/information naturally available only to readdir,
e.g. follow-symlink behavior because file type/stat information is available
during scanning.

Another level is naturally handled outside readdir by the recursion control
combinator (e.g. concatIterate), such as filtering based on:

* traversal depth
* directory names
* path patterns

## Output Control

Again there are two levels.

Control based on information naturally available only to readdir (e.g. file
type/stat data) can be implemented via config predicates/options to
readdir.

Control based purely on path information (e.g. filename, extension, regex,
glob patterns etc.) can be implemented by filtering the stream returned by
readdir.

If the stream remains lazy with good fusion, we should not lose significant
performance with this modular design.

Chunking of traversal output and normal output can also be implemented
outside readdir over the stream. We should compare performance of chunking
inside readdir vs outside to evaluate whether a monolithic design provides
measurable gains.

## Stat based filtering for traversal and output streams

In case of Windows along with file type the API provides other attributes
like times, size, readonly which are generally available only by using stat
on Posix. We can use predicates to filter based on these as well. In case of
Posix we will have to dynamically decide whether to stat or not to use these
predicates. One effect of that is if someone uses these predicates the
performance will suddenly drop, we need to document that.

Another point is any of these can be used for traversal filtering of
directories as well. So do we need separate option arguments for traversal
and output control or just different names for the filters? Such a filter
can be just one composed function so we do not need many options, it will be
just one filter for traversal and one for output.

## Symlink Traversal Mechanism

This is part of traversal configuration controlled by followSymlinks option.

Currently, for the traversal root we always follow symlinks, there is no
option to change that. We control the follow symlinks behavior by
controlling the traversal stream contents.

For recursive traversal, we emit directories in the traversal stream,
therefore, we need to know the type of each directory entry, if it is a dir
then it is emitted in the traversal stream otherwise in the regular output
stream. When the type of directory entries is not available we use an
explicit stat call. When following symlinks, even if we know the types of
directory entries we need to determine the type of the link destination
using an explicit stat call and classify it as dir or not dir.

Alternatively, instead of classifying the dirents using stat, we can emit
files that potentially may be dirs in the traversal output, the recursion
handler will then feed those to readdir again, at that time we can deal with
ENOTDIR when doing an opendir. We can just ignore that error if it is not a
dir. This way we do not need to do an additional stat for symlinks, the stat
happens when we are trying to read it as dir, therefore, we can save a call
in case the symlink is a dir. For using this strategy, we will have to
translate the followSymlinks option to O_NOFOLLOW open flag. Using that
strategy we can classify symlinks or DT_UNKNOWN as potential dirs. Not sure
what potential problems can arise if we follow this.

## Symlink Resolution

In general, instead of fully resolving all indirections in a symlink at once
we can resolve it one step at a time utilizing the recursion in the
traversal handler. For example, we can find the link destination and emit it
in the traversal stream as a potential recursion candidate. This could be
useful in a general graph traversal, however, in case of file system
symlinks resolving them up to a non-symlink destination and then determining
whether it is a dir or not is more efficient.

## Handling symlink errors

When resolving a symlink we may encounter errors only if a directory entry
is a symlink. If the directory entry is not a symlink then stat on it will
usually succeed, we know it exists and it has permissions because the path
resolution goes through this directory which we just read, it will not give
ENOENT unless the file was modified, deleted or recreated after we read the
dirent.

For symlink resolution we may encounter, missing file, permission issues,
cycles. For that we need options to control the behavior.

## Errors during opendir

The same errors that apply to symlink resolution also apply to opendir for
the directory we are reading. These errors are especially important to
control during recursive traversal, probably not very useful for reading a
single dir. If a directory in the traversal stream went away or does not
have permission to read we may want to ignore those errors for recursive
traversal. This handling may be separate from the similar error handling for
symlink resolution which applies only when we are following symlinks.

The error handling for the traversal root and for reading subsequent
subdirectories may need to be different. For example, we want to know any
errors when opening the root itself but may want to ignore errors on
recursive travesal. This will have to be handled by the traversal handler.
The traversal handler can check the errors on the root separately and then
use error suppression on the entire traversal.

If we use a followSymlinks control at opendir, then the traversal handler
can resolve the root symlink separately and then do the entire traversal
with noFollowSymlinks. But if we do that the path may resolve to a different
prefix and we may want to emit paths with the original non-resolved prefix,
so it is better for readdir to return relative paths rather than prefixed
paths, or we should have the ability to supply a custom prefix to readdir.

We may throw errors and leave the handling to the traversal handler, but
that may less efficient and less ergonomic. Readdir will always have to
be wrapped into an exception handler, should measure if that works as well.

## Cyclic symlink paths

This applies only when following symlinks during traversal.

One way to curtail cyclic symlink traversal is relying on ELOOP, which
aborts traversal after too many levels of indirection.

If a loop is found we can ignore ELOOP and continue, however, it may be
inefficient because the cycle may be encountered repeatedly from different
nodes in the graph.

## Detecting Cycles

If we do not want to use ELOOP for some reason, or if we want a cycle detection
for graph traversal in general we can use a generic mechanism by tracking
visited inode/device or paths. Cycle detection can be done in the traversal
combinator (concatIterate).

When we encounter a symlink we resolve it into a canonical path, and then check
if the canonical path has already been visited. The canonical path serves as
the unique label of the node in the graph.

If we know unique labels identifying nodes in the graph we can maintain a bloom
filter and a hash table to find visited nodes quickly. If the nodes are
contiguous integers then a bitmap is enough.

## Broken symlinks

This applies only when following symlinks during traversal.

If there are broken symlinks, we have two options, check it even before
emitting the dir in traversal output or check it when we perform readdir.
Postponing it may be better because we may not even need it. But it can also
be done via traversal predicates to check whether file exists or not.

Another point is whether we handle broken links via an explicit option or
via stat based traversal predicates. If we do it via predicates then it will
require full resolution of the link recursively, detecting ELOOP right
there, if we resolve only once then readdir will anyway have to resolve
again when the path is sent back for reading via traversal handler. One way
is to let readdir resolve once and emit the result in the traversal output,
but that will create recursion cycles that will have to be detected in the
traversal handler and it will also be inefficient because of too many
resolution steps. Though something like this can make sense in handling http
crawler redirections.

## Symlink resolved vs unresolved paths

When a symlink is encountered, should we emit its path through the traversal
root or should we use the resolved path which could be any arbitrary path.
Should we control this with options?

If we use resolved paths then the cost of resolving will not be paid during
further traversals, but that may or may not be significant.

Also, if we use the resolved path then the traversal depth will have to be
maintained, we cannot determine it by just counting the components after
removing the traversal root.

## Traversal Depth

How is depth defined?

To keep things simple, traversal depth is defined as the number of recursive
traversal steps from the starting traversal root. A symlink resolution is
counted as only one step even if it involves many indirections.

Readdir itself is unaware of traversal depth. We pass it a parent directory
and it reads the children. Recursing into one of the children counts as
one level deeper.

If descendant paths are constructed incrementally by appending child names
returned by readdir, and readdir never returns "." or ".." entries, then
recursion depth is identical to lexical path depth relative to the traversal
root.

Therefore, traversal depth can be computed statelessly from constructed
descendant paths by stripping the traversal root prefix and counting path
segments. This can be checked before descending further.

It does not matter whether the traversal root itself contains ".."
segments or symlinks, as long as descendant paths are constructed only by
appending returned child names.

However, this approach has a disadvantage, if a child is a symlink

## Full vs Relative Paths

Paths returned by "read" can be:

* absolute      (/x/y/z)
* relative to cwd (./y/z)
* relative path segments (y/z)

To accommodate all cases we can provide a prefix to readdir to attach while
generating paths. For example, the prefix would be "/x" in the previous
example.

Alternatively, higher layers could attach prefixes, but generating the
final path once inside readdir is likely more efficient than rebuilding it
later.

Emitting paths as a tuple (parent, child) may be more efficient because the
parent path can be shared instead of copied into every emitted path.

One advantage of a lower-level API emitting tuples is that construction of
full paths can be deferred until chunk creation or final output buffering
downstream. If the lower-level API itself emits full paths and downstream
processing copies those paths again into chunk buffers, then the earlier
path-prefix copy may become additional overhead.

Therefore, one possible design is for the lowest-level API to always emit
(parent, child) tuples, while higher-level APIs expose either:

* relative paths
* full prefixed paths

generated from the tuple representation.

However, tuple representation also complicates downstream stream processing
because consumers often require self-contained full paths and would need to
repeatedly reconstruct them.

Alternatively, readdir can directly emit either relative or full paths based
on configuration options. This keeps the stream representation simpler and
more ergonomic, though it may lose the advantage of deferring parent-prefix
copying until final chunk construction.

Need to benchmark whether deferred parent copying provides measurable gains
compared to the simpler direct full-path representation.

## Option structure

The options that apply only when followSymlinks is "on" can be passed as an
argument to (followSymlinks SymlinkOptions), for default we can use
(followSymlinks id), to disable we can use noFollowSymlinks. Or just keep a
flat structure and these options do nothing when not following symlinks.

## Benchmark workloads

For symlink heavy workload we can try using a nix store.

For stat efficiency we need to use a filesystem/OS that does not provide the
type of file during readdir.

