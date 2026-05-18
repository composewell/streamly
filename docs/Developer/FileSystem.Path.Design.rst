WindowsPath and PosixPath Modules
---------------------------------

We should be able to manipulate windows paths on posix and posix paths on
windows as well. Therefore, we have WindowsPath and PosixPath types which
are supported on both platforms. However, the Path module aliases Path to
WindowsPath on Windows and PosixPath on Posix.

PATH CLASSIFICATION
-------------------

In general paths can be divided into the followign categories.

RelPath
 RelFree          -- x, ./x  -- both curdir and curdrive are unspecified
 RelFixedDrive    -- C:x  -- On windows, drive specified, path relative to current dir on that drive
 RelFixedDir      -- \x   -- On windows, absolute path on current drive (root-relative)

RelFixedDrive and RelFixedDir are Windows only cases.
When we split a relative path into a head and stem we get ".", "c:", "/" as
heads when the path is RelFree, RelFixedDrive and RelFixedDir respectively.

AbsPath -- fully specified path
On Posix, it is simple: "/x"
On Windows, there are multiple possibilities:
 AbsDrive    -- C:\x
 AbsUNC      -- \\server\share\x
 AbsVerbatim -- \\?\...
 AbsDevice   -- \Device\...

On Posix RelPath and AbsPath do not have any further classification.

When appending paths, on Windows, do not insert a separator after a bare
drive (C:). For all practical purposes a bare "C:" can be treated as "C:./"
and then we do not need this special treatement wrt separators.

   C: </> x -> C:x

PATH NAVIGATION SEMANTICS (follow)
----------------------------------

Most libraries (python, rust) including haskell filepath use the path
navigation semantics when composing paths ("</>" in filepath).

The "follow" operation navigates first path followed by the second. In
other words, "follow p1 p2" interprets p2 in the context of p1.

Operationally:
  cd (follow p1 p2)  ==  cd p1; cd p2

That is, p2 is resolved relative to the location denoted by p1.
The two paths denote a sequence of resolution operations, we resolve p1 and
then we resolve p2 with respect to p1.

Note that this operation is total and never results in an error.

Rules:

1. If p2 is Relative:

   Absolute </> Relative -> Absolute
   Relative </> Relative -> Relative
   RelFixed </> Relative -> RelFixed

   (p2 is appended to p1)

2. If p2 is Absolute:

   Any </> Absolute -> Absolute (p2 wins)

3. If p2 is RelFixedDrive (C:y), if the drive is the same then combine
otherwise take the second path. If the drive is not specified then it is
considered to be different.

   C:   </> C:y -> C:y    -- C: equiv C:./
   C:x  </> C:y -> C:x/y
   C:/x </> C:y -> C:/x/y
   D:x  </> C:y -> C:y

   /x    </> C:y -> C:y
   x     </> C:y -> C:y

   The "cd" semantics can be incorrect for the last two if we assume the
   drive of the first path to be same as the second.

4. If p2 is RelFixedDir (\y), discard LHS, if LHS has drive keep the drive:

   C:    </> \y -> C:\y    -- C: equiv C:./
   C:/   </> \y -> C:\y
   C:/x  </> \y -> C:\y
   C:x   </> \y -> C:\y
   \x    </> \y -> \y
   x     </> \y -> \y

   For the first 3 cases above, UNC behaves the same as a drive root:

   \\server\share\x </> \y -> \\server\share\y

These are based on how python 'ntpath' module behaves.

PATH CONSTRUCTION SEMANTICS (append)
------------------------------------

In Streamly path module the path "append" operation uses the path construction
semantics rather than path navigation semantics.

"append" operation constructs paths structurally. The second argument must
be such that it can be interpreted relative to the first. While "follow" is
total, "append" is partial and can result in runtime errors.

"append p r" extends path p suffixing the segments of r.

1. Always valid if path being appended is fully relative:

   appendRel :: RelPath -> RelPath -> RelPath
   appendAbs :: AbsPath -> RelPath -> AbsPath

2. Never valid if r is AbsPath:

   /   </> /x      -> error  -- can be allowed, but no exception
   p   </> AbsPath -> error

3. Identity:

   "." is the empty relative path, it is identity of composition:

   appendAbs p "." == p
   appendRel p "." == p
   appendRel "." p == p
   appendRelFixed p "." == p

4. Associativity (via RelPath):

   append (append p a) b == append p (a <> b)

Notes:

- "." is not an anchor; it is the identity element of relative paths.
- On Windows AnchoredPath can only start with "\" or "C:", it cannot start
with "C:\" as that would make it an AbsPath.

SUMMARY: append and follow
--------------------------

follow = resolution (contextual, may override)
append = construction (structural, no override)

follow models filesystem navigation semantics
append models path construction semantics

Appending Anchored Paths
------------------------

We provided a simple append algebra above, however, it gets complicated when
Windows anchored paths are considered.

If second path is RelFixed, and has the same Anchor as the first path, then
strip the Anchor into a Maybe Drive and a Relative or "/" Anchored path and
then apply the same rules as above considering the "/" Anchored path as
absolute.

1. If the second path is RelFixedDrive (C:y), if both the paths have drive
and it is the same then combine otherwise it is runtime error.

   C:\x </> C:y -> C:\x\y
   C:   </> C:y -> C:y    -- C: equiv C:./
   C:x  </> C:y -> C:x\y

   D:x  </> C:y -> error
   \x   </> C:y -> error
   x    </> C:y -> error

2. If the second path is RelFixedDir (\y). It is absolute within the
drive, therefore, similar to the absolute path rules, not allowed.

   C:\   </> \y -> error    -- can be allowed, but this will be an exception
   C:\x  </> \y -> error
   C:    </> \y -> error    -- C: is equiv C:. which is a relative path
   C:x   </> \y -> error
   \x    </> \y -> error
   x     </> \y -> error

   For the first 3 cases above, UNC behaves the same as a drive root:

Typed paths
-----------

Posix is simple but if we consider the Windows cases our algebra becomes
complicated. Cases that are allowed ::

   appendRel :: AnyPath -> RelFree -> AnyPath
   appendFixedDrive :: RelFixedDrive -> RelFixedDrive -> RelFixedDrive

   where AnyPath => AbsPath, RelPath, RelFixedDrive, RelFixedDir

Cases that are not allowed::

   appendFixedDrive :: AnyExceptRelFixedDrive -> RelFixedDrive -> Error
   appendFixedDir :: AnyPath -> RelFixedDir -> Error

We see that the additional Windows anchored paths behave more like AbsPath when
composing. So they fall in the AbsPath bucket.

Rooted Paths
------------

To keep the types and algebra simple we extend the concept of AbsPath to
RootedPath which is a path which may have some sort of root or anchor attached
to it. This includes absolute paths as well as the Windows anchored paths::

   appendUnrooted :: Unrooted -> Unrooted -> Unrooted
   appendRooted :: Rooted -> Unrooted -> Rooted

Now, if we do that we disallow some cases that are possible for anchored but
not absolute paths. These cases are very few and a bit unusual, and we can do
without allowing them as well. The cases that we have are ::

   C:\x </> C:y -> C:\x\y
   C:   </> C:y -> C:y    -- C: equiv C:./
   C:x  </> C:y -> C:x\y

To allow these cases we can provide a "combine" operation to combine Rooted
paths that have a common anchor and the path is relative to that anchor where
one more dimension of the path is free to change, and can be combined (e.g.
drive is fixed but current directory can change).::

   combineRooted :: Rooted -> Rooted -> Maybe Rooted

This operation can fail if the path does not have a free dimension that allows
it to combine or the root is not the same.

How will the splitRoot operation behave when considering anchored paths::

  splitRoot :: Rooted -> (Rooted, Unrooted)
  -- Posix
  splitRoot "/" => ("/", ".")
  splitRoot "/x" => ("/", "x")
  -- Windows
  splitRoot "/" => ("/", ".")
  splitRoot "/x" => ("/", "x")
  splitRoot "C:" => ("C:", ".")
  splitRoot "C:/" => ("C:/", ".")
  splitRoot "//server/share" => ("//server/share", ".")

To combine Rooted paths, split the root first and combine the Unrooted paths if
the root is common and not absolute drive or absolute dir in a drive.

Examples of Rooted: "/", "/x", "C:", "C:x", "C:/", "//x/y".
Examples of Unrooted: "x", "x/y", ".", "./x", "..", "../x".

------------------------------------------------------------------------------
Naming Summary
------------------------------------------------------------------------------

Path classification is divided along two orthogonal dimensions:

* Rootedness: ``Rooted`` / ``Unrooted``
* Node type: ``File`` / ``Dir``

The modules corresponding to these dimensions are:

* ``Streamly.FileSystem.Path.Rooted``
* ``Streamly.FileSystem.Path.FileDir``
* ``Streamly.FileSystem.Path.Typed``

The ``Typed`` module combines both dimensions, allowing types such as::

    Rooted File
    Rooted Dir
    Unrooted File
    Unrooted Dir


------------------------------------------------------------------------------
Rooted / Unrooted
------------------------------------------------------------------------------

We considered several alternatives:

* ``RootedPath`` / ``UnrootedPath``
* ``AbsPath`` / ``RelPath``
* ``Anchored`` / ``Branch``, ``Segment``

``AbsPath``/``RelPath`` are concise and familiar, but on Windows we also need
to classify constrained paths like::

    C:x
    \x

These paths are rooted/constrained but not truly absolute because they still
depend on ambient process state such as the current directory or current drive.

Treating such paths as ``AbsPath`` weakens the conventional meaning of
"absolute", and leaves no stronger term for paths that are fully anchored and
context-independent.

Therefore we use ``Rooted``/``Unrooted``:

``Rooted``

    Paths with anchoring semantics. These may be fully absolute or partially
    constrained.

``Unrooted``

    Pure appendable path branches with no anchoring semantics.

This terminology:

* preserves the conventional meaning of "absolute"
* thus allows using (isAbsolute Rooted)
* expresses Windows path semantics
* matches the append algebra
* keeps path append total and type-safe


------------------------------------------------------------------------------
Why not use the ``Path`` suffix?
------------------------------------------------------------------------------

We considered names like::

    RootedPath
    UnrootedPath

However, these names become verbose and repetitive when composing orthogonal
path dimensions::

    RootedPath FilePath
    UnrootedPath DirPath

Since these types are already defined within the
``Streamly.FileSystem.Path`` hierarchy, the additional ``Path`` suffix does
not add much information.

Using shorter modifier-style names keeps the type algebra concise and easy to
read::

    Rooted File
    Unrooted Dir

This style also scales naturally when combining multiple orthogonal path
dimensions.


------------------------------------------------------------------------------
Module Names
------------------------------------------------------------------------------

``Rooted``

    Contains the rooted/unrooted distinction.

``FileDir``

    Contains the file/directory distinction.

``Typed``

    Fully typed paths, combines the rootedness and node-type dimensions.

We also considered:

* ``Rooted``: ``AbsRel``, ``Seg``
* ``FileDir``: ``Node``, ``Kind``

but ``Rooted``/``FileDir``/``Typed`` were chosen because they use familiar
filesystem terminology and avoid introducing abstract or type-theoretic
vocabulary into the public API.

Comparing Relative Paths
------------------------

We can compare two absolute rooted paths or path branches but we cannot
compare two relative rooted paths if the implicit meaning of the roots
may be different or contextual. If each component of two unrooted paths
are equal then the paths are considered to be equal.

Implicit Rooted Paths (. as root)
---------------------------------

The following is a possible strict way of treating implicitly rooted relative
paths, but we are not doing this because this may become surprising and go
against the established intuition.

The special path component "." implicitly refers to the current directory. On
Windows a path like @/Users/@ has the drive reference implicit. Such references
are contextual and may have different meanings at different times.

@./bin@ may refer to a different location depending on what "." is referring
to. Thus ideally we should not allow @./bin@ to be appended to another path,
@bin@ can be appended though. Similarly, we cannot compare @./bin@ with @./bin@
and say that they are equal because they may be referring to different
locations depending on in what context the paths were created.

The same arguments apply to paths with implicit drive on Windows.

Strictly speaking @.\/bin\/ls@ can be treated as an absolute path with "." as
an implicit root. On the other hand "bin/ls" is relative path which represents
steps from somewhere to somewhere else rather than a particular location. We
can also call @./bin@ as a "rooted path" as it starts at a particular location
rather than defining "steps" to go from one place to another. If we want to
append such paths we need to first make them explicitly relative by dropping
the implicit root. Or we can use unsafeAppend to force it anyway or unsafeCast
to convert absolute to relative.

If we compare these absolute/located paths having implicit roots then result
should be EqUnknown or maybe we can just return False?. @./bin@ and @./bin@
should be treated as paths with different roots/drives but same relative path.
The programmer can explicitly drop the root and compare the relative paths if
they want to check literal equality.

Note that a trailing . or a . in the middle of a path is different as it
refers to a known name.

Normalizing Paths With (..)
---------------------------

".." in a path refers to the parent directory relative to the current path.
For an absolute root directory ".." refers to the root itself because you
cannot go further up.

When resolving ".." it always resolves to the parent of a directory as
stored in the directory entry. So if we landed in a directory via a symlink,
".." can take us back to a different directory and not to the symlink
itself. Thus @a\/b/..@ may not be the same as @a/@. Shells like bash keep
track of the old paths explicitly, so you may not see this behavior when
using a shell.

For this reason we cannot process ".." in the path statically. However, if
the components of two paths are exactly the same then they will always
resolve to the same target. But two paths with different components could
also point to the same target. So if there are ".." in the path we cannot
definitively say if they are the same without resolving them.

Normalization and comparison of paths
-------------------------------------

Windows literal paths:

Windows "Literal" Paths (\\?\): When you prefix a path with \\?\, you are
telling the Windows APIs to turn off all "normalization".

Object Manager Paths: On Windows, paths like \??\C:\ or
\Device\HarddiskVolume1\ have very specific rules about separators.

POSIX //
--------

On POSIX a path starting with exactly two slashes ("//x") is
implementation-defined.

See https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html

If a pathname begins with two successive <slash> characters, the first
component following the leading <slash> characters may be interpreted in an
implementation-defined manner, although more than two leading <slash>
characters shall be treated as a single <slash> character.

This is rarely or historically used on Posix but may be of importance in
portable cygwin style paths where a UNC path \\server\share\file gets
converted to Posix style //server/share/file .

If we want this behavior on Posix we can treat the path as a Windows path
and use Windows path operations on it.

Design Considerations (old)
---------------------------

This section is from early thoughts and may be obsolete with respect to the
current design.

* Should we store path as separate components or single string with
  separators?

* Should we validate the paths returned from the file system or trust
  those and use directly without any validations? Need to see if that makes
  any difference to path heavy benchmarks. If we want to use it directly
  then we have to store it as a single string.

* Parameterize the low level APIs with the separator so that we can
  support arbitrary separators when parsing or reconstructing paths.

* The low level API can support path handling in trees/DAGs/Graphs in general.
  For example, in trees we cannot have multiple parents of a child whereas in
  DAGs that is allowed, in graphs we can have cycles. We may also need ways to
  detect cycles.

* Do we need to support arbitrarily long paths i.e. streaming of path? We do
  not need that for file system paths and file system paths are limited size
  and operating system anyway requires them in strict buffers. In case of
  graphs if we have cycles paths can be infinite, we could generate a stream of
  path and the consumer could be traversing the graph according to the
  generated stream. If we want to support streaming then we have to store paths
  as a stream of chunks rather than a single string.

* In general, paths need not be strings, e.g. they can be references to
  locations in memory or they can be IP addresses of nodes. At an abstract
  level, paths are just a stream of tokens that represent a certain traversal.

* Relative paths are the most general representation. At a low level,
  all paths are relative, absolute paths are relative to a specified root
  whereas relative paths are relative to a dynamic root which is the
  current directory.

* Windows can have the root as different drive letters. So to represent paths
  with a root in general we can also store the specific root along with the
  path. In case of POSIX this will always be "/". In general, it could be a
  host name or IP address or dependent on the protocol whose path we are
  representing.

* We can parameterize the low level path type with the type of path e.g. POSIX,
  WINDOWS, HTTP etc. In general, programs may have to manipulate different
  types of paths at the same time. High level path types can be instantiated
  using the low level type therefore they can be much simpler as desired.

References
----------

Windows paths:

* https://docs.microsoft.com/en-us/windows/win32/intl/character-sets-used-in-file-names
* https://learn.microsoft.com/en-us/windows/win32/fileio/naming-a-file
* https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-dtyp/62e862f4-2a51-452e-8eeb-dc4ff5ee33cc

Related Packages
----------------

* https://hackage.haskell.org/package/paths
* https://hackage.haskell.org/package/path
* https://hackage.haskell.org/package/hpath
* https://hackage.haskell.org/package/filepath
* https://hackage.haskell.org/package/file-io
* https://hackage.haskell.org/package/os-string
