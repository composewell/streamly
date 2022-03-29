Introduction
------------

Paths are used by file systems as well as protocols to represent paths to files
and other resources. We need a generic type safe way to represent paths in
general and file system paths in particular.

Path limits
-----------

OS include files generally define PATH_MAX to 4K and NAME_MAX to 255,
however, it is possible to create paths bigger than these depending on
the file system.

Scalability
-----------

In general directory trees could be quite deep and a directory can contain
millions of entries. A good benchmark to measure the efficiency of path
representation would be to traverse a directory tree recursively and list all
the nodes under the tree. We could do this many times so that we do not need a
really big directory tree.

Compatibility
-------------

A file system starts with just the root directory and then files are
created in the file system by the user or by programs storing their data
on the file system. When a directory or file is created, or when a directory is
listed, the following operations are performed:

1. for lookups an existing directory name must be resolved based on the name
   supplied by the user.
2. for creation the file name to be created is supplied by the user

When the user asks the file system to lookup or create a file or
directory in the file system:

1) The operating system passes the name, as it is without any changes
   whatsoever, to the file system. or does it? Windows?
2) The file system may translate the name to its own conventions before a
   lookup or create, e.g. it may

   * change the name to upper case
   * translate the name to 8.3 chars
   * change the character encoding?
   * change the unicode normalization form of the name (Apple)

When resolving an existing directory name in the file system we need
to supply a path which consists of component names separated by a separator
byte. Separators are of no consequence to the file system, they are
resolved by the OS and the path components are used to lookup the paths
one components at a time. The path for lookup is acquired either by a
user input, device input or by the program which previously got the path
entries by traversing the file system itself.

1) When the path is acquired by a user input,  the user input could be:

   a) a literal string in the program
   b) a path entered via an input device
   c) a path coming from the network

2) If the path was previously acquired from the file system then the
   best thing to do is to never change anything in the path and store it
   as it is and supply exactly the same path when needed. That way we can
   guarantee that the path remains exactly what it was in the file system.

Handling String Literals
========================

The encoding of the source code file depends on the editor used and the
encoding chosen when saving it. The string literals would be parsed
by the GHC parser and then stored in the generated binaries as null
terminated C string literals encoded in UTF-8 (see GHC reference). There are
several possible points of failure here:

a) GHC parser needs to interpret the source code encoding correctly.
b) We assume that the editor does not perform any translation on the
   literal as entered by the user e.g. it does not perform unicode
   normalization on it. If it does then the string as entered by the user
   won't remain the same when it reaches the file system.
c) GHC parser stores the parsed string literal in UTF-8 encoding. We
   assume that GHC does not perform any unicode normalization or any
   other translation on the string.

The UTF-8 encoded string literal can be passed as a blob of bytes to the file
system or it can be converted to String type and re-encoded as UTF-8 both
should work equivalently in this case.

Handling Input From Devices
===========================

The path provided by the user would assume some encoding based on
the terminal settings or the encoding assumed by the sender over the
network. The correctness depends on the contract between the two parties
e.g. the locale setting. We assume that we get a sequence of raw bytes
from the input device. We need to use the sequence as raw bytes and send
it as it is to the file system without any translation.

Manipulating Paths
------------------

We need to parse the path components by the separator bytes.  We assume
that the separator can be identified and removed correctly irrespective
of the encoding. We also make sure that none of the bytes in the
components is a separator byte.

Also, we would join the path components by the separator byte
irrespective of the encoding of the components. If the OS treats the
path as a sequence of bytes and nothing else and the components do not
have the separator byte then we are good, we know that the OS would also be
parsing based on the separator as a raw byte.

We may perform some validations on the paths such as the file names are
not "." or "..". Such validations could be optional and we could also
provide a way to not perform any validations and just blindly use the
paths as provided by the user and let the file system/OS fail.

File System Translations
------------------------

As we noted earlier, the file system may translate the paths before
using them.  For example, it may store the path after converting it to
NFD unicode normalization. Translation may create some round tripping
issues for programs. For example, a program may use a string literal
which is stored in NFC and the file system converts it to NFD. Later,
when the same path is retrieved from the file system and compared with
the string literal that was to create it then it won't match. For such
cases the programs need to understand the file system and perform
comparisons by performing appropriate translations on the paths. To
perform matching and translations correctly the program needs to
correctly interpret the encoding specific to the file system.

Displaying Paths
----------------

When we display the paths to the user then we are forced to interpret
it according to some encoding, to display the path correctly we have to
know exactly how the file system stores the path. Otherwise if we display
it differently, the user may use the displayed result to find the file
and may not find it.

Type Safety Requirements
------------------------

* Safety against using an absolute path where a relative path is to be
  used and vice-versa.  
  
  * Validations for absolute or relative path when constructing a path.
  * We cannot append an absolute path to another path
* Safety against using a file name where a directory name is to be used and
  vice-versa.

  * Certain validations can be performed e.g. file names cannot be "." or "..".
  * We should not be appending more directory components to a file path

In don't care situations we should be easily able to use any type
conveniently or cast a type into another.  It is desirable that the
programmer can choose the safety level. For example, we should be able
to instantiate a path type where we only worry about the distinction
between Absolute and Relative paths but no distinction between files and
directories or vice versa.

Requirement Summary
-------------------

* minimal dependencies, specifically streamly does not depend on bytestring
* round-tripping safety wrt to file system returned paths
* type safety for different path types
* support Posix/Windows
* support URI paths and other ways to represent paths where the separator could
  be different.

Design Considerations
---------------------

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

Some related links found by web search:

* https://gitlab.haskell.org/ghc/ghc/issues/5218
* https://nodejs.org/fr/docs/guides/working-with-different-filesystems/
* https://unix.stackexchange.com/questions/2089/what-charset-encoding-is-used-for-filenames-and-paths-on-linux
* https://docs.microsoft.com/en-us/windows/win32/intl/character-sets-used-in-file-names
* https://beets.io/blog/paths.html
