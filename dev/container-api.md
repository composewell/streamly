# Data Containers

Containers are persistent containers of data e.g. files, arrays, maps. For
consistency we use similar API for all such containers where possible. Usually
the API names are relative to the current module e.g. toBytes in an array
module means converting the array to bytes, it sounds intuitive if read
qualified with the module name e.g. Array.toBytes. This doc lists some
conventions and guidelines to be followed.

## Bytes and Chunks

APIs that deal with streams of Word8 may have "bytes" in their names
while the ones dealing with "Array Word8" have "chunks" in their
names. Usually the default is bytes when there is no explicit mention in
the name.

## Unfolds and Folds

* Unfolds are named as "read" or with a "read" prefix (e.g. readChunks).
* Folds are named as "write" or with a "write" prefix (e.g. writeChunks).

## To and from Stream

### From and Put

* Immutable construction from some external source is named with a "from"
  prefix (e.g.  fromList).
* Mutation of an existing container uses a "put" prefix instead of "from". When
  an API uses an existing container to write to and does not return a newly
  constructed container then use "put".
* "from" vs "put": "from" assumes creation of a new object, it may fail if the
  object being created already exists (e.g. the file exists), it may not take a
  lock as it assumes immutability. "put" may create a new object or overwrite
  an existing one, it may take a lock for writing as it assumes mutability.

### To and Get

* Converting the complete object to an external representation is prefixed with
  "to" (e.g. toBytes).
* For mutable objects "get" APIs may be used instead of "to" APIs.
* "to" vs "get": "to" assumes immutable object so does not have to take a lock.
  "get" assumes mutable object so may take a lock.

### Append

* Use "append" prefix for appending data at the end of a mutable container

## With additional config

Sometimes we need to modify the behavior of a combinator using some additional
config. For example, combinators to read/write using a specified size of
buffer. For such cases we apply the "With" suffix to standard combinator names:

* readWith
* readChunksWith
* toBytesWith
* getBytesWith

## Random Access (Arrays)

### Single elements

* getIndex (for arrays)
* putIndex (for mutable arrays)

### Ranges

* ...FromTo (e.g. readFromTo, readChunksFromTo, toBytesFromTo, getBytesFromTo)
* ...FromThenTo
* ...Indices

We could use FromThenTo or FromStepN style for stepwise enumeration. We chose
FromThenTo style for the following reasons:

* It is the style used in base lists, and we are already using it in Enumerable
  type class.
* Both have their pros and cons. It may be non-intuitive whether To is
  inclusive in one, we have to compute the correct count in the other one.

### Appending

* append (for mutable arrays)

## Key-Value Store Access (Maps)

* getKey
* findKey (test existence)
* createKey (insert new key for mutable maps)
* putKey (insert or update for mutable maps)
* updateKey (update existing or fail)
* deleteKey  (delete existing or fail)
* destroyKey (delete existing or not)

## Points to Consider

The fold and unfold APIs can be used to express the to/from stream APIs. So we
may not need both, it may just add to more APIs being proliferated.

We may need an "append" style fold as well? What would "stream" append
operations be called then?

We may need locked version of "write" folds for mutable containers for
concurrent access.
