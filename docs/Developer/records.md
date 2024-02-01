Tabular data processing
-----------------------

Extensible records can be directly mapped to SQL queries. We can also
perform LINQ like queries. We can have this generic DSL for tabular
data, it can either map to DB schema or to CSV or any other format of
tabular data.

Joins should be performed by us and not by the database. That could simplify
the SQL query generation.

Same SQL queries can be performed on CSV data or a DB or any other form of
tabular data.

Extensible records
------------------

We basically need a sequence of types (type level/hetero lists?) and a
conforming value level representation which is Storable for efficiency.

Most existing extensible records are represented with static tuple
types. Can we represent these using a single type level list type
instead. This is also similar to effect systems, we can learn from those
as well and we can also plan using this to build an effect system.

Operations on records, it would be just like arrays except that the fields are
heterogeneous rather than homogeneous:

* Create a new record
* get a field
* set a field
* Serialize to byte stream
* Apply a view/mask on the record (just take selected fields from the
  record and create another record from that)
* Extend a record at the end (this extends the schema)
* insert a field in the record at a given position (changes the schema)
* Merge two records to create a union record (changes the schema)
* intersect two records to take common fields (changes the schema)

Based on the type of the record, we should be able to generate getters and
setters automatically or use lens like operators.

The type of the record is basically the schema. We need a convenient DSL to
operate on schemas and the value level functions should be generated
automatically from the schema.

To construct a record we append the values of different types and it
creates a value of the combined type.

We can slice a record like array slicing. We could keep the underlying
memory of the array and make the fields of the new record refer to the
underlying memory of the old record.

An SQL query then just becomes record slicing and filtering. The
database should only provide a way of storing and retrieving entire
records, a stream of those, it is basically a key/value store. The rest
of the DB operations (slicing, filtering, joining) should be outside the
DB.

The DB schema would correspond to a record type and all the SQL required to
interact with the DB can be generated from that.

We can use SQL to manipulate types instead. For example, "select
(FieldX, FieldY, FieldZ) from RecordA as RecordB" would create another
record type and all the functions to migrate values of RecordA to the
new type. For example, it could generate:

* recordAToRecordB :: RecordA -> RecordB -- copying operation
* sliceRecordAToRecordB :: RecordA -> RecordB -- slicing operation

Or we could have a type class::

class migrate a b where
  migrate :: a -> b

And the select operation above would generate the instances automatically for
the two types involved in the migration.

We could have more involved queries where the migrate could even do other
operations rather than just filtering the fields from a record, it could map
functions after filtering converting a type to another type as well.

We can directly map our records on the buffers that we get by reading a DB
table.

Look at row types?

Streamly.Tabular
----------------

For multistream operations like intersection, joins union etc. Basically for
tabular data processing.

or we can keep this in stream modules, but have more specific tabular data
processing based on multiple columns and row wise processing in
Streamly.Tabular.

specifically for keyed data.

Extensible records
------------------

In a stream of values of type "a" we can augment the value in the next stream
pipeline by a tag producing (s, a), which can again be augmented as (s1, (s,
a)), and then (s2, (s1, (s, a)) and so on. Producing a nested structure.

The good thing about this is that we are only using a tuple of two elements
throughout, so we do not need to deal with tuples of large arities.

We can represent large structures like this though it is inefficient because of
too many indirection layers.

This binary tree structure has a flattened dual. We can flatten it to an n-ary
record. First we flatten it like:

* (s2, (s1, (s, a))
* s2 (s1, (s, a))
* s2 s1 (s, a)
* s2 s1 s a

The type (s2, (s1, (s, a)) can be used to automatically derive the type of
flattened structure, so the type flat structure would be "toFlat (nested
type)". We can flatten one/twice/... or fully.

We can cast/coerce the nested structure into a flat structure and
vice-versa.  If the values are all storable we can use a C like
structure to store the record. We can always cast that flat structure to
any of the intermediate forms of the nested structure.

Note that (s2, (s1, (s, a)) is just a heterogenous linked list. The
corresponding flat structure is like an array.


