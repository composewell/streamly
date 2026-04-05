# Option Modifiers

Instead of exposing internal record structures, our best practice is
to use **Option Modifiers**: functions of type `Config -> Config`
(endomorphisms) that are composed to build a final configuration.

## Advantages

* **Clean Composition:** Use the standard `.` operator to chain multiple
options together seamlessly.
* **Encapsulation:** The internal record structure and field names
remain hidden, preventing breaking changes if the implementation shifts.
* **Order Independence:** Unlike positional arguments, the order of
modifiers in a chain does not affect the final result (unless a specific
option is overridden).
* **Unified Interface:** Removes the need for record syntax or multiple
constructor patterns. There is only one way to specify options: through
functions.
* **Bundling:** Common configurations can be pre-composed into a single
named modifier (e.g., `withForce Force . recursive True`).
* **Discipline:** By limiting the "surface area" of the API, we reduce
the possibility of misuse or invalid state transitions.

## Disadvantages

* Deserialization is problematic unless the module provides a way to turn the
  composed functions to a record.
* Enumeration of all options at one place is not possible.

To mitigate these the internal implementation using a record can be exposed, we
can dump the composed options to a record or read options from a record.

## Modifiers and Setters

A combination of independent options usually represented by a record is
represented by different functions, one for each record field.

```
data RmOptions = RmOptions
    { rmForce :: RmForce
    , rmRecursive :: Bool
    }

withForce :: RmForce -> RmOptions -> RmOptions
recursive :: Bool -> RmOptions -> RmOptions
```

When an option has a choice it is represented by constructors of a sum
type so that only one state is representable.  For example, here we can
choose one of the three choices:

```
data RmForce
    = NoForce
    | Force
    | FullForce

withForce :: RmForce -> RmOptions -> RmOptions
```

## Naming

For toggling options like `recursive` we can name them like attributes which
can be true or false. For example, `recursive True`.

For options that have multiple choices we prefer the `with` prefix e.g.
`withForce`. Another alternative is `set` prefix but `with` is usually
clearer.  Unlike `set`, which implies an imperative "action" or a binary
toggle, `with` conveys a functional transformation. It suggests that
the resulting operation will be performed *with* a specific property or
value, regardless of the previous state.

## Expressing All Possible Values

A modifier must be **total**. It should not merely "toggle" a default;
it must allow the user to explicitly define the desired state.

If a user receives a pre-composed bundle of modifiers, they may not know
the current state of a specific option. To ensure predictable behavior,
the modifier must allow them to force a value (e.g., `withVerbose True`
or `withVerbose False`), ensuring the final config matches their intent
regardless of the input chain.

For example, if default is not-recursive we may be tempted to use
`recursive` without an argument, but it is better to use `recursive`
with a Bool argument so we can say `recursive True` or `recursive
False`. This gives us the ability to set recursive to any value we want.

## Reset to Default (Optional)

While the base configuration starts with library defaults, we can
optionally provide `resetOption` functions (e.g., `resetRecursive`).

These are useful when you want to "neutralize" an option within a
specific composition. For example, if you have a `standardConfig` bundle
that includes recursion, but for one specific call you want to ensure it
is disabled, you can simply append `. resetRecursive` to the chain.

## Backing Record (Optional)

The internal implementation invariably uses a record for all options which is
passed around. If serialization of options is needed we can expose the record
and associated types such that we can dump the composed functions to the record
or build a options function from a record. This will provide a way to serialize
the options if needed.

## Summary

By using composed endomorphisms, we provide a declarative DSL for
filesystem operations. This approach balances the flexibility of a
record with the safety and elegance of functional composition, covering
all use cases while maintaining a strict, predictable API.
