# fast-record

Fast anonymous records in Haskell, at the cost of using some unsafe constructs. Current features:

* Type-indexed field names/types
* Space efficient, O(1) field accessing
* `lens`-compatible lens for free
* Minimal footprint; no need for Template Haskell

## Quick Intro

Take a look at [`Data.FastRecord.Example`](src/Data/FastRecord/Example.hs) to get some idea on how things work.

### schemas

An anonymous record tracks its schema in its type. We use `ts :: [(Symbol, Type)]` to represent a schema. The kind of `ts` is simple: a list of fields, each field consists of a name and a type. So `'[ '("foo", Bool), '("bar", String)]` is similar to `data FooBar = FooBar { foo :: Bool, bar :: String }`. The `'` prefix on type constructors is related to the `DataKinds` extension.

Code related to schemas is in [`Data.FastRecord.Schema`](src/Data/FastRecord/Schema.hs). We use type families to do some calculations at the type level (like the total number of fields, or the index of a particular field), then we apply constraints like `KnownNat (FieldIndex ts sym t)` to our function types, which means we can use `Proxy#`s to connect type-level and value-level and obtain results of type-level computations at runtime.

Functions with names ending with `#` explicitly pass `Proxy#`s around and aren't recommended for use by end-users.

### records

`Rec ts` is the actual record value. The common representation of anonymous records is type-indexed heterogeneous lists, which is elegant and type-safe. Here we use primitive `SmallArray#`s instead to improve performance. The memory is more compact (a constant overhead over normal records), and field-accessing is O(1).

We can get/set a field of a record via `getField#`/`setField#`. We can also initialize an empty record via `nullRec#`, then set its individual fields.

### lens

The recommended way to get/set a field is to use lens. Import [`Data.FastRecord.Field`](src/Data/FastRecord/Field), turn on the `OverloadedLabels` extension, and simply use `label #foo` as a lens for the `foo` field. It works for any schema including a `foo` field.

### bulk updating

There are cases when we wish to update several fields at once. With nested single-field updates, the record is copied for multiple times which is a huge waste.

[`Data.FastRecord.BulkSet`](src/Data/FastRecord/BulkSet.hs) is for bulk updating. A `BulkSet ts` value describes the action of updating one or more fields of a `Rec ts`. It can be generated and composed using the `Monoid` interface (if there's overlapping, the right side has higher priority). We can also use the `~=` operator to describe assigning a single field.

So a typical `BulkSet ts` looks like `Proxy @"foo" ~= True <> Proxy @"bar" ~= "233"` (with `TypeApplications` turned on)

A `BulkSet ts` value can be used to update a record (via `bulkSet`) or initialize a new record (via `newRec`).

## What comes next

`fast-record` needs some more polishing before it's available on Hackage/Stackage. Possible improvements include:

* Better docs & a benchmark/test suite
* Schema singletons, useful for implementing stuff like constructor functions without Template Haskell
* Instances for common classes (`Show`, `Eq`/`Ord`, `NFData`, etc)
* Casting between records of different schemas
* Mutable/strict/unboxed fields
* Schema sanity check (e.g. field name dedup)
* etc
