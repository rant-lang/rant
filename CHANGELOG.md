## 4.0.0-alpha.2

* Added new `Rant::with_options` constructor that lets you customize the context using new `RantOptions` struct.
* Added `RantValueType` to represent the type of a value.
* Changed `IndexError` and `KeyError` to use `RantValueType` instead of `&'static str` to represent types.
* Renamed `get_by_index`, `set_by_index`, `get_by_key`, and `set_by_key` methods in `RantValue` to `index_get`, `index_set`, `key_get`, and `key_set` respectively.
* Expanded type aliases on `Rant::compile*` method return types to make them less confusing.
* Hid some modules that don't need to be user-facing.

## 4.0.0-alpha.1

* Initial alpha release