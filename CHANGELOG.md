## 4.0.0-alpha.2

* Added new `Rant::with_options` constructor that lets you customize the context 
* Added `RantValueType` to represent value types without an attached value
* Changed `IndexError` and `KeyError` to use `RantValueType` instead of `&'static str` to represent types
* Expanded type aliases on `Rant::compile*` method return types to make them less confusing
* Hid some modules that don't need to be user-facing

## 4.0.0-alpha.1

* Initial alpha release