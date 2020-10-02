# 4.0.0-alpha.7

* Main program body can now be returned from with `[return]`
* Added new stdlib functions:
  * `[break]`
  * `[continue]`
  * `[fork]`
  * `[max]`
  * `[min]`
  * `[return]`
  * `[shred]`
  * `[shuffled]`
  * `[sort]`
  * `[sum]`
  * `[unfork]`
* Upgraded `smartstring` to 0.2.5

# 4.0.0-alpha.6

* Modules are now cached by the executing context in a global map
* Added support for variable capturing in closures
* Added `.delete_global()`, `.has_global()`, `.delete_global()`, `.global_names()` methods to `Rant`
* Removed `.globals()` method from `Rant`
* Upgraded `quickscope` to 0.1.3

# 4.0.0-alpha.5

* Added `Rant::with_random_seed()` constructor
* Added `.compile_named()` and `.compile_quiet_named()` methods to `Rant`
* Added support for multi-part accessors
* Added support for modules
* Removed `RantProgram::with_name()` method
* Replaced `RuntimeErrorType::GeneralError` with `RuntimeErrorType::UserError` 
* Upgraded `quickscope` to 0.1.2
* `[sep]` now supports `block` values and will auto-resolve them for each separator
* Improved stack trace formatting
  * Error messages are categorized and have improved readability
  * Consecutive duplicate stack frames are now combined
* Added new stdlib functions:
  * `[require]`
  * `[error]`

# 4.0.0-alpha.4

* Changed: Runtime now uses `quickscope` crate for variable access
* Changed `Rant::run()` to return a `RantValue` and added `Rant::run_into_string()` to explicitly return a `String` output
* Added descope and explicit global modifiers to accessors
* Added implicit local variable scope to program root
* Added support for whitespace normalization via `[whitespace-fmt]`.
* Added new stdlib functions:
  * `[insert]`
  * `[remove]`
  * `[take]`
  * `[whitespace-fmt]`
  * `[dignz]`
* Renamed stdlib functions:
  * `[dec]` -> `[dig]`
  * `[hex]` -> `[digh]`
* Removed `_GLOBALS` from stdlib
* Upgraded `smartstring` to version 0.2.4

# 4.0.0-alpha.3

* Added initial support for optional debug symbols. Enable them via `debug_mode` option in `RantOptions`.
* Added (very basic) stack traces. They may be buggy still.
* Added the `block` type.
* Added new stdlib functions:
  * `[and]`, `[or]`, `[not]`, `[xor]`
  * `[clear]`
  * `[copy]`
  * `[either]`
  * `[eq]`, `[neq]`, `[gt]`, `[lt]`, `[ge]`, `[le]`
  * `[has-key]`
  * `[if]`, `[else-if]`, `[else]`
  * `[indent]`
  * `[is-odd]`, `[is-even]`, `[is-factor]`
  * `[is-string]`, `[is-number]`, `[is-integer]`, `[is-float]`, `[is-bool]`, `[is-empty]`, `[is-nan]`
  * `[keys]`
  * `[lines]`
  * `[nop]`
  * `[push]`, `[pop]`
  * `[reset-attrs]`
  * `[resolve]`
  * `[shuffle]`
  * `[sorted]`
* Renamed `[n]` to `[num]`
* Renamed `[nf]` to `[numf]`
* Fixed compiler bug where non-printing whitespace was sometimes printed.
* Fixed bug where printing empties with a non-empty would cause output to be coerced incorrectly to a string.
* Fixed bug where sinked blocks print when they're not supposed to.
* CLI benchmarks are now hidden by default; enable with `--bench-mode` / `-b`.

# 4.0.0-alpha.2

* Add support for some block attributes: conditional value, repetitions, separator
* Added a lot of new stdlib functions
* Added `special` Rant type to represent opaque internal runtime data.
* Added `get_global()` and `set_global()` to `Rant`.
* Added new `Rant::with_options` constructor that lets you customize the context using new `RantOptions` struct.
* Added `RantValueType` to represent the type of a value.
* Replaced empty `Err` variant on `Rant::compile*` methods with new `compiler::ErrorKind` enum.
* Changed `IndexError` and `KeyError` to use `RantValueType` instead of `&'static str` to represent types.
* Renamed `get_by_index`, `set_by_index`, `get_by_key`, and `set_by_key` methods in `RantValue` to `index_get`, `index_set`, `key_get`, and `key_set` respectively.
* Expanded type aliases on `Rant::compile*` method return types to make them less confusing.
* Hid some modules that don't need to be user-facing.

# 4.0.0-alpha.1

* Initial alpha release.