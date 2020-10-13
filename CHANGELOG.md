# 4.0.0-alpha.11

* Added: New stdlib functions:
  * `[zip]`: combine two lists into a new list using a function

# 4.0.0-alpha.10

* Added: Inverse indices in accessors
* Added: Anonymous getters/setters
* Added: "Function percolation": function calls will now descope until the requested variable provides a function or other callable value
* Changed: Compiler now consolidates adjacent fragments/escape sequences into single AST node
* Changed: Updated `quickscope` to 0.1.5
* Removed: `[get]`
* Fixed: Dynamically-keyed setters have wrong value stack insertion order 

# 4.0.0-alpha.9

* Added: `RuntimeErrorType::TypeError`
* Added: `Rant.run_with()`, `Rant.run_into_string_with()`
* Added: new stdlib functions:
  * `[filter]`: make a copy of a list containing only items that pass a predicate
  * `[map]`: apply a function to items in a list and return another list with the results
  * `[oxford-join]`: print a list with custom separators for applying series commas and conjunctions
  * `[rand-list]`: generate a list of random integers
  * `[randf-list]`: generate a list of random floats
  * `[translate]`: map a list's values to the values in a map
* Fixed: `[sifted]` and `[squished]` returning `<>` when input list meets target size
* Fixed: Sinks not honored on native function calls

# 4.0.0-alpha.8

* Added: `RantProgramInfo` struct
* Added: `[require]` now searches the containing directory of the running program before the local modules path
* Added: new stdlib functions:
  * `[alpha]`: print a string of random alphanumeric characters
  * `[assoc]`: create a map from a list of keys and a list of values
  * `[ceil]`: get the smallest integer greater than or equal to a number
  * `[floor]`: get the largest integer less than or equal to a number
  * `[frac]`: get the fractional part of a float value
  * `[sift]`: remove random items from a list down to a target size
  * `[sifted]`: create a random, ordered subset of a list
  * `[squish]`: merge random adjacent elements in-place in a list using addition until a target size is reached
  * `[squished]`: make a copy of a list with random adjacent elements merged down to a target size
* Changed: `[require]` now accepts a relative path, as long as it is contained in the modules directory
* Changed: `RantProgram.name()` now returns `Option<&str>` instead of `&str`
* Changed: Renamed `RantOptions.local_module_path` to `RantOptions.local_modules_path`
* Changed: Renamed stdlib functions `[num]` and `[numf]` to `[rand]` and `[randf]`, respectively
* Fixed: Function calls don't trigger variable capture pass
* Fixed: def + get/set of same variable in chained accessor causes erroneous variable capture inside functions

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