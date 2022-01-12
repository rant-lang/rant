# Changelog

## 4.0.0-alpha.23 (unreleased)

### New
* Added stdlib functions:
  * `[print]`
  * `[chunks]`
  * `[bool]`
  * `[values]`
* Added stdlib constants:
  * `MIN_INT`, `MAX_INT`
  * `MIN_FLOAT`, `MAX_FLOAT`
  * `EPSILON`
* Added API items:
  * `RantValue::MIN_INT`, `RantValue::MAX_INT`
  * `RantValue::MIN_FLOAT`, `RantValue::MAX_FLOAT`
  * `RantValue::NAN`
  * `RantValue::EPSILON`

### Changes
* Changed hint operator: `'` &rarr; <code>`</code>
* Changed sink operator: `_` &rarr; `~`
* Changed emptyval literal: `~` &rarr; `<>`
* Added old behavior back to `[cat]`; previous `[cat]` behavior migrated to `[print]` function
* Upgraded library dependencies:
  * `cast` &rarr; 0.3.0
* Upgraded CLI dependencies:
  * `ctrlc` &rarr; 3.1.9
* Renamed stdlib functions:
  * `[bool]` &rarr; `[to-bool]`
  * `[string]` &rarr; `[to-string]`
  * `[int]` &rarr; `[to-int]`
  * `[float]` &rarr; `[to-float]`
  * `[list]` &rarr; `[to-list]`

### Removed
* Removed the `block` type
* Removed the defer operator (formerly `<>`)
* Removed stdlib functions:
  * `[resolve]`
* Removed `RantValue::nan()` (superceded by constant `RantValue::NAN`)

### Fixes
* Fixed bench stats in CLI printing to stdout instead of stderr
* Fixed anonymous calls always expecting a pipeval even when not in a piped context
* Fixed pipeval not getting captured in closures
* Fixed panic in `[pick]` when input list is empty; now prints nothing in this case
* Fixed several panics in number formatter when handling `<MIN_INT>`

## 4.0.0-alpha.22

(4/27/2021)

### New
* Added data sources (access through the `rant::data::DataSource` trait)
* Added inline lambdas (you no longer need braces for lambdas with a single expression in them!)
* Optional function parameters can now have user-specified default values
* Exposed `Rant.options` through the `Rant.options()` method

### Changes
* Boolean literals are now keywords; this means that `true` and `false` are now `@true` and `@false`. The original reserved fragments are now treated like strings.
  * In addition, boolean keywords now ignore surrounding whitespace.
* Optional parameters without default arguments now no longer define variables in the function body when the user doesn't pass a value to them; as a result, access to these arguments is now fallible.
* Made runtime error messages more consistent

### Removed
* Removed `Rant.debug_mode` in favor of using `Rant.options()` to get the value instead
* Removed `Rant.run_into_string()` and `Rant.run_into_string_with()` because they're kind of pointless

### Fixes
* Fixed panic that occurred with some variadic function calls

## 4.0.0-alpha.21

### New
* Added new number format system: `alpha`
* Added "depth operator" to determine originating scope of a variable
* Trailing semicolons are now allowed in list initializers

### Changes
* `[seg]` now operates internally on grapheme boundaries instead of `char` boundaries
* Renamed `[is-any]` to `[is-some]`
* CLI now uses `clap` instead of `argh` to parse cmdline options

### Fixes
* Fixed `[cat]` printing to the wrong scope level
* Fixed panic when setting number format with no output
* Fixed bug where `@return`-ing from a module scope would unwind the call stack too far
* Fixed `[require]` not assigning module constants when retrieving from cache

## 4.0.0-alpha.20

### New
* Added number formatter
  * Currently supports these numeral systems:
    * Arabic (Western + Eastern)
    * Persian
    * Babylonian cuneiform (base 60)
    * Roman (upper/lower)
    * Hexadecimal (upper/lower)
    * Octal
    * Binary
* Added stdlib functions:
  * `[num-fmt: options?; depth?]`
  * `[num-fmt-system: system?; depth?]`
  * `[num-fmt-padding: padding?; depth]`
  * `[num-fmt-precision: precision?; depth?]`
  * `[num-fmt-sign: sign-mode?; depth?]`
  * `[num-fmt-alt: enable?; depth?]`
  * `[num-fmt-upper: enable?; depth?]`
  * `[num-fmt-infinity: infinity?; depth?]`
  * `[num-fmt-endian: endianness?; depth?]`
* Added stdlib constants:
  * Float constants: `INFINITY`, `NEG_INFINITY`, `NAN`

### Changes
* Renamed `[else-if]` to `[elseif]`
* Removed unnecessary `unsafe` usage from string formatter for `function` type
* Renamed module `rant::random` to `rant::rng`
* E notation is now accepted on float literals

## 4.0.0-alpha.19

### New
* Add `[rev]` support to the `block` type
* Add slice support to `block` type

### Changes
* Encapsulate `RantRange` fields to prevent unwanted mutation
* Rename `ToRant` trait to `IntoRant` in order to better reflect its behavior

### Fixes
* (internal) Fix `flag` parameter on `rant::runtime::VM::pre_push_block()` not being used
* Fixed nested function lookups accidentally considering their container as a function
* Fixed incorrect const redef error on nested function definitions against a constant collection
* Fixed panic when block with no elements is resolved
* Fixed bad slicing behavior on ranges

## 4.0.0-alpha.18

### New
* Added "charms": right-associative operators for control flow
* Added block weights using the `@weight` charm
* `return`, `continue`, and `break` are now reimplemented as charms

### Removed
* Removed from stdlib: `[return]`, `[continue]`, `[break]`

## 4.0.0-alpha.17

### New
* Added `range` type
* Standard Library items:
  * `[irange]`: Create a new inclusive range
  * `[range]`: Create a new exclusive range
  * `[rev]`: Create a reversed copy of an ordered collection (list, string, range)

### Changes
* Renamed old `[list]` function to `[collect]`
* Renamed `[concat]` to `[cat]`
* Added new `[list]` function which converts a single argument to a Rant list 
* Temporal spreading now works on any indexable type (`list`, `string`, `range`)
* String representations of collections actually show the contents now (up to 4 layers deep)
* Upgraded library dependencies:
  * `logos` &rarr; 0.12.0
  * `rand` &rarr; 0.8.3
* Upgraded dev dependencies:
  * `assert_matches` &rarr; 1.5.0

### Fixes
* Fixed stdlib functions `[frac]`, `[floor]`, `[ceil]` not being registered (oops.)
* Fixed temporal spreads not handling non-list collections properly

## 4.0.0-alpha.16

### New
* Added temporal argument spreading
* Standard Library items:
  * `[concat]`: Print a series of arguments
  * `[list]`: Create a list from a set of arguments
  * `[nlist]`: Create a list from a set of arguments and nest it inside another list

### Changes
* Changed pipe operator from `&` to `|>`
* Changed spread operator from `+` to `*`
* Changed defer operator from `*` to `<>`
* Adjusted whitespace behavior around blocks to avoid ambiguities
* Made separator parameter on `[join]` optional

### Fixes
* Fixed incorrect whitespace print behavior around unflagged function calls

## 4.0.0-alpha.15

### New
* Added support for constants
* Added "pipe" attribute to allow custom block element handling
* Print semantics for collection auto-concatenation
* Compiler now emits warnings for unused variables
* Attribute frame stack now has a max size of 255
* `[sel]` can now additionally accept selector mode strings directly to create + apply a transient selector
* API features:
  * `Rant::set_global_const()`
  * `Rant::set_global_force()`
  * `lang::AccessPath::is_variable()`
  * `lang::Rst::ConstDef`
  * `compiler::message::CompilerMessage.is_warning()`
* CLI: added `-W` switch to disable warnings
* Standard Library:
  * `[pipe]`: Sets the pipe attribute.

### Changes
* Updated some AST structures to support constants
* The following items are now made constant:
  * Standard library items
  * Program arguments
  * Function arguments
  * Imported modules
* API changes:
  * Renamed `lang::AccessPath::capture_var_name()` to `var_name()`
* Upgraded library dependencies:
  * `once_cell` &rarr; 0.5.2
  * `quickscope` &rarr; 0.1.6
  * `rand` &rarr; 0.8.2
  * `rand-xoshiro` &rarr; 0.6.0
  * `smallvec` &rarr; 1.6.1
  * `smartstring` &rarr; 0.2.6
  * `unicode-segmentation` &rarr; 1.7.1
* Stamdard library:
  * Swapped the positions of the two parameters in `[join]` so the separator comes last
* Upgraded CLI dependencies:
  * `argh` &rarr; 0.1.4

### Fixes
* Fixed globals created within program scope not being tracked by compiler
* Fixed `[whitespace-fmt]` modifying the wrong stack frame

## 4.0.0-alpha.14

### New
* Add support for spread notation in function calls
* Add support for slice notation in getters and setters
* Added `BUILD_VERSION` constant to stdlib
* Added new stdlib functions:
  * `[try]`: execute a protected call on a function or block with optional error handling callback

### Changes
* Make some unnecessarily public runtime/compiler APIs crate-visible
* Strings are now measured by grapheme clusters instead of bytes; this affects indexing + slicing behavior

### Fixes
* Fixed some unintended module re-exports
* Fixed modules imported with `[require]` not living long enough
* Fixed panic when trying to return from main program scope

## 4.0.0-alpha.13

### Changes
* Renamed `ErrorKind` to `CompilerErrorKind` in `rant::compiler`
* Exposed the `rant::runtime` module and several types inside
  * More runtime types will be exposed over time as they are documented. 

### Fixes
* Fixed several stdlib functions not being registered

## 4.0.0-alpha.12 (yanked)

### New
* Added `RuntimeErrorType::AssertError`
* Added `ValueError::Overflow`
* Added new stdlib functions:
  * `[abs]`: calculate the absolute value of a number
  * `[acos]`: calculate the arccosine of a number
  * `[asin]`: calculate the arcsine of a number
  * `[assert-eq]`: raise an error if two values are not equal
  * `[assert-neq]`: raise an error if two values are equal
  * `[assert]`: raise an error if a condition is false
  * `[atan]`: calculate the arctangent of a number
  * `[atan2]`: calculate the four-quadrant arctangent of a number
  * `[cos]`: calculate the cosine of an angle
  * `[has]`: check if a collection contains a specific value
  * `[index-of]`: return the index of the first occurrence of a value in a list, or `~` if it's not found
  * `[is]`: check if a value is a specific type
  * `[last-index-of]`: return the index of the last occurrence of a value in a list, or `~` if it's not found
  * `[pow]`: raise x to y power
  * `[sin]`: calculate the sine of an angle
  * `[sqrt]`: calculate the square root of a number
  * `[tan]`: calculate the tangent of an angle

### Changes
* Renamed Rant's `integer` type to `int`
* Renamed stdlib functions:
  * `[is-integer]` &rarr; `[is-int]`
* Changed `[min]` and `[max]` to accept varargs+; lists are replaced with their min/max values respectively

### Removed
  * Removed `[has-key]` from stdlib

## 4.0.0-alpha.11

### New
* Getters can now accept an optional fallback value
* Added function call piping
* Added new stdlib functions:
  * `[clamp]`: clamp a value between two others
  * `[is-any]`: check if a value is not empty
  * `[is-between]`: check if a value is between two others (inclusive)
  * `[zip]`: combine two lists into a new list using a function

### Changes
* Empty value token `<>` changed to `~`
* CLI now trims trailing whitespace from REPL input

## 4.0.0-alpha.10

### New
* Added inverse indices in accessors
* Added anonymous getters/setters
* Added "function percolation": function calls will now descope until the requested variable provides a function or other callable value

### Changes
* Compiler now consolidates adjacent fragments/escape sequences into single AST node
* Updated `quickscope` to 0.1.5

### Fixes
* Fixed dynamically-keyed setters pushing values to stack in wrong order 

### Removed
* Removed `[get]` from stdlib

## 4.0.0-alpha.9

### New
* Added `RuntimeErrorType::TypeError`
* Added `Rant.run_with()`, `Rant.run_into_string_with()`
* Added new stdlib functions:
  * `[filter]`: make a copy of a list containing only items that pass a predicate
  * `[map]`: apply a function to items in a list and return another list with the results
  * `[oxford-join]`: print a list with custom separators for applying series commas and conjunctions
  * `[rand-list]`: generate a list of random integers
  * `[randf-list]`: generate a list of random floats
  * `[translate]`: map a list's values to the values in a map

### Fixes
* Fixed `[sifted]` and `[squished]` returning `<>` when input list meets target size
* Fixed sinks not being honored on native function calls

## 4.0.0-alpha.8

### New
* Added `RantProgramInfo` struct
* Added `[require]` now searches the containing directory of the running program before the local modules path
* Added new stdlib functions:
  * `[alpha]`: print a string of random alphanumeric characters
  * `[assoc]`: create a map from a list of keys and a list of values
  * `[ceil]`: get the smallest integer greater than or equal to a number
  * `[floor]`: get the largest integer less than or equal to a number
  * `[frac]`: get the fractional part of a float value
  * `[sift]`: remove random items from a list down to a target size
  * `[sifted]`: create a random, ordered subset of a list
  * `[squish]`: merge random adjacent elements in-place in a list using addition until a target size is reached
  * `[squished]`: make a copy of a list with random adjacent elements merged down to a target size

### Changes
* `[require]` now accepts a relative path, as long as it is contained in the modules directory
* `RantProgram.name()` now returns `Option<&str>` instead of `&str`
* Renamed `RantOptions.local_module_path` to `RantOptions.local_modules_path`
* Renamed stdlib functions `[num]` and `[numf]` to `[rand]` and `[randf]`, respectively

### Fixes
* Fixed function calls not triggering variable capture pass
* Fixed def + get/set of same variable in chained accessor causing erroneous variable capture inside functions

## 4.0.0-alpha.7

### New
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

### Changes
* Main program body can now be returned from with `[return]`
* Upgraded `smartstring` to 0.2.5

## 4.0.0-alpha.6

### New
* Modules are now cached by the executing context in a global map
* Added support for variable capturing in closures
* Added `.delete_global()`, `.has_global()`, `.delete_global()`, `.global_names()` methods to `Rant`

### Removed
* Removed `.globals()` method from `Rant`

### Changes
* Upgraded `quickscope` to 0.1.3

## 4.0.0-alpha.5

### New
* Added `Rant::with_random_seed()` constructor
* Added `.compile_named()` and `.compile_quiet_named()` methods to `Rant`
* Added support for multi-part accessors
* Added support for modules
* Added new stdlib functions:
  * `[require]`
  * `[error]`

### Removed
* Removed `RantProgram::with_name()` method

### Changes
* Replaced `RuntimeErrorType::GeneralError` with `RuntimeErrorType::UserError` 
* Upgraded `quickscope` to 0.1.2
* `[sep]` now supports `block` values and will auto-resolve them for each separator
* Improved stack trace formatting
  * Error messages are categorized and have improved readability
  * Consecutive duplicate stack frames are now combined

## 4.0.0-alpha.4

### New
* Added descope and explicit global modifiers to accessors
* Added implicit local variable scope to program root
* Added support for whitespace normalization via `[whitespace-fmt]`.
* Added new stdlib functions:
  * `[insert]`
  * `[remove]`
  * `[take]`
  * `[whitespace-fmt]`

### Changes

* Runtime now uses `quickscope` crate for variable access
* Changed `Rant::run()` to return a `RantValue` and added `Rant::run_into_string()` to explicitly return a `String` output
  * `[dignz]`
* Renamed stdlib functions:
  * `[dec]` -> `[dig]`
  * `[hex]` -> `[digh]`
* Upgraded `smartstring` to version 0.2.4

### Removed
* Removed `_GLOBALS` from stdlib

## 4.0.0-alpha.3

### New
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
* CLI benchmarks are now hidden by default; enable with `--bench-mode` / `-b`.

### Changes
* Renamed `[n]` to `[num]`
* Renamed `[nf]` to `[numf]`

### Fixes
* Fixed compiler bug where non-printing whitespace was sometimes printed.
* Fixed bug where printing empties with a non-empty would cause output to be coerced incorrectly to a string.
* Fixed bug where sinked blocks print when they're not supposed to.

## 4.0.0-alpha.2

### New
* Added support for some block attributes: conditional value, repetitions, separator
* Added a lot of new stdlib functions
* Added `special` Rant type to represent opaque internal runtime data.
* Added `get_global()` and `set_global()` to `Rant`.
* Added new `Rant::with_options` constructor that lets you customize the context using new `RantOptions` struct.
* Added `RantValueType` to represent the type of a value.

### Changes
* Replaced empty `Err` variant on `Rant::compile*` methods with new `compiler::ErrorKind` enum.
* Changed `IndexError` and `KeyError` to use `RantValueType` instead of `&'static str` to represent types.
* Renamed `get_by_index`, `set_by_index`, `get_by_key`, and `set_by_key` methods in `RantValue` to `index_get`, `index_set`, `key_get`, and `key_set` respectively.
* Expanded type aliases on `Rant::compile*` method return types to make them less confusing.
* Hid some modules that don't need to be user-facing.

## 4.0.0-alpha.1

* Initial alpha release.