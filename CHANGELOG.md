# Changelog

## 4.0.0-alpha.33

(5/20/2022)

### New

### Changes
* All types are now allowed in block weights. Non-numeric types are converted to integers based on their truthiness.

### Fixes
* Fixed build failure on installation due to missing files


## 4.0.0-alpha.32

(2/17/2022)

### New
* Added stdlib functions:
  * `[char]`
  * `[ord]`
  * `[ord-all]`

### Changes
* Changed `@and` operator to a symbol: `&`
* Changed `@or` operator to a symbol: `|` (must use within parentheses in block element context)
* Changed `@xor` operator to a symbol: `^`
* Compound assignment operator symbols are now monomorphic -- meaning you can't insert whitespace before the `=` anymore.
* Allow hints/sinks in more places

### Fixes
* Fixed parser bug caused by `/=` operator
* Fixed trailing newline on REPL input

### Removed
* Removed `@nand` and `@nor`

## 4.0.0-alpha.31

(2/12/2022)

### New
* Added stdlib functions:
  * `[fill-self]`
  * `[fill-thru]`
  * `[pickn]`
  * `[sel-skip]`
  * `[sel-freeze]`
  * `[sel-frozen]`
  * `[trim]`
* CLI: Added `[credits]` function
* CLI: Program-global variables now persist between lines in the REPL
* CLI: Added functions to display copyright notice & credits
* Closures can now be used to create Rant functions
* Added compound assignment support for all arithmetic and logic operators

### Changes
* Renamed type `special` to `selector` because it's only used for selectors
* Renamed type `empty` to `nothing` to make its meaning clearer and avoid API confusion
* Renamed stdlib functions:
  * `[is-empty]` &rarr; `[is-nothing]`
* Changed behavior of `[sel]` so that it now prints the current selector when no arguments are passed
* Updated the Rust API to reflect the selector type changes:
  * Renamed `Selector` to `RantSelector` for consistency.
  * `RantSelector` has its own handle type, `RantSelectorHandle`.
  * `RantSelectorHandle` can be directly accepted by native functions. 
* Exposed some read-only fields on `RantSelector` through public methods

### Fixes
* CLI: Fixed accidental REPL activation when launched with redirected stdin

## 4.0.0-alpha.30

(2/5/2022)

### New
* Added three new Unicode escape formats
  * 16-bit: `\uXXXX`
  * 32-bit: `\UXXXXXXXX`
  * Unsized (max 1-8 digits): `\U(XXXXXXXX)`
* Added stdlib functions
  * `[ds-query-sources]`

### Changes
* The escape sequence parser is now more strict, and will create a compiler error if you escape non-reserved characters
* Renamed stdlib functions:
  * `[data]` &rarr; `[ds-request]`

### Fixes
* Fixed `RantValue::concat()` not supporting maps
* Fixed field visibility on `ModuleResolveError`

## 4.0.0-alpha.29

(1/31/2022)

### New
* Module resolver has been externalized into a separate `ModuleResolver` trait, which you can use to write your own custom resolution logic.
  * 2 impls included: `DefaultModuleResolver` (default), `NoModuleResolver` (to disable modules completely)


### Changes
* `[sum]` now accepts any ordered collection type
* Changed behavior of `RantValue::is_empty()` to match that of `Vec`
* Changed slice bound separator from `:` to `..`
* Removed need for `!` in anonymous accessors and function calls; just use an access path!
* Renamed `CompilerErrorKind` to `CompilerError`
* Moved `RantOptions::enable_global_modules` and `RantOptions::local_modules_path` to `DefaultModuleResolver`
* `VarArgs<T>` and `RequiredVarArgs<T>` are now public. Use them to add variadic parameters to your native functions!
* Renamed stdlib functions:
  * `[nop]` &rarr; `[tap]`

### Removed
* Removed ability to specify dynamic variable names in accessors - expressions in the first path component now act as anonymous value sources
* `RantOptions::enable_require`

### Fixes
* Fixed parsing bug in function call accessors where a dynamic child component followed by arg list start (`:`) was incorrectly parsed as a slice
* Fixed incorrect behavior in `[shuffle-thru]`
* Fixed incorrect infix operator associativity

## 4.0.0-alpha.28

(1/29/2022)

### New
* Added complex spread; add a parametric spread to each iteration of a temporal argument with `***` or `*a**`
* Added assignment pipe; assign pipeval to a new or existing variable at the end of a piped call
* Added infallible `IntoRant` and `FromRant` traits
* Added stdlib functions:
  * `[pick-sparse]`
  * `[string-replace]`
  * `[tuple]`

### Changes
* Dynamic keys are now wrapped in `()` instead of `{}`
* Renamed stdlib functions:
  * `[collect]` &rarr; `[list]`
* Upgraded library dependencies:
  * `quickscope` &rarr; 0.2.0

### Removed
* Removed depth operator

### Fixes
* Fixed runtime error when attempting to use pipeval in a pipecall path
* Fixed improper tokenization of labeled temporal spread operators
* Fixed typo in `[to-tuple]` registration

## 4.0.0-alpha.27

(1/26/2022)

### New
* Added the `tuple` type, along with tuple initialization syntax:
  * `()`, `(A;)`, `(A; B)`, `(A; B; C)`, ...
* Added stdlib functions:
  * `[squish-thru]`
  * `[to-tuple]`
* Added several Rust API functions to support conversion to and from tuples and other collection types

### Changes
* Renamed `FromRant` to `TryFromRant` (and adjusted member names)
* Renamed `IntoRant` to `TryIntoRant` (and adjusted member names)
* Renamed `lang::Rst` to `lang::Expression` to better reflect its purpose
* Parametric spread is now enabled for all indexable collection types; not just lists
* Changed the reference handle types for all collection types from type aliases to structs (and much refactoring was had)
* Completely changed all collection initializer syntax:
  * List: `(:)`, `(: A)`, `(: A; B)`, ...
  * Map: `(::)`, `(:: A=1)`, `(:: A=1; B=2)`, ...
* Renamed stdlib functions:
  * `[whitespace-fmt]` &rarr; `[ws-fmt]`
  * `[squish]` &rarr; `[squish-self]`
  * `[squished]` &rarr; `[squish]`

### Fixes
* Fixed `[augment]` accidentally mutating the original instance of the destination map
* Fixed `[to-list]` not working on tuples

## 4.0.0-alpha.26

(1/25/2022)

### New
* Added map initializer shorthands; specify both a key and value with a single variable getter
* Added stdlib functions:
  * `[augment]`
  * `[augment-self]`
  * `[augment-thru]`
  * `[sort-thru]`
  * `[sift-thru]`
  * `[shuffle-thru]`

### Changes
* Hints/sinks on string literals are allowed
* Renamed stdlib functions:
  * `[sort]` &rarr; `[sort-self]`
  * `[sorted]` &rarr; `[sort]`
  * `[sift]` &rarr; `[sift-self]`
  * `[sifted]` &rarr; `[sift]`
  * `[shuffle]` &rarr; `[shuffle-self]`
  * `[shuffled]` &rarr; `[shuffle]`

### Removed
* Removed inner-protected blocks
* Removed stdlib functions:
  * `[push-attrs]`
  * `[pop-attrs]`
  * `[count-attrs]`

### Fixes
* Fixed hints/sinks being rejected on accessors
* Fixed key setters accidentally tripping constant reassignment error

## 4.0.0-alpha.25

(1/23/2022)

### New
* Added compiler errors when int/float literals are out of range
* Added `@edit`: apply recursive functions to your output, iteratively!
* Pipeval (`[]`) is now supported in access paths without needing to nest it in a dynamic key
* Added protected blocks

### Changes
* Renamed stdlib functions:
  * `[pipe]` &rarr; `[mut]`

### Fixes
* Fixed pipeval being unhintable
* Fixed fallible optional parameters not generating warnings when unused
* Fixed minimum int value not being parsed correctly
* Fixed conditional bodies not working like standard blocks


## 4.0.0-alpha.24

(1/17/2022)

### New
* Added `@require` statements. These take *static* module paths. You can still use `[require]` if you need to import modules dynamically.
* Added conditional branching with `@if`, `@elseif`, `@else`
* Added ability to auto-hint variables, params, and function calls using the `@text` keyword
* Added a ton of arithmetic, logic, and comparison operators, including many C-style infix operators with precedence support 
  * Addition: `+`
  * Subtraction: `-`
  * Multiplication: `*`
  * Division: `/`
  * Modulo: `%`
  * Exponentiation: `**`
  * Negation: `@neg` (`-` is still valid for negating constants, e.g. `-123`)
  * Logical AND: `@and`
  * Logical OR: `@or`
  * Logical NOT: `@not`
  * Logical NAND: `@nand`
  * Logical NOR: `@nor`
  * Logical XOR: `@xor`
  * Equality: `@eq`
  * Inequality: `@neq`
  * Less than: `@lt`
  * Less than or equal: `@le`
  * Greater than: `@gt`
  * Greater than or equal to: `@ge`
- Added stdlib functions:
  - `[assert-not]`

### Changes
* Anonymous accessor values now must be surrounded by a block.
* Rant is now relicensed under MIT and Apache 2.0!
* Update `FromRant` impl for `bool` to coerce from any RantValue type according to truthiness rules
* Renamed stdlib functions:
  * `[shred]` &rarr; `[rand-list-sum]`

### Removed
* Removed inline lambda bodies. All lambda bodies now require braces.

### Fixes
* Fixed broken `[chunks]` behavior


## 4.0.0-alpha.23

(1/11/2022)

### New
* Added stdlib functions:
  * `[print]`
  * `[chunks]`
  * `[to-bool]`
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
* Changed behavior of sinks
  * Instead of suppressing output, sinking an element does the opposite of hinting: it informs the compiler that the sinked element should be treated as _not text_, causing surrounding whitespace to be ignored during compilation.
* Upgraded library dependencies:
  * `cast` &rarr; 0.3.0
* Upgraded CLI dependencies:
  * `ctrlc` &rarr; 3.1.9
* Renamed stdlib functions:
  * `[string]` &rarr; `[to-string]`
  * `[int]` &rarr; `[to-int]`
  * `[float]` &rarr; `[to-float]`
  * `[list]` &rarr; `[to-list]`
* Move R1005 (fallible optional param access) to R0028 and upgrade it from a warning to an error

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

(4/13/2021)

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

(4/10/2021)

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

(4/3/2021)

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

(2/16/2021)

### New
* Added "charms": right-associative operators for control flow
* Added block weights using the `@weight` charm
* `return`, `continue`, and `break` are now reimplemented as charms

### Removed
* Removed from stdlib: `[return]`, `[continue]`, `[break]`

## 4.0.0-alpha.17

(2/7/2021)

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

(1/26/2021)

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

(1/15/2021)

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

(1/9/2021)

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

(10/19/2020)

### Changes
* Renamed `ErrorKind` to `CompilerErrorKind` in `rant::compiler`
* Exposed the `rant::runtime` module and several types inside
  * More runtime types will be exposed over time as they are documented. 

### Fixes
* Fixed several stdlib functions not being registered

## 4.0.0-alpha.12 (yanked)

(10/19/2020)

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

(10/14/2020)

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

(10/11/2020)

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

(10/10/2020)

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

(10/7/2020)

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

(10/1/2020)

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

(9/22/2020)

### New
* Modules are now cached by the executing context in a global map
* Added support for variable capturing in closures
* Added `.delete_global()`, `.has_global()`, `.delete_global()`, `.global_names()` methods to `Rant`

### Removed
* Removed `.globals()` method from `Rant`

### Changes
* Upgraded `quickscope` to 0.1.3

## 4.0.0-alpha.5

(9/21/2020)

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

(9/12/2020)

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

(8/29/2020)

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

(8/26/2020)

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

(8/24/2020)

* Initial alpha release.