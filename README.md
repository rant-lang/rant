<p align="center">
<img src="https://i.imgur.com/5IV9vwx.png" alt="Rant Logo" height="225px" width="225px"></img>
</p>

[![Crates.io](https://img.shields.io/crates/v/rant)](https://crates.io/crates/rant)
[![Docs.rs](https://docs.rs/rant/badge.svg)](https://docs.rs/rant)
[![Discord](https://img.shields.io/discord/332251452334669834?color=6C8BD5&label=discord&logo=discord&logoColor=%23fff)](https://discord.gg/5n7bnAD)



**Rant** is a high-level language for templating and procedural generation.
Easily create dynamic templates, dialogue, stories, names, test data, and much more.

***

## WARNING:

**This project is in alpha. Expect to see broken/missing features.**

**While I encourage you to try it out and give feedback, please do not use in production environments until a stable version is released.**

**Features may appear or disappear at any time for any reason. Assume that every alpha release will be breaking.**

***

## Introducing Rant 4

Rant 4 is a complete remake of the Rant language and runtime. It delivers a vastly improved user experience designed around the needs of everyone-- from writers and programmers, to anybody else!

Think of Rant as the opposite of Regex: 
just as a regular expression compares inputs to a pattern, Rant generates matching outputs from a pattern!

Consider this regex:

```regex
(Hey|Hi|Hello) world!
```

The Rant equivalent is nearly identical:

```rant
{Hey|Hi|Hello} world!

##
Possible outputs:
 * "Hey world!"
 * "Hi world!"
 * "Hello world!"
##
```

## Features

* **Pain-free API:** Rant's API is designed to be as straightforward as possible. Get started with just a few lines of code.
* **Dynamic variable system:** Represent any kind of data using Rant's built-in primitive and collection types.
* **Branching:** Diversify your output with a multitude of branch selection modes, both random and otherwise.
* **Automatic formatting:** Passively format your output with automatic capitalization, whitespace normalization, and number formatting.
* **Rant Standard Library:** Leverage Rant's comprehensive standard library to get more done with less code. 
* **Modules:** Create libraries of Rant functions to easily share between your programs.
* **Entanglement:** Synchronize different parts of your programs using built-in RNG synchronization utilities.
* **Portability:** Rant code is fully portable. Write once and run on any supported platform!

## Getting started

### CLI

Rant comes with a CLI tool that includes a REPL. You can install and run it with:

```sh
$ cargo install rant --version=4.0.0-alpha.12 --all-features
$ rant
```

### Library

Add Rant to your `Cargo.toml`:

```toml
[dependencies]
rant = "*"
```

You can run a Rant program with just a few lines of code:

```rust
use rant::Rant;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
  // Create a default Rant context
  let mut rant = Rant::new();

  // Compile a simple program
  let program = rant.compile_quiet(r#"
  [$greet:name] {
    {Hello|Hi|Hey} <name>!
  }
  [greet:world]
  "#)?;

  // Run the program and print the output
  let output = rant.run(&program)?;
  println!("{}", output);

  Ok(())
}
```

## Documentation

The latest reference documentation can be found at **[docs.rant-lang.org](https://docs.rant-lang.org)**.

Since Rant 4 is early in development, some documentation may be outdated/incomplete, but it is actively updated to ensure that it reflects current features with reasonable accuracy.

## [Changelog](https://github.com/rant-lang/rant/blob/master/CHANGELOG.md)

The changelog is updated constantly throughout the development process, providing a complete summary of upcoming changes at a glance even before the next release.

## License

Rant is distributed under the GNU Affero General Public License v3. See [LICENSE](./LICENSE) for more details.

An alternative, commercial license is planned for the future.
