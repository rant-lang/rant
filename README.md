<p align="center">
<img src="https://i.imgur.com/5IV9vwx.png" alt="Rant Logo" height="225px" width="225px"></img>
</p>

[![Crates.io](https://img.shields.io/crates/v/rant)](https://crates.io/crates/rant)
[![Docs.rs](https://docs.rs/rant/badge.svg)](https://docs.rs/rant)
[![Discord](https://img.shields.io/discord/332251452334669834?color=6C8BD5&label=discord&logo=discord&logoColor=%23fff)](https://discord.gg/5n7bnAD)



**Rant** is a high-level language for **procedural templating**, providing a powerful toolset for easily creating code templates, dynamic game dialogue, stories, name generators, test data, and much more.

***

## WARNING:

**This project is in alpha. Expect to see broken/missing features.**

**While I encourage you to try it out and give feedback, please do not use in production environments until a stable version is released.**

**Features may appear or disappear at any time for any reason. Assume that every alpha release will be breaking.**

***

## Introducing Rant 4

Rant 4 is a complete redesign and reimplementation from the ground up, written in Rust. 
It's designed around the needs of everyone-- programmers, writers, and everyone in-between!

### Intuitive syntax

Rant is basically the opposite of [Regex](https://en.wikipedia.org/wiki/Regular_expression): 
just as a regular expression compares inputs to a pattern, Rant generates matching outputs from a pattern!

Consider this regex:

```regex
# Matching inputs:
#  - "Hey world!"
#  - "Hi world!"
#  - "Hello world!"

(Hey|Hi|Hello) world!
```

The Rant equivalent is nearly identical:

```rant
{Hey|Hi|Hello} world!

# Possible outputs:
#  - "Hey world!"
#  - "Hi world!"
#  - "Hello world!"
```

## Features

* **Pain-Free API:** Rant's API is designed to be as straightforward as possible. Integrating Rant into your project only takes a few lines of code!
* **Rant Standard Library:** Rant's comprehensive standard library lets you get more done and iterate rapidly with far less boilerplate than other languages.
* **Branching on Steroids:** Diversify your output with a multitude of branch selection modes, including several built-in randomization modes.
* **Dynamic Variable System:** Inspired by other popular languages, Rant's built-in primitive and collection types let you represent whatever data you need.
* **Automatic Formatting:** Passively format text output with automatic capitalization, whitespace normalization, and number formatting.
* **Entanglement:** Synchronize RNG behavior in different parts of your program with a single function call.
* **Modules:** Create libraries of Rant functions to easily share between your programs.
* **Fully Cross-Platform:** Rant code is fully portable. Write once and run on any supported platform!

## Getting started

### CLI

Rant comes with a CLI tool that includes a REPL. You can install and run it with:

```sh
$ cargo install rant --version=4.0.0-alpha.14 --features=cli
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
