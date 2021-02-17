<p align="center">
<img src="https://i.imgur.com/5IV9vwx.png" alt="Rant Logo" height="225px" width="225px"></img>
</p>

[![Crates.io](https://img.shields.io/crates/v/rant)](https://crates.io/crates/rant)
[![Docs.rs](https://docs.rs/rant/badge.svg)](https://docs.rs/rant)
[![Discord](https://img.shields.io/discord/332251452334669834?color=6C8BD5&label=discord&logo=discord&logoColor=%23fff)](https://discord.gg/U8Bj6gSshJ)



**Rant** is a high-level procedural templating language with a diverse toolset for easily creating dynamic code templates, game dialogue, stories, name generators, test data, and much more.

***

## NOTICE:

**This project is in alpha. Expect to see broken/missing features.**

**While I encourage you to try it out and give feedback, please don't use in production environments until a stable version is released.**

**Features may appear or disappear at any time for any reason. Assume that every alpha release is breaking.**

***

## Introducing Rant 4

Rant 4 is a complete redesign and reimplementation of the Rant language, written in Rust. 
It's designed around the needs of everyone-- programmers, writers, and everyone in-between!


Rant is basically the opposite of [Regex](https://en.wikipedia.org/wiki/Regular_expression): 
just as a regular expression compares inputs to a pattern, Rant generates matching outputs from a pattern!

## Why Rant?

🧰 **Painless API** <br/> 
Rant's API is designed to be as straightforward to use as possible. Integrating Rant into your project only takes a few lines of code.

💻 **Cross-Platform** <br/> 
Write once, run anywhere-- Rant code is fully portable across all supported platforms.

🎲 **Built with ♥ for RNG** <br/>
Throw the dice with a wide array of powerful, built-in utilities for generating random numbers, strings, booleans, lists, list subsets, and much more. Synchronize RNG behavior in different parts of your program with a single function call.

The built-in RNG can be externally seeded to produce deterministic program outputs.

🔱 **Advanced Branching** <br/> 
Fine-tune branching behavior with a multitude of configuration options and branch selection modes for iterative, random, and weighted selection.

🎨 **Dynamic Variable System** <br/> 
Inspired by popular scripting languages, Rant's dynamic variable system lets you work with common primitives, collection types, callbacks, and more without any fuss.

🖨 **Print Semantics** <br/> 
Rant is all about "printing": each lexical scope has an output that can be printed to, which then prints itself to the parent output, and so on. This lets you easily build strings and collections from multiple parts with minimal code bloat.

🧬 **Delightful Combinatorics** <br/> 
Effortlessly perform nested mappings, filters, zips, combinations, and more with Rant's intuitive function composition and iteration engine.

🎛 **Automatic Formatting** <br/> 
Passively format text output with automatic capitalization, whitespace normalization, and number formatting.

🧩 **Modules** <br/> 
Create libraries of Rant functions and share them between your programs using Rant's simple module system.

📚 **Rant Standard Library** <br/> 
Rant's comprehensive standard library lets you get more done and iterate rapidly with far less boilerplate than other languages.

## Language 

## Getting started

### CLI

Rant's CLI can run Rant code from files or the command line.
Install it from Cargo with:

```sh
$ cargo install rant --version 4.0.0-alpha.19 --features cli
```

Then run it:

```sh
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

## [Examples](./examples/rant/)

This repository contains a directory of example programs written in Rant for you to learn from. Check them out!

## Documentation

The latest reference documentation can be found at **[docs.rant-lang.org](https://docs.rant-lang.org)**.

Since Rant 4 is early in development, some documentation may be outdated/incomplete, but it is actively updated to ensure that it reflects current features with reasonable accuracy.

## [Changelog](https://github.com/rant-lang/rant/blob/master/CHANGELOG.md)

The changelog is updated constantly throughout the development process, providing a complete summary of upcoming changes at a glance even before the next release.

## License

Rant is distributed under the GNU Affero General Public License v3. See [LICENSE](./LICENSE) for more details.

An alternative, commercial license is planned for the future.
