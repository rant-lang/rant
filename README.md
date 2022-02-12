<p align="center">
<img src="https://i.imgur.com/s9jMDhI.png" alt="Rant Logo" ></img>
</p>

[![Crates.io](https://img.shields.io/crates/v/rant)](https://crates.io/crates/rant)
[![Docs.rs](https://docs.rs/rant/badge.svg)](https://docs.rs/rant)
[![Discord](https://img.shields.io/discord/332251452334669834?color=6C8BD5&label=discord&logo=discord&logoColor=%23fff)](https://discord.gg/U8Bj6gSshJ)

**Rant** is a dynamically-typed, multi-paradigm templating language designed primarily for procedural generation. It is designed with scalability in mind: it can handle tasks ranging from simple randomized string generation to more complex workloads such as procedural dialogue, character generation, and worldbuilding.

***

> ## **Prerelease notice**
>
> **This project is in alpha.**
> **This means that the API is unstable, functionality may be broken/missing, and everything is subject to change.**
>
> **Please dont hesitate to try it out and give feedback; however, _do not_ use in production environments until a stable version is released.**
>
> **Features may appear/disappear at any time for any reason. Assume that every alpha release will have breaking changes.**


## Introducing Rant 4

Rant is the result of a long-standing desire for an all-in-one data templating tool made especially for creative applications like games and interactive art.

Rant 4 is the next leap forward in achieving this goal: the syntax, standard library, and interpreter have all been completely reimagined from the ground up.

## Features

🧰 **Painless API** <br/> 
Rant has a no-nonsense API designed for ease of use.
No getting lost in configuration hell. Integrating Rant into your project only takes a few lines of code.

💻 **Cross-platform** <br/> 
Write once, run anywhere! The runtime works the same across Windows, Mac, Linux, and WebAssembly.

✍ **Templating that does more** <br/>
Rant is all about "printing": each lexical scope has an output to print (append values) to, which then prints itself to the parent output, and so on.
This enables you to intuitively build strings, collections, and more in a familiar templating setting.

🎨 **Now *intentionally* Turing-complete!** <br/> 
In addition to being a templating language, Rant adopts declarative and imperative programming concepts with design influences from many other popular languages.

✨ **Generate anything &mdash; not just text** <br/>
Unlike older Rant versions that could only generate strings, Rant 4 can output arbitrary data structures using any of the built-in data types. Enjoy first-class support for common primitives like strings, numbers, collections, closures, and more.

🎲 **Built with ♥ for RNG** <br/>
Rant is made with random generation in mind as a major use-case. 

Make use of a wide array of built-in utilities for generating random numbers, strings, booleans, lists, list subsets, and much more for all your randomization needs. 
The internal RNG can be manually seeded to produce repeatable outputs.

🔱 **Branching and beyond** <br/> 
Augment regular control flow behavior with a multitude of configuration options for iterative, randomized, and weighted branch selection.

🧬 **Delightful combinatorics** <br/>
Perform nested mappings, filters, zips, combinations, and more with minimal effort.
Rant's powerful piping syntax lets you perform complex operations with shorter, more readable code.

📝 **Automatic text formatting** <br/>
Passively format text output with automatic capitalization, whitespace normalization, and number formatting &mdash; including built-in support for numerous writing systems.

📦 **Data sources** <br/>
Attach custom data sources to your Rant execution context to give your scripts controlled access to external resources.

🧩 **Simple module system** <br/> 
Sharing code between Rant programs is trivial. Just write your module script and `@require` it elsewhere.

Need custom module resolution logic? No problem. You can write your own resolver and just plug it in.

📚 **Rant Standard Library** <br/> 
A comprehensive standard library provides the tools needed to quickly iterate on your ideas.

🧪 **Use integrated or standalone** <br/>
Whether you want to integrate Rant directly into a product or use it as a standalone tool to assist with writing, Rant has a place in any part of your workflow.

## Getting started

Rant is written in [Rust](https://rust-lang.org), so you'll need to install [the toolchain](https://www.rust-lang.org/tools/install) in order to build it.

### CLI

Rant's CLI can run Rant code from files or the command line.
Install it from Cargo with:

```sh
$ cargo install rant --version 4.0.0-alpha.31 --features cli
```

Then run it:

```sh
# Launch the REPL
$ rant

# Get a full list of options
$ rant -h

# Run an inline script and display output
$ rant -e '[rep:3] [sep:\s] {b{ee|i|o|u}bbity}'

# Run hello_world.rant and send output to result.txt
$ rant hello_world.rant > result.txt
```

### Library

To use Rant in a Rust project, add the `rant` crate to `Cargo.toml`:

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

## [Examples](./examples/)

This repository includes a collection of example Rant scripts for you to learn from. Check them out!

## Documentation

The latest reference documentation can be found at **[docs.rant-lang.org](https://docs.rant-lang.org)**.

Since Rant 4 is early in development, some documentation may be outdated/incomplete, but it is actively updated to ensure that it reflects current features with reasonable accuracy.


## [Changelog](./CHANGELOG.md)

The changelog is updated constantly throughout the development process, providing a complete summary of upcoming changes at a glance even before the next release.

## License

Licensed under either of

* Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
* MIT license
   ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.