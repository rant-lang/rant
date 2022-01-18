<p align="center">
<img src="https://i.imgur.com/s9jMDhI.png" alt="Rant Logo" ></img>
</p>

[![Crates.io](https://img.shields.io/crates/v/rant)](https://crates.io/crates/rant)
[![Docs.rs](https://docs.rs/rant/badge.svg)](https://docs.rs/rant)
[![Discord](https://img.shields.io/discord/332251452334669834?color=6C8BD5&label=discord&logo=discord&logoColor=%23fff)](https://discord.gg/U8Bj6gSshJ)



**Rant** is a high-level procedural templating language with a diverse toolset for easily creating dynamic code templates, game dialogue, stories, name generators, test data, and much more.

***

> ## **Prerelease notice**
>
> **This project is in alpha.**
> **This means that the API is unstable, functionality may be broken/missing, and everything is subject to change.**
>
> **Please do try it out and give feedback; however, _do not_ use in production environments until a stable version is released.**
>
> **Features may appear/disappear at any time for any reason. Assume that every alpha release will have breaking changes.**


## Introducing Rant 4

Rant 4 is a complete redesign and reimplementation of the original Rant language, written in [Rust](https://rust-lang.org).

### More than a text generator

Unlike previous versions, Rant 4 is for more than just text: 
you can now generate arbitrary structured data of any kind with improved and expanded syntax that preserves the ergonomics of its predecessor.

### Built on a decade of experience

Based on 10 years (and counting) of ideas, iteration, and community feedback, Rant 4 is built on a desire for an all-in-one data templating tool made especially for creative applications like games and interactive art.

### Use integrated or standalone

Whether you want to integrate Rant directly into a product or use it as a standalone tool to assist with writing, Rant has a place in any part of your workflow.

## Why Rant?

🧰 **Painless API** <br/> 
Rant has a no-nonsense API designed for ease of use.
No getting lost in configuration hell. Integrating Rant into your project only takes a few lines of code.

💻 **Cross-Platform** <br/> 
Write once, run anywhere! The runtime works on Windows, Mac, Linux, and WebAssembly.

🎲 **Built with ♥ for RNG** <br/>
Rant is made with random generation in mind as a major use-case. 

Make use of a wide array of built-in utilities for generating random numbers, strings, booleans, lists, list subsets, and much more for all your randomization needs. 
The internal RNG can be manually seeded to produce repeatable outputs.

🎨 **It's a Scripting Language** <br/> 
Inspired by other popular scripting languages, Rant is a fully-fledged imperative programming language with a dynamic type system. 
Enjoy first-class support for common primitives like strings, numbers, collections, closures, and more.

🖨 **It's a Templating Language** <br/> 
Rant is all about "printing": each lexical scope has an output to print (append values) to, which then prints itself to the parent output, and so on. 
This enables you to intuitively build strings, collections, and more in a familiar templating setting.

🔱 **Advanced Branching** <br/> 
Fine-tune branching behavior with a multitude of configuration options and branch selection modes for iterative, random, and weighted selection.

🧬 **Delightful Combinatorics** <br/>
Perform nested mappings, filters, zips, combinations, and more with minimal effort.
Rant's powerful piping syntax lets you perform complex operations with shorter, more readable code.

🎛 **Automatic Formatting** <br/>
Passively format text output with automatic capitalization, whitespace normalization, and number formatting &mdash; including built-in support for numerous writing systems.

🧩 **Simple Module System** <br/> 
Creating a library and depending on it in another program should be trivial, and Rant makes it so.
Just create your library script, then import it elsewhere with a single function call.

You can add your own package manager on top if you want, but it's not required.

📚 **Rant Standard Library** <br/> 
A comprehensive standard library provides the tools needed to quickly iterate on your ideas.

## Getting started

### CLI

Rant's CLI can run Rant code from files or the command line.
Install it from Cargo with:

```sh
$ cargo install rant --version 4.0.0-alpha.24 --features cli
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

## MSRV

The minimum supported Rust version is `1.57.0`.

## License

Licensed under either of

* Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or <http://www.apache.org/licenses/LICENSE-2.0>)
* MIT license
   ([LICENSE-MIT](LICENSE-MIT) or <http://opensource.org/licenses/MIT>)

at your option.