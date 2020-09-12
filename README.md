<p align="center">
<img src="https://i.imgur.com/5IV9vwx.png" alt="Rant Logo" height="225px" width="225px"></img>
</p>

[![Crates.io](https://img.shields.io/crates/v/rant)](https://crates.io/crates/rant)
[![Docs.rs](https://docs.rs/rant/badge.svg)](https://docs.rs/rant)
[![Discord](https://img.shields.io/discord/332251452334669834?color=6C8BD5&label=discord&logo=discord&logoColor=%23fff)](https://discord.gg/5n7bnAD)



**Rant** is a high-level language for templating and procedural generation.
Write more expressive and dynamic templates, dialogue, stories, names, test data, and much more.

## WARNING:

**This project is in early alpha. Expect to see broken/missing features.**

**While I encourage you to try it out and give feedback, please do not use in production environments until a stable version is released.**

**Features may appear or disappear at any time for any reason. Assume that every alpha release will be breaking.**

***

## Introducing Rant 4

Rant 4 is a complete remake of the Rant language and runtime. It delivers a vastly improved user experience designed around the needs of everyone-- from writers and programmers, to anybody else!

### Intuitive syntax

Rant is basically the opposite of Regex: 
just as a regular expression compares inputs to a pattern, Rant generates matching outputs from a pattern!

It's easy to express your desired output as a set of branching possibilities.
For example, consider a simple regex that matches on three strings:

```regex
(foo|bar|baz)
```

The Rant equivalent to generate any matching string is quite similar:

```rant
{foo|bar|baz} # Resolves randomly to "foo", "bar", or "baz"
```

### Concise operations

Rant's standard library provides built-in utilities for many common use cases, cutting down on the amount of boilerplate you need to write.

Even for more complex generation tasks, Rant has your back. With its powerful set of synchronization, branching, and generation tools, you can get results with far less code than conventional programming languages.

### Flexible applications

Rant does more than randomization: its behavior is infinitely configurable for a wide range of use cases ranging from natural language generation to simple code templating. What you do with it is up to you!

Need to work with structured data rather than just strings? No problem. Rant offers a fully-featured object model including common primitives like numbers and booleans as well as collection types like maps and lists.

## Getting started

### CLI

Rant comes with a CLI tool that includes a REPL. You can install and run it with:

```sh
$ cargo install rant --version=4.0.0-alpha.4 --all-features
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

The latest documentation can be found at the official **[docs.rant-lang.org](https://docs.rant-lang.org)**.

Since Rant 4 is early in development, some documentation may be outdated/incomplete, but it is actively updated to ensure that it reflects current features with reasonable accuracy.


## License

Rant is distributed under the GNU Affero General Public License v3. See [LICENSE](./LICENSE) for more details.

An alternative, commercial license is planned for the future.
