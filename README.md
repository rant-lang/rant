# Rant

[![Crates.io](https://img.shields.io/crates/v/rant)](https://crates.io/crates/rant)
[![Crates.io](https://img.shields.io/crates/d/rant)](https://crates.io/crates/rant)
[![Docs.rs](https://docs.rs/rant/badge.svg)](https://docs.rs/rant)

***
### IMPORTANT:

**This project is in early alpha, and there are a lot of broken/missing features.**

**While I encourage you to try it out and give feedback, please do not use in production environments until a stable version is released.**

**Features may appear or disappear at any time for any reason. Assume that every alpha release will be breaking.**

***

Rant is a language for procedural text generation.
It is designed to help you write more dynamic and expressive templates, dialogue, stories, names, test data, and much more.

## Introducing Rant 4.0

Rant 4.0 is a complete redesign and reimplementation of the Rant language and runtime. The result is a completely redefined and superior text generation experience designed around the needs of everyone-- from writers and programmers, to anybody else!

### Rant is intuitive

Rant is basically the opposite of Regex: Instead of matching a string to a pattern, you provide the pattern and a matching string comes out!

Rant makes it easy to express your desired output as a set of branching possibilities.
For example, consider a simple regex that matches on three strings:

```regex
(foo|bar|baz)
```

The Rant equivalent to generate any matching string is quite similar:

```rant
{foo|bar|baz} # Resolves randomly to "foo", "bar", or "baz"
```

### Rant is concise

Common operations have shorter syntax. This means you can apply Rant to your most common use cases with minimal code. 

For more complex generation, Rant makes templating tasks far more painless than in conventional programming languages with its powerful set of synchronization, branching, and generation tools.

### Rant is flexible

Rant does more than generate random strings: its behavior is infinitely configurable for a wide range of use cases ranging from natural language generation to simple code templating. What you do with it is up to you!

In addition to its rock-solid string generation tools, Rant offers a fully-featured variable system including common primitives like numbers and booleans as well as collection types. And with Rant's extensive formatting tools, you can fine-tune how anything prints.

## Getting started

### CLI

Rant comes with a CLI tool that includes a REPL. You can install and run it with:

```sh
$ cargo install rant --version=4.0.0-alpha.3 --all-features
$ rant
```

### Library

Add Rant to your `Cargo.toml`:

```toml
[dependencies]
rant = "*" # Actual version may vary
```

You can run a Rant program with just a few lines of code:

```rust
use rant::Rant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
  // Create a default Rant context and load the standard library
  let mut rant = Rant::new();

  // Compile a simple program
  let program = rant.compile_quiet(r#"
  [$greet:name] {
    {Hello|Hi|Hey} <name>!
  }
  [greet:world]
  "#)?;

  // Run the program and fetch the result string
  let output = rant.run(&program)?;
  println!("{}", output);

  Ok(())
}
```

## Learn Rant

The latest documentation can be found at the official **[docs.rant-lang.org](https://docs.rant-lang.org)**.

Since Rant 4 is early in development, some documentation may be outdated/incomplete, but it is actively updated to ensure that it reflects current features with reasonable accuracy.


## License

Rant is distributed under the GNU Affero General Public License v3. See [LICENSE](./LICENSE) for more details.

An alternative, commercial license is planned for the future.
