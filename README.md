# Rant

|**IMPORTANT:**<br>This project is in early alpha, and there are a lot of broken/missing features.<br>Please do not use in production environments until a stable version is released.|
|-|
<br>

Rant is a language for procedural text generation.
It is designed to help you write more dynamic and expressive templates, dialogue, stories, names, test data, and much more.

## Introducing Rant 4.0

Rant 4.0 is a complete redesign and reimplementation of the Rant language and runtime. The result is a completely redefined and superior text generation experience designed around the needs of everyone-- from writers and programmers, to anybody else!

## Rant is **intuitive**

Rant is basically the opposite of Regex: Instead of matching a string to a pattern, you provide the pattern and a matching string comes out!

Rant makes it easy to express your desired output as a set of branching possibilities.
For example, here is a simple regex that matches on three strings:

```regex
(foo|bar|baz)
```

And its equivalent in Rant:

```rant
{foo|bar|baz} # Resolves randomly to "foo", "bar", or "baz"
```

## Rant is **concise**

Common operations have shorter syntax. This means you can apply Rant to your most common use cases with minimal code. 

For more complex generation, Rant makes templating tasks far more painless than in conventional programming languages with its powerful set of synchronization, branching, and generation tools.

While randomization is one of the cornerstones of Rant's power, deterministic selection is just as easy.

## Rant is **flexible**

Rant does more than generate random strings: its behavior is infinitely configurable for a wide range of use cases ranging from natural language generation to simple code templating. What you do with it is up to you!

In addition to its rock-solid string generation tools, Rant offers a fully-featured variable system including common primitives like numbers and booleans as well as collection types. And with Rant's extensive formatting tools, you can fine-tune how anything prints.

## Licensing

TBA