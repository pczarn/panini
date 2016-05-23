## Panini • [![Latest version](https://img.shields.io/crates/v/panini.png)](https://crates.io/crates/panini)

Work in progress.
A general-purpose parser generator as a Rust syntax extension.

On the surface, the goal of Panini is ease of use. In the future, it
will also offer advanced features without compromising on the speed.

Panini uses **[gearley](https://github.com/pczarn/gearley)** for Earley parsing,
which can handle all context-free grammars. If you can write a grammar for a
language, it will work as expected.

### Syntax and semantics of grammar description

User-friendliness is harder to achieve in a statically typed language.
Consequently, Panini offers typed expressions and type inference. Expression
are typed according to intuitive rules:

| Expression    | Automatic type                     | Description                                              |
|---------------|------------------------------------|----------------------------------------------------------|
| `name`        | _type of name_                     | Use of a symbol.                                         |
| `()`          | **()**                             | A nulling expression.                                    |
| `(A)`         | _type of A_                        | Ignore redundant parentheses.                            |
| `(A B)`       | **(**_type of A_, _type of B_**)** | Group two or more expressions.                           |
| `item*`       | **Vec<**_type of item_**>**        | A sequence that repeats zero or more times.              |
| `item+`       | **Vec<**_type of item_**>**        | A sequence that repeats one or more times.               |
| `A | B`       | _type of A and B_                  | An alternative. All of its arms must have the same type. |

Automatic recursive types are forbidden.

Grammars that have mistakes may cause warnings during compilation. These
warnings may indicate that certain semantic actions are ignored.

### Dependency graph

These libraries were created for Panini.

![Dependency graph](doc/dependency_graph.png)

### Related work

* [Marpa](https://jeffreykegler.github.io/Marpa-web-site/) — an Earley parser (not a generator)
  that has advanced features. Written in C and Perl.
* [YAEP](https://github.com/vnmakarov/yaep) — an Earley parser engine that currently has
  the best speed and small memory use. Written in C.
* [Oak](https://github.com/ptal/oak/) — a PEG parser generator with typed expressions.
  Written in Rust.
* [LALRPOP](https://github.com/ptal/oak/) — a LR(1) parser generator focused on ease of use.
  Written in Rust.
* OMeta — a PEG parser with advanced features that go beyond parsing.

### License

Dual-licensed for compatibility with the Rust project.

Licensed under the Apache License Version 2.0:
http://www.apache.org/licenses/LICENSE-2.0, or the MIT license:
http://opensource.org/licenses/MIT, at your option.
