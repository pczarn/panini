## panini • [![Latest version](https://img.shields.io/crates/v/panini.png)](https://crates.io/crates/panini)

A general-purpose parser generator. Work in progress.

On the surface, the goal of Panini is ease of use. In the future, it
will also offer advanced features without compromising on the speed.

User-friendliness is harder to achieve in a statically typed language. Panini
has typed expressions and type inference. Expression are typed according to
intuitive rules:

| Expression    | Automatic type                     | Description                                 |
|---------------|------------------------------------|---------------------------------------------|
| terminal_name | **Terminal**                       | Use of a terminal symbol.                   |
| name          | _typeof_(name)                     | Use of the `name` symbol.                   |
| ()            | **()**                             | Nulling expression.                         |
| (A)           | _typeof_(A)                        | Group zero or more expressions.             |
| (A B)         | **(**_typeof_(A), _typeof_(B)**)** | Group two or more expressions.              |
| A*            | **Vec<**Elem**>**                  | A sequence that repeats zero or more times. |
| A+            | **Vec<**Elem**>**                  | A sequence that repeats one or more times.  |


Recursive types are forbidden.

Panini uses [gearley](https://github.com/pczarn/gearley) for Earley parsing,
which can handle all context-free grammars. If you can write a grammar, it
will run as expected.

Grammars that have mistakes may cause warnings during compilation. These
warnings may indicate that certain semantic actions are ignored.

## Related work

* [Marpa](https://jeffreykegler.github.io/Marpa-web-site/)
* [Oak](https://github.com/ptal/oak/)
* OMeta

## License

Dual-licensed for compatibility with the Rust project.

Licensed under the Apache License Version 2.0:
http://www.apache.org/licenses/LICENSE-2.0, or the MIT license:
http://opensource.org/licenses/MIT, at your option.
