# stlc-infer

A small interpreter for the simply typed lambda calculus, with type inference.

It's pretty heavily commented. Try looking at some files in the
[src/Stlc](src/Stlc) directory!

## Usage

This project is written in Haskell, and we're using Stack to manage all
dependencies. If you already have Stack installed, you can run:

```
stack build
stack test
stack exec -- stlc-infer
```

This launches an interactive REPL. The syntax is similar to Haskell, but only
has intro and elim forms for functions, numbers, booleans. (You can also glean
the syntax from [test/Spec.hs](test/Spec.hs) and
[src/Stlc/Parser.hs](src/Stlc/Parser.hs)).

## TODO

It should pretty much work. But since this is kind of a little playground for
me, these are things I'm going to try to also implement:

- [ ] Replace naturals with proper integers
- [ ] Allow type annotations from programmer
- [ ] Let-polymorphism (simple version)
- [ ] Unit test individual files, i.e.:
  - actually test that things infer to the right type
  - actually test that things evaluate to the right value
- [ ] Better type error messages with position information
- [ ] Get type of identifier (or even expression) at position

## License

[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://jez.io/MIT-LICENSE.txt)
