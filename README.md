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

```
❯ stack exec -- stlc-infer

stlc> (\x -> x) 0
Term: Tapp (Tlam (<x> Tvar 0@0)) Tz
Type: Cvar t_2
Constraints: fromList [Constraint (Carrow (Cvar t1_1) (Cvar t1_1)) (Carrow Cnat (Cvar t_2))]
Solution: fromList [ExplSubst t_2 Cnat,ExplSubst t1_1 Cnat]
Principal type: Cnat

Tz

stlc> let f = \x -> x in f True
Term: Tlet (Tlam (<x> Tvar 0@0)) (<f> Tapp (Tvar 0@0) (Tbool True))
Type: Cvar t_3
Constraints: fromList [Constraint (Carrow (Cvar t1_2) (Cvar t1_2)) (Carrow Cbool (Cvar t_3))]
Solution: fromList [ExplSubst t_3 Cbool,ExplSubst t1_2 Cbool]
Principal type: Cbool

Tbool True

stlc>
```

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
