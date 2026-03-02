# simpl

A tiny OCaml interpreter for a small expression language (SimPL). Includes lexer (ocamllex), parser (menhir), and an evaluator, plus a minimal test suite.

## Language features

- Integers and booleans
- Binary ops: `+`, `*`, `<=`
- `let x = e1 in e2`
- `if e1 then e2 else e3`
- Parentheses

## Build

Requires OCaml + dune.

```bash
dune build
```
