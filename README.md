# lambda-calculus

Interpreter for untyped lambda calculus, written in Haskell.

Use cases:

- Check homework solutions.
- Appreciate lambda calculus.

## Build

Build with `cabal`.

## Usage

```
lambda-calculus [FILE] [-l|--load PRELUDE]
```

- `FILE` is the path to the input file. Run in REPL mode if it is unspecified.
- `PRELUDE` is the path to an optional pre-loaded file. See `prelude.lc` for an example.

## Syntax

The abstract syntax is as follows:

```
<prog> ::= <cmd>*
<cmd>  ::= <var> = <term>                                   definition
        |  <term>                                           evaluation
<term> ::=  <var> | <term> <term> | (λ|\) <var> . <term>
```

Commands are separated by line breaks.

A variable consists of letters and digits and starts with a letter.

Applications associate to the left, and the scope of a lambda abstraction extends as far as possible. For example, `λx. x x x` is the same as `(λx. (x x) x)`.

See `src/Parser.y` for the concrete syntax.

## Semantics

- Definition `x = t`, where `x` is a variable and `t` is a term, makes `x` an alias of `t`. Future definitions and evaluations substitue `t` for `x` before execution. New definitions shadow old definitions with the same name. Print an empty line.
- Evaluation `t`, where `t` is a term, prints the normal form of `t`. If `t` does not have a normal form, the interpreter does not halt. Free variables are allowed.

## Example

Load the provided prelude with `-l prelude.lc`. The following example computes `0 + 1 + ... + n` with the Y combinator.

```
sum = Y λs. λn. if0 n zero (plus n (s (pred n)))

sum zero
(λx. λy. y)
sum one
(λx. λy. x y)
sum two
(λx. λy. x (x (x y)))
sum three
(λx. λy. x (x (x (x (x (x y))))))
```

## Implementation

The parser `src/Parser.hs` is generated from `src/Parser.y` with the [Happy](https://github.com/haskell/happy) parser generator.

`src/ADT.hs` defined two representations of lambda terms, `NamedTerm` and `Term`, and functions to convert them back and forth.

`Term` uses names for free variables and De Bruijn indices for (1) bound variables and (2) bound variables that become free in the process of manipulation.

Input is parsed into `NamedTerm`, and functions in `src/Eval.hs` use `Term`, which is converted back to `NamedTerm` for printing. Note that bound variables may change names when converted back.

Normalization happens step by step. (See the `step` function in `src/Eval.hs`.) In an application `t1 t2`, reduce `t1` to a lambda abstraction or until stuck. Once `t1` is reduced to a lambda abstraction, apply β-reduction.

Call-by-name and call-by-value are also implemented.

## Libraries Used

- [happy](https://github.com/haskell/happy)
- [split](https://hackage.haskell.org/package/split)
- [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)

See `lambda-calculus.cabal` for the full list.

## Future Work

Introduce types, which can be selected in options:

- Simple types
- Polymorphic types
- Dependent types
