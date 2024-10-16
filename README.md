# descartes

[![tests](https://github.com/vikraman/descartes/actions/workflows/workflow.yml/badge.svg)](https://github.com/vikraman/descartes/actions/workflows/workflow.yml)
[![opam](https://github.com/vikraman/descartes/actions/workflows/opam-dependency-submission.yml/badge.svg)](https://github.com/vikraman/descartes/actions/workflows/opam-dependency-submission.yml)

This is an Ocaml implementation of the $\lambda\widetilde{\lambda}$ calculus.

## Usage

To build and test, use opam and dune:

``` bash
$ opam install . --deps-only --with-test
$ opam exec -- dune build
$ opam exec -- dune runtest
```

To use the repl:

```bash
$ dune exec bin/main.exe
λλ~> (\x:int.x+1) 2
   (fn (x : int) => x + 1) 2 : int
~> 3 : int
λλ~> \~x:co int.x
   cofn (x : co int) => x : (int + co int)
~> cofn (x : co int) => x : (int + co int)
λλ~> case (\~x:co int.x) of x.0 | y.1
   case cofn (x : co int) => x of inl x => 0 | inr y => 1 : int
~> 1 : int
```

The syntax closely follows the syntax used in the code samples in the paper.
The test suite contains several examples.

## Acknowledgements

gpt-4-1106 was used to generate the parser for the syntax of the language.

## Related

- https://github.com/dignissimus/coexp
- https://github.com/vikraman/sml-coexp
- https://github.com/vikraman/hs-coexp
- https://github.com/vikraman/agda-coexp

## Trivia

The name is a tribute to Descartes for his theory of Cartesian dualism.
