# descartes

[![tests](https://github.com/vikraman/descartes/actions/workflows/workflow.yml/badge.svg)](https://github.com/vikraman/descartes/actions/workflows/workflow.yml)
[![opam](https://github.com/vikraman/descartes/actions/workflows/opam-dependency-submission.yml/badge.svg)](https://github.com/vikraman/descartes/actions/workflows/opam-dependency-submission.yml)

This is an implementation of the $\lambda\widetilde{\lambda}$ calculus.

## Usage

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

## Trivia

The name is a tribute to Descartes for his theory of Cartesian dualism.
