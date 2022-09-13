# Introduction.

Jeopardy is a functional programming language designed for writing
invertible algorithms without the syntactic restrictions of reversible programming.
In particular, Jeopardy allows the limited use of locally noninvertible operations,
provided that they are used in a way that can be statically determined to be globally invertible.

As the language design becomes less experimental, this repository will serve as the official
reference implementation in disseminations.

## Status:
[![hlint](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-hlint.yaml/badge.svg)](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-hlint.yaml)
[![tests](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-test.yaml/badge.svg)](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-test.yaml)

## TODO:
- [x] CI/CD.
- [x] Per module property based testing.
- [x] Available expressions analysis (Nilson and Nilson but as a type-and-effect system).
- [x] Consistency type checking.
- [x] Graph where labels are vertices, but edges are annotated by labels
      (where a function gives control to another function.
- [ ] Properly tested (limited) reversible semantics from IFL.
- [ ] Write a better todo list.
