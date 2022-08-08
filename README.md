# Jeopardy lang.

As the paper currently suggests that bi-directional first match can be extended with Reaching Definitions to include various programs (such as the `add` example), I have decided to implement the language in its final form (syntactically). It has obtained its own repository, since I want to rewrite it with proper testing as a go and less experimentation.

## Status:
[![hlint](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-hlint.yaml/badge.svg)](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-hlint.yaml)
[![tests](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-test.yaml/badge.svg)](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-test.yaml)

## TODO:
- [ ] Abstract syntax etc..
- [ ] Type checker for linear typing.
- [ ] Interpreter reversible semantics.
- [ ] Reaching definitions analysis (Nilson and Nilson but as a type-and-effect system).
- [ ] Use reaching definitions in online partial evaluation (instead of in match-policy)?
- [ ] Control flow analysis (CFA1 from Nilson and Nilson).
- [ ] Per module property based testing.
- [ ] CI/CD.
- [ ] Consistency type checking.
- [ ] Limited Invertible Semantics based on Reaching Definitions
- [ ] Test that the invertible semantics is a conservative extension of the reversible one.
- [ ] Reconstruction types (is the name we gave them at a meeting).
- [ ] Extend with lazy evaluation and existential variables that.
- [ ] Analysis that check that existential variables will be resolvable before they are used.
- [ ] Program elaborator (transformation that expands terms until existential parts are distinguishable from non-existential ones.
- [ ] Size change termination (straigt up, to see that I understand it properly).
- [ ] Analysis similar to size change that builds multiple graphs.
