# Jeopardy lang.

As the paper currently suggests that bi-directional first match can be extended with Reaching Definitions to include various programs (such as the `add` example), I have decided to implement the language in its final form (syntactically). It has obtained its own repository, since I want to rewrite it with proper testing as a go and less experimentation.

## Status:
[![hlint](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-hlint.yaml/badge.svg)](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-hlint.yaml)
[![tests](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-test.yaml/badge.svg)](https://github.com/jtkristensen/Jeopardy/actions/workflows/main-test.yaml)

## TODO:
- [x] Abstract syntax etc.
- [x] Basic program validity checks to be assumed further.
- [x] Transformation that introduces labels everywhere.
- [x] Per module property based testing.
- [x] CI/CD.
- [ ] Reversible semantics from paper.
- [ ] Available expressions analysis (Nilson and Nilson but as a type-and-effect system).
- [ ] Consistency type checking.
- [ ] Plausible pattern expansions analysis.
- [ ] Graph where labels are vertices, but edges are annotated by labels
      (where a function gives control to another function.
- [ ] Write a better todo list.
- [ ] Reconstruction types (is the name we gave them at a meeting).
- [ ] Does the analysis need a program elaborator.

## To make decidable by analysis
- [ ] Decide to do equality check or continue evaluating.
- [ ] Ortogonality by analysis.
- [ ] Ortogonality by partial evaluation.
- [ ] Bidirectional online partial evaluator "(RD |- term -| Gamma) ~> term".
- [ ] Derivable Expressions.
- [ ] Eventual "non-existentialism".
- [ ] Tail recursive functions.
- [ ] Termination checking.
- [ ] Generalize Torbens notion of semi-inversion.
- [ ] Control flow analysis (CFA1 from Nilson and Nilson).
- [ ] Desugaring program transformation.
- [ ] Template polymorphism.
- [ ] Extend with lazy evaluation and existential variables that.
- [ ] Size change termination (straigt up, to see that I understand it properly).
- [ ] Analysis similar to size change that builds multiple graphs.
