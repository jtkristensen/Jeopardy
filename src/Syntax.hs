{-|

Module      : Syntax
Description : Abstract syntax for Jeopardy (with a bit of syntactic sugar).
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

The current syntax of the invertible programming language suggested in the
article, `Jeopardy : an invertible programming language`, submitted to the
`37th symposium on implementation of functional languages` in Copenhagen 2022.

-}

module Syntax where

import Core.Syntax (Name, X, C, F)

-- It has been decided that types have upper case capitalization, while type
-- varibles have lower case. We resolve polymorphism like templates.
data T = ConcreteType Name | TypeVariable Name

data Program
  = Function F (Pattern, T) (Term, T)
  | Data T [(C, [T])]
  | Main Inversion

data Pattern
  = Variable X
  | Constructor C [Pattern]
  | NilPattern
  | ConsPattern Pattern Pattern
  | TuplePattern [Pattern]

data Term
  = VariableTerm    X
  | ConstructorTerm C [Term]
  | Nil
  | Cons Term Term
  | Tuple [Term]
  | Application Inversion Pattern
  | Let (X, T) Term Term
  | If   Term Term Term
  | Case (Term, T) [(Pattern, Term)]

data Inversion
  = Conventional F
  | Invert Inversion
