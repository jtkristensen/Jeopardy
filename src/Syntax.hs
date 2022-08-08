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

module Syntax
  ( desugar
  , Program(..)
  , Pattern(..)
  , Term(..)
  , Inversion(..)
  ) where

import           Core.Syntax (Name, X, C, F)
import qualified Core.Syntax as Core

import Control.Monad.State  (State(..) , runState )
import Control.Monad.Writer (Writer(..), runWriter)

-- It has been decided that types have upper case capitalization, while type
-- varibles have lower case. We resolve polymorphism like templates.
data T = ConcreteType Name | TypeVariable Name

-- We generate built-in tuples for each type, for each size.
-- Likewise for built-in lists.
data Context =
  Context { max_tuple_size :: Integer
          , datatype_names :: [T]
          }

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

desugar :: Program -> Core.Program ()
desugar = undefined

-- desugarPattern :: Pattern -> Core.Pattern ()
-- desugarPattern (Variable    x   )   = Core.Variable x ()
-- desugarPattern (Constructor c ps)   = Core.Constructor c (map desugarPattern ps) ()
-- desugarPattern (NilPattern      )   = Core.Constructor "$built-in-nil" [] ()
-- desugarPattern (ConsPattern x xs)   = Core.Constructor "$built-in-cons" [x', xs'] ()
--   where x'  = desugarPattern x
--         xs' = desugarPattern xs
-- desugarPattern (TuplePattern p1 p2) = Core.Constructor "$built-in-tuple" [q1, q2] ()
--   where q1 = desugar p1
--         q2 = desugar p2

-- desugarTerm :: Term -> Core.Term ()
-- desugarTerm (VariableTerm    x   ) = Core.Pattern (desugarPattern (Variable x))
-- desugarTerm (ConstructorTerm c ts) = Core.Pattern (

-- desugarInversion :: Inversion -> Core.Inversion             ()
-- desugarInversion (Conventional f) = Core.Conventional f     ()
-- desugarInversion (Invert       g) = Core.Invert (desugar g) ()

