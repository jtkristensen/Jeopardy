{-# LANGUAGE DeriveFunctor #-}
{-|

Module      : Core.Syntax
Description : Abstract syntax for Jeopardy.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

The current syntax of the invertible programming language suggested in the
article, `Jeopardy : an invertible programming language`, submitted to the
`37th symposium on implementation of functional languages` in Copenhagen 2022.

-}

module Core.Syntax where

-- * Abbreviations.

type Name = String
type X    = Name   -- Variable names.
type C    = Name   -- Data constructor names.
type T    = Name   -- Data type names.
type F    = Name   -- Function names.

-- * Language definition.
--
--   Note that everything is annotated with an annotation variable `a`, that
--   will be populated by properties inferred by program analysis.

data Program a
  = Function F (Pattern a, T) (Term a, T) (Program a)
  | Data T [(C, [T])]                     (Program a)
  | Main (Inversion a)
  deriving (Functor, Eq)

data Pattern                  a
  = Variable X                a
  | Constructor C [Pattern a] a
  deriving (Functor, Eq)

data Term                                  a
  = Pattern                   (Pattern a)
  | Application (Inversion a) (Pattern a)  a
  | Case (Term a, T) [(Pattern a, Term a)] a
  deriving (Functor, Eq)

data Inversion           a
  = Conventional F       a
  | Invert (Inversion a) a
  deriving (Functor, Eq)

data Value                a
  = Algebraic C [Value a] a
  deriving (Functor, Eq)

-- * Further more, a canonical form is a value. A value is a pattern that
-- * contains no variables, a term can be a pattern hence:
class CanonicalForm c where
  canonical :: Value a -> c a

instance CanonicalForm Pattern where
  canonical (Algebraic c vs a) = Constructor c (canonical <$> vs) a

instance CanonicalForm Term where
  canonical = Pattern . canonical
