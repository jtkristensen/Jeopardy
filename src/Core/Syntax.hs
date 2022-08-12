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

import Data.Bifunctor (first, bimap)

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

data Pattern                  a
  = Variable X                a
  | Constructor C [Pattern a] a

data Term                                  a
  = Pattern                   (Pattern a)
  | Application (Inversion a) (Pattern a)  a
  | Case (Term a, T) [(Pattern a, Term a)] a

data Inversion           a
  = Conventional F       a
  | Invert (Inversion a) a

data Value                a
  = Algebraic C [Value a] a

-- * All of the above are off course functors.
instance Functor Program where
  fmap f (Function g p t p') = Function g (first (f<$>) p) (first (f<$>) t) (f <$> p')
  fmap f (Data t cts     p') = Data t cts (fmap f p')
  fmap f (Main g           ) = Main (fmap f g)

instance Functor Pattern where
  fmap f (Variable    x    a) = Variable x (f a)
  fmap f (Constructor c ps a) = Constructor c (fmap f <$> ps) (f a)

instance Functor Term where
  fmap f (Pattern p) = Pattern (fmap f p)
  fmap f (Application g p a) = Application (fmap f g) (fmap f p) (f a)
  fmap f (Case t pts a) = Case (first (fmap f) t) (bimap (fmap f) (fmap f) <$> pts) (f a)

instance Functor Inversion where
  fmap f (Conventional g a) = Conventional g (f a)
  fmap f (Invert i a)       = Invert (fmap f i) (f a)

instance Functor Value where
  fmap f (Algebraic c vs a) = Algebraic c (fmap f <$> vs) (f a)

-- * Further more, a canonical form is a value. A value is a pattern that
-- * contains no variables, a term can be a pattern hence:
class CanonicalForm c where
  canonical :: Value a -> c a

instance CanonicalForm Pattern where
  canonical (Algebraic c vs a) = Constructor c (canonical <$> vs) a

instance CanonicalForm Term where
  canonical = Pattern . canonical
