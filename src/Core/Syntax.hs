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
-- Note that everything is annotated with an annotation variable `a`, that
-- will be populated by properties inferred by program analysis.

data Program a
  = Function F (Pattern a, T) (Term a, T) (Program a)
  | Data T [(C, [T])]                     (Program a)
  | Main (Function a)
  deriving (Functor, Eq, Show)

-- distinguish about kinds of variables
data Sort = Ordinary | Existential
  deriving (Eq, Show)

data Pattern                  a
  = Variable    Sort X        a
  | Constructor C [Pattern a] a
  deriving (Functor, Eq, Show)

data Term                                  a
  = Pattern                  (Pattern a)
  | Application (Function a) (Pattern a)   a
  | Case (Term a, T) [(Pattern a, Term a)] a
  deriving (Functor, Eq, Show)

data Function            a
  = Conventional F       a
  | Invert (Function a) a
  deriving (Functor, Eq, Show)

data Value                a
  = Algebraic C [Value a] a
  deriving (Functor, Eq, Show)

-- A canonical form is a value. A value is a pattern that contains no
-- variables, a term can be a pattern. So,
class CanonicalForm c where
  canonical   :: Value a -> c a
  isCanonical :: c a -> Bool

instance CanonicalForm Pattern where
  canonical   (Algebraic   c vs a) = Constructor c (canonical <$> vs) a
  isCanonical (Constructor _ ps _) = all isCanonical ps
  isCanonical _                    = False

instance CanonicalForm Term where
  canonical               = Pattern . canonical
  isCanonical (Pattern p) = isCanonical p
  isCanonical _           = False

class MetaData m where
  meta :: m a -> a

instance MetaData Pattern where
  meta (Variable    _ _ a) = a
  meta (Constructor _ _ a) = a

instance MetaData Term where
  meta (Pattern         p) = meta p
  meta (Application _ _ a) = a
  meta (Case        _ _ a) = a

instance MetaData Function where
  meta (Conventional _ a) = a
  meta (Invert       _ a) = a

class Annotateable m where
  labels :: m label -> [label]

instance Annotateable Pattern where
  labels (Variable    _ _  l) = [l]
  labels (Constructor _ ps l) = l : (ps >>= labels)

instance Annotateable Term where
  labels (Pattern p) = labels p
  labels (Application i p l) = l : labels p <> labels i
  labels (Case (t, _) pts l) = l : labels t <> (pts >>= (\(p, s) -> labels p <> labels s))

instance Annotateable Function where
  labels (Conventional _ l) = [l]
  labels (Invert       i l) = l : labels i

class Invertible f where
  invert :: f -> f

instance Invertible (Function a) where
  invert f = Invert f (meta f)

instance Invertible (Program a) where
  invert (Function a b c program) = Function a b c (invert program)
  invert (Data       b c program) = Data       b c (invert program)
  invert (Main f                ) = Main (invert f)

-- Convinient utility functions.
mainFunction :: Program a -> Function a
mainFunction (Function _ _ _ program) = mainFunction program
mainFunction (Data       _ _ program) = mainFunction program
mainFunction (Main          function) = function
