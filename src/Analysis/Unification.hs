{-|

Module      : Analysis.Unification
Description : Implements various unification algorithms used for Most General Match.
Author      : Joachim Tilsted Kristensen
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : tilsted@di.ku.dk
Stability   : experimental
Portability : POSIX

-}

module Analysis.Unification where

import Core.Syntax
import Control.Monad.Except

type Transformation f a = (f a -> f a)

-- Iff two patterns match, then a transformation exists, that make unify
-- (make equal) expressions that consists of those patterns.
data PatternMatch a
  = NoMatch
  | MatchBy (Transformation Pattern a)

-- Decides if two patterns match, and provides the above mentioned
-- transformation as evidence.
patternMatch :: Pattern a -> Pattern a -> PatternMatch a
patternMatch p q =
  case runExcept $ unifier $ unify p q of
    (Left ()) -> NoMatch
    (Right f) -> MatchBy f

-- Succeds if the a pattern match is not NoMatch.
isMatch :: PatternMatch meta -> Bool
isMatch NoMatch = False
isMatch _       = True

-- A unifier is a computation that either fails, or provides the
-- transformation.
type Unifier a = Except () (a -> a)

doesNotUnify :: Unifier failure
doesNotUnify = throwError ()

-- A is a special unifying transformation.
newtype Substitution f a
  = Substitution { unifier :: Unifier (f a) }

instance Semigroup (Substitution f a) where
  s1 <> s2 = Substitution $ (.) <$> unifier s1 <*> unifier s2

instance Monoid (Substitution f a) where
  mempty  = Substitution $ return id
  mappend = (<>)

-- Computes the most general unifier for patterns
unify :: Pattern a -> Pattern a -> Substitution Pattern a
unify   (Variable  _ x _)   (Variable  _ y _)  | x == y               = mempty
unify   (Variable  _ x _) p                    | not (p `contains` x) = p `substitutes` x
unify p                      (Variable  _ x _) | not (p `contains` x) = p `substitutes` x
unify   (Constructor c ps _) (Constructor t qs _)
  | c == t && length ps == length qs
  = foldr ((<>) . uncurry unify) mempty (zip ps qs)
unify _ _
  = Substitution doesNotUnify

-- Holds if the variable argument appears in the pattern.
contains :: Pattern a -> X -> Bool
contains (Variable  _ x _) y | x == y = True
contains (Constructor _ ps _) y       = any (`contains` y) ps
contains _                    _       = False

-- The substitution where `x` is replaced by `q`.
substitutes :: Pattern a -> X -> Substitution Pattern a
substitutes p x = Substitution $ return subst
  where
    subst (Variable    _ y _) | x == y = p
    subst (Constructor c ps m)       = Constructor c (subst <$> ps) m
    subst q                          = q

-- Modifies the transformation resulting from substitution.
modify
  :: (Transformation f a -> Transformation g b)
  -> (Substitution   f a -> Substitution   g b)
modify t s =
  Substitution $ t <$> unifier s
