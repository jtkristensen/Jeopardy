{-|

Module      : Semantics.Reversible
Description : A per function invertible semantics for Jeopardy.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

An interpreter for Jeopardy that only accepts linearly typable programs, and
on the other hand promises that functions are locally invertible.

-}

-----------------------------------------------------------------------------
-- /!\ Warning.                                                            --
-----------------------------------------------------------------------------
-- Currently ERWS does not implement MonadFail. So, errors are just thrown --
-- directly. Note however, some errors cannot happen. For instance, if a   --
-- pattern is linearly typed, then we never call the code that throws an   --
-- error about unbound varibles (since type checking would already have    --
-- thrown the error).                                                      --
-----------------------------------------------------------------------------

module Semantics.Reversible where

import Core.Syntax
import Transformations.ProgramEnvironment
import Analysis.Unification
import Control.Monad (zipWithM)

type Bindings a = Transformation Pattern a
type Runtime  a = ERWS a (Bindings a) () ()
type Input    a = Value a
type Output   a = Value a

-- Running a program is to call the main function on a value.
interpret :: Program a -> (Input a -> Output a)
interpret program input = output
  where
    (output, _, _) = runERWS (evaluate meaning) program id mempty
    meaning        = Application transformation term (meta transformation)
    transformation = mainFunction program
    term           = canonical input

-- Unrunning a program corresponds to running its inverse program.
uninterpret :: Program a -> (Output a -> Input a)
uninterpret = interpret . invert

-- Programs running in the conventional direction.
class Evalation term where
  evaluate :: term a -> Runtime a (Value a)

-- Terms running in the opposit direction
class EvalationOp term where
  unEvaluate :: term a -> Runtime a (Value a)

-- Unique environment evaluatedToence for linear terms.
class LinearEvaluatedToence term where
  evaluatedTo :: term a -> Value a -> Runtime a (Bindings a)

-- Inverse environment evaluatedToence for linear terms.
class LinearEvaluatedToenceOp term where
  unEvaluatedTo :: term a -> Value a -> Runtime a (Bindings a)

instance Evalation Term where
  evaluate (Pattern p)
    = evaluate p
  evaluate (Application (Conventional f _) input _)
    = do ((arguments, _), (body, _)) <- function <$> environment <?> f
         output                      <- evaluate input
         case patternMatch (canonical output) arguments of
           NoMatch   -> error "stuck in evaluating term"
           MatchBy g -> local (const g) (evaluate body)
  evaluate (Application (Invert g _) p a)
    = unEvaluate (Application g p a)
  evaluate (Case (selector, _) cases _)
    = do env           <- ask
         value         <- evaluate selector
         (subst, body) <- firstMatch value cases
         local (const $ env . subst) $ evaluate body

instance EvalationOp Term where
  unEvaluate (Pattern p)
    = unEvaluate p
  unEvaluate (Application (Conventional f _) output _)
    = do ((arguments, _), (body, _)) <- function <$> environment <?> f
         result          <- unEvaluate output
         pastEnvironment <- body `evaluatedTo` result
         local (const pastEnvironment) $ evaluate arguments
  unEvaluate (Application (Invert g _) p a)
    = evaluate (Application g p a)
  unEvaluate Case {}
    = error "This cannot happend!"

instance LinearEvaluatedToence Term where
  evaluatedTo (Pattern p) v
    = p `evaluatedTo` v
  evaluatedTo (Application (Conventional f _) input _) result
    = do ((arguments, _), (body, _)) <- function <$> environment <?> f
         pastEnvironment <- body `evaluatedTo` result
         output          <- local (const pastEnvironment) $ evaluate arguments
         evaluatedTo (Pattern input) output
  evaluatedTo (Application (Invert g _) p a) result
    = Application g p a `unEvaluatedTo` result
  evaluatedTo (Case (selector, _) cases _) result
    = do (value, body)   <- firstUnMatch result cases
         pastEnvironment <- body `evaluatedTo` result
         choice          <- local (const pastEnvironment) $ evaluate value
         (.) pastEnvironment <$> selector `evaluatedTo` choice

instance LinearEvaluatedToenceOp Term where
  unEvaluatedTo (Application (Conventional f _) output _) input
    = do ((arguments, _), (body, _)) <- function <$> environment <?> f
         pastEnvironment <- arguments `evaluatedTo` input
         result          <- local (const pastEnvironment) $ evaluate body
         output `evaluatedTo` result
  unEvaluatedTo (Application (Invert g _) p a) result
    = Application g p a `evaluatedTo` result
  unEvaluatedTo _ _
    = error "This cannot happen!"

-- Evaluating a pattern corresponds to looking up the variables it contains.
instance Evalation Pattern where
  evaluate p@(Variable _ x _)
    = do subst <- ask
         case toCanonical $ subst p of
           Nothing -> error $ "unbound variable " ++ x
           Just v  -> return v
  evaluate (Constructor c ps a)
    = do vs <- mapM evaluate ps
         return (Algebraic c vs a)

-- It does not matter which order we look up variables.
instance EvalationOp Pattern where
  unEvaluate = evaluate

-- The unique environment in which the pattern evaluated to a particular value.
instance LinearEvaluatedToence Pattern where
  evaluatedTo (Variable _ x _) v
    = return $ x `mapsto` canonical v
  evaluatedTo (Constructor c _ _) (Algebraic c' _ _)
    | c /= c' = error $ concat
      [ "The value was constructed using ", c', " "
      , "but the provided term is constructed using ", c ]
  evaluatedTo (Constructor _ ps _) (Algebraic c' vs _)
    | length ps /= length vs = error $ concat
      [ "The constructor ", c', " expects ", show (length vs), " arguments. "
      , "But here, ", show (length ps), " were given."]
  evaluatedTo (Constructor _ ps _) (Algebraic _ vs _)
    = foldl (.) id <$> zipWithM evaluatedTo ps vs

-- The unique environment in which the pattern evaluated to a particular value.
instance LinearEvaluatedToenceOp Pattern where
  unEvaluatedTo = evaluatedTo

-- If successfull, returns the substitution and body of the first match in
-- the conventional direction.
firstMatch
  ::       Value a -> [(Pattern a, Term a)]
  -----------------------------------------------
  -> Runtime a (Transformation Pattern a, Term a)
firstMatch _ [              ] = error "pattern matching not exhaustive"
firstMatch v ((p, t) : cases) =
  case patternMatch (canonical v) p of
    NoMatch   -> firstMatch v cases
    MatchBy f -> return (f, t)

-- If successfull, returns the substitution and body of the first match in
-- the conventional direction.
firstUnMatch
  ::       Value a -> [(Pattern a, Term a)]
  -----------------------------------------------
  ->        Runtime a (Pattern a, Term a)
firstUnMatch _ [                ] = error "Search exhausted"
firstUnMatch v (q@(p, _) : cases) =
  case patternMatch (canonical v) p of
    NoMatch -> firstUnMatch v cases
    _       -> return q
