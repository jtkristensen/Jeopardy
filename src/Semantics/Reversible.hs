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

-- Unique environment inference for linear terms.
class LinearInference term where
  infer :: term a -> Value a -> Runtime a (Bindings a)

-- Inverse environment inference for linear terms.
class LinearInferenceOp term where
  unInfer :: term a -> Value a -> Runtime a (Bindings a)

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
  evaluate (Case (_term, _) _pts _a)
    = undefined

instance EvalationOp Term where
  unEvaluate
    = undefined

instance LinearInference Term where
  infer _ _
    = undefined

instance LinearInferenceOp Term where
  unInfer
    = undefined

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
  unEvaluate
    = evaluate

-- The unique environment in which the pattern evaluated to a particular value.
instance LinearInference Pattern where
  infer (Variable _ x _) v
    = return $ x `mapsto` canonical v
  infer (Constructor c _ _) (Algebraic c' _ _)
    | c /= c' = error $ concat
      [ "The value was constructed using ", c', " "
      , "but the provided term is constructed using ", c ]
  infer (Constructor _ ps _) (Algebraic c' vs _)
    | length ps /= length vs = error $ concat
      [ "The constructor ", c', " expects ", show (length vs), " arguments. "
      , "But here, ", show (length ps), " were given."]
  infer (Constructor _ ps _) (Algebraic _ vs _)
    = foldl (.) id <$> zipWithM infer ps vs

-- The unique environment in which the pattern evaluated to a particular value.
instance LinearInferenceOp Pattern where
  unInfer
    = infer
