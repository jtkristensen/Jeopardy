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
import Control.Monad (join, zipWithM)

type Bindings a = [(X, Value a)]
type Runtime  a = ERWS a (Bindings a) () ()

-- Programs running in the conventional direction.
class Eval term where
  run   :: term a -> Runtime a (Value a)

-- Terms running in the opposit direction
class EvalOp term where
  unRun :: term a -> Runtime a (Value a)

-- Unique environment inference for linear terms.
class LinearInference term where
  infer :: term a -> Value a -> Runtime a (Bindings a)

-- Inverse environment inference for linear terms.
class LinearInferenceOp term where
  unInfer :: term a -> Value a -> Runtime a (Bindings a)

-- Evaluating a pattern corresponds to looking up the variables it contains.
instance Eval Pattern where
  run (Variable _ x _) =
    do u <- lookup x <$> ask
       case u of
         Nothing -> error $ "unbound variable " ++ x
         Just v  -> return v
  run (Constructor c ps a) =
    do vs <- mapM run ps
       return (Algebraic c vs a)

-- It does not matter which order we look up variables.
instance EvalOp Pattern where
  unRun = run

-- The unique environment in which the pattern evaluated to a particular value.
instance LinearInference Pattern where
  infer (Variable _ x _) v = return [(x, v)]
  infer (Constructor c _ _) (Algebraic c' _ _)
    | c /= c' = error $ concat
      [ "The value was constructed using ", c', " "
      , "but the provided term is constructed using ", c ]
  infer (Constructor _ ps _) (Algebraic c' vs _)
    | length ps /= length vs = error $ concat
      [ "The constructor ", c', " expects ", show (length vs), " arguments. "
      , "But here, ", show (length ps), " were given."]
  infer (Constructor _ ps _) (Algebraic _ vs _) = join <$> zipWithM infer ps vs

-- The unique environment in which the pattern evaluated to a particular value.
instance LinearInferenceOp Pattern where
  unInfer = infer

instance Eval Term where
  run (Pattern       p  ) = run p
  run (Application (Conventional f _) p _) =
    do ((p', _), (_t', _)) <- function <$> environment <?> f
       v'                 <- run p
       case patternMatch (canonical v') p' of
         NoMatch    -> error "stuck in evaluating term"
         MatchBy _f -> undefined
  run (Application (Invert g _) p a) = unRun (Application g p a)
  run (Case (_term, _t) _pts _a) = undefined

instance EvalOp Term where
  unRun = undefined

instance LinearInference Term where
  infer _ _ = undefined

instance LinearInferenceOp Term where
  unInfer = undefined
