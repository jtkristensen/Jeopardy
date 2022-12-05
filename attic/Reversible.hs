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
class Linear term where
  infer :: term a -> Value a -> Runtime a (Bindings a)

-- Inverse environment inference for linear terms.
class LinearOp term where
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

instance Linear Pattern where
  infer (Variable _ x _) v = return [(x, v)]
  infer (Constructor c ps _) (Algebraic c' vs _)
    | c == c' && length ps == length vs =
      do pvs <- zipWithM infer ps vs
         return $ join pvs
  -- TODO: extend ERWS to be a transformer, or to include failure.
  infer _ _ = error "stuck in environment inference for pattern."

instance LinearOp Pattern where
  unInfer = infer

instance Eval Term where
  run (Pattern       p  ) = run p
  run (Application (Conventional f _) p _) =
    do ((p', _), (_t', _)) <- function <$> environment <?> f
       v'                 <- run p
       case patternMatch (canonical v') p' of
         NoMatch   -> error "stuck in evaluating term"
         MatchBy _f -> undefined
  run (Application (Invert g _) p a) = unRun (Application g p a)
  run (Case (_term, _t) _pts _a) = undefined

instance EvalOp Term where
  unRun = undefined
