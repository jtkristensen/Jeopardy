{-|

Module      : Analysis.Bindings.
Description : Properties about bindings in Jeopardy programs.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

This analysis contains the most general bindings analysis.

-}

module Analysis.Bindings
  ( bindingsViolationsAnalysis )
where

import Core.Syntax
-- import Analysis.Definitions

-- import Control.Monad.RWS (RWS(..), runRWS)

data BindingsViolation a
  = IrregularPattern       F X [a] -- f x .. x = ?
  | DefinedButNotUsed      F X  a  -- f x      = ..
  | UsedButNotDefined      F X  a  -- f ..     = .. x ..
  | LinearityViolation     F X [a] -- f ?      = .. x .. x ..
  | ConflictingDefinitions F X [a] -- f x      = .. let x = ..
  deriving (Eq, Show)

-- type BindingsAnalysis a = RWS (F, Program a) [BindingsViolation a] ()

bindingsViolationsAnalysis :: Program a -> [BindingsViolation a]
bindingsViolationsAnalysis = undefined

class FreeVariables f where
  freeVariables :: f a -> [Name]

instance FreeVariables Pattern where
  freeVariables (Variable    x    _) = return x
  freeVariables (Constructor _ ps _) = ps >>= freeVariables

instance FreeVariables Term where
  freeVariables (Pattern       p  ) = freeVariables p
  freeVariables (Application _ p _) = freeVariables p
  freeVariables (Case (t, _) pts _) =
        freeVariables t
    ++ (pts >>= freeVariables . fst)
    ++ (pts >>= freeVariables . snd)
