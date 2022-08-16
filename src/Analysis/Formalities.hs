{-|

Module      : Analysis.Formalities.
Description : Analyis for verifying Jeopardy programs.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

These analysis check that programs are not malformed. Meaning that a
constructor is not applied to too few arguments. Never is a function symbol
applied to a function which is not defined and so on.

-}

module Analysis.Formalities
  ( bindingsViolationsAnalysis )
where

import Core.Syntax
import Data.List
  ( (\\), nub )

import Control.Monad.RWS
  ( RWS, runRWS, ask, local, tell )

import Control.Monad
  ( void )

-- We throw an error about the violation, and in which function it occured.
-- This is sufficient information to search the program text for more useful
-- information upon raising an error.
data BindingsViolation
  = IrregularPattern       F X -- f x .. x = ?
  | DefinedButNotUsed      F X -- f x      = ..
  | UsedButNotDefined      F X -- f ..     = .. x ..
  | ConflictingDefinitions F X -- f x      = .. let x = ..
  deriving (Eq, Show)

type BindingsAnalysis a = RWS (F, Program a, [X]) [BindingsViolation] ()

bindingsViolationsAnalysis :: Program a -> [BindingsViolation]
bindingsViolationsAnalysis program = writer
  where reader         = ("", program, [])
        (_, _, writer) = runRWS (violations program) reader state
        state          = ()

throw :: BindingsViolation -> BindingsAnalysis a ()
throw v = void $ tell [v]

class Violatable thing where
  violations :: thing a -> BindingsAnalysis a ()

instance Violatable Program where
  violations (Main _                          ) = return ()
  violations (Data _ _                 program) = violations program
  violations (Function f (p, _) (t, _) program) =
    do (_, source, _) <- ask
       mapM_ (throw . IrregularPattern f) (free \\ nub free)
       _ <- local (const (f, source, nub free)) (violations t)
       violations program
    where
       free = freeVariables p

-- instance Violatable Pattern where
--   violations (Variable x _) =
--     do (f, _, ys) <- ask
--        case ys of
--          [ y ] | x == y -> return ()
--          ys             -> if   x `elem` ys
--                            then throw $ ConflictingDefinitions f x
--                            else throw $ UsedButNotDefined      f x
--   violations (Constructor c ps _) =
--     do (f, _, ys) <- ask

instance Violatable Term where
  violations _ = undefined

-- instance Violatable Inversion where
--   violations _ = undefined

freeVariables :: Pattern a -> [X]
freeVariables (Variable    x    _) = return x
freeVariables (Constructor _ ps _) = ps >>= freeVariables
