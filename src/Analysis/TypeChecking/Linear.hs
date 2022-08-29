{-|

Module      : Analysis.TypeChecking.Linear
Description : Linear type checking for Jeopardy Programs.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

Linear type checking for the reversible semantics of Jeopardy as suggested
in the article, `Jeopardy : an invertible programming language`, submitted
to the `37th symposium on implementation of functional languages` in
Copenhagen 2022.

-}

module Analysis.TypeChecking.Linear
  (check, LinearTypeError, ConflictingCall, ConflictingDefinitions )
where

import Core.Syntax
import Transformations.ProgramEnvironment
import Analysis.Definitions
import Analysis.ArgumentsAndCalls

import Transformations.Join ( join     )
import Data.Maybe           ( fromJust )
import Data.List            ( nub      )

import Control.Monad.RWS (RWS, runRWS)

data Error a
  = ErrorneousCall       (ConflictingCall a)
  | ErrorneousDefinition ConflictingDefinitions
  | IllTyped (LinearTypeError a)

data LinearTypeError               a
  = ExpectedButGot           F T T a
  | UnusedRescource          F X   a
  | UndefinedRescource       F X   a
  | ReusedRescource          F X  [a]
  | ExistentialsAreNotLinear F X   a
  | IrregularPattern         F     a
  deriving Eq

-- Applies linear typing rules as described in the paper (Jeopardy : an
-- invertible programming language). Returns a list of errors if something
-- went wrong.
check :: Eq a => Program a -> Either [Error a] (Program (a, T))
check program =
  case ds ++ ac of
    [] ->
      case runRWS (run program) (programEnvironment program, "$>") mempty of
        (typedProgram, _, []) ->
          Right $ fromJust $ join program typedProgram
        (_, _, typeErrors)         ->
          Left (IllTyped <$> combine typeErrors)
    xs -> Left xs
  where
   ds  = map ErrorneousDefinition $ duplicateDefinitionsAnalysis      program
   ac  = map ErrorneousCall       $ problematicArgumentOrCallAnalysis program
   run = coanalyse . analyse
   combine (ReusedRescource f x as : rest) =
     nub $ ReusedRescource f x (as <> (rest >>= reused f x)) : combine rest
   combine (err                    : rest) = err : combine rest
   combine [                             ] = []
   reused f x (ReusedRescource g y as) | f == g && x == y = as
   reused _ _ _                                           = []

data State a =
  State
    { used    :: [(X, T, a)]
    , defined :: [(X, T, a)]
    }

instance Semigroup (State a) where
  s1 <> s2 = State (used s1 <> used s2) (defined s1 <> defined s2)

instance Monoid (State a) where
  mempty  = State [] []
  mappend = (<>)

newtype Analysis a b
  = Analysis
  { coanalyse :: RWS (Environment (Analysis a) a, F) [LinearTypeError a] (State a) b }

instance Monad (Analysis a) where
  return   = Analysis . return
  ma >>= f = Analysis $ coanalyse ma >>= coanalyse . f

instance Applicative (Analysis a) where
  pure        = return
  an0 <*> an1 = an0 >>= \f -> f <$> an1

instance Functor (Analysis a) where
  fmap f = Analysis . fmap f . coanalyse

class LinearlyTypeable thing where
  analyse :: thing a -> Analysis a (thing T)

instance LinearlyTypeable Program where
  analyse = undefined

instance LinearlyTypeable Pattern where
  analyse = undefined

instance LinearlyTypeable Term where
  analyse = undefined

instance LinearlyTypeable Inversion where
  analyse = undefined

-- class Environemental m where
--   infer :: m a -> Either [LinearTypeError a] [(X, T)]
