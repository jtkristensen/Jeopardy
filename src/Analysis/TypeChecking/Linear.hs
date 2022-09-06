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
--  (check, LinearTypeError, ConflictingCall, ConflictingDefinitions )
where

import Core.Syntax
import Transformations.ProgramEnvironment
import Analysis.Definitions
import Analysis.ArgumentsAndCalls

import Transformations.Join ( join     )
import Data.Maybe           ( fromJust )
import Data.List            ( nub      )

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
  | IrregularPattern         F    [a]
  deriving Eq

-- Applies linear typing rules as described in the paper (Jeopardy : an
-- invertible programming language). Returns a list of errors if something
-- went wrong.
check :: Eq a => Program a -> Either [Error a] (Program (a, T))
check program =
  case ds ++ ac of
    [] ->
      case runERWS (analyse program) program "$>" mempty of
        (typedProgram, _, []) ->
          Right $ fromJust $ join program typedProgram
        (_, _, typeErrors)         ->
          Left (IllTyped <$> combine typeErrors)
    xs -> Left xs
  where
   ds  = map ErrorneousDefinition $ duplicateDefinitionsAnalysis      program
   ac  = map ErrorneousCall       $ problematicArgumentOrCallAnalysis program
   combine (ReusedRescource f x as : rest) =
     nub $ ReusedRescource f x (as <> (rest >>= reused f x)) : combine rest
   combine (IrregularPattern f as : rest) =
     nub $ IrregularPattern f (as <> (rest >>= irregular f)) : combine rest
   combine (err                    : rest) = err : combine rest
   combine [                             ] = []
   reused f x (ReusedRescource g y as) | f == g && x == y = as
   reused _ _ _                                           = []
   irregular f (IrregularPattern g as) | f == g           = as
   irregular _ _                                          = []



data State a =
  State { used    :: [(X, T, a)]
        , defined :: [(X, T, a)] }

instance Semigroup (State a) where
  s1 <> s2 = State (used s1 <> used s2) (defined s1 <> defined s2)

instance Monoid (State a) where
  mempty  = State [] []
  mappend = (<>)

putUsed, putDefined :: (X, T, a) -> Analysis a ()
putUsed    x = modify $ \s -> s { used    = x : used    s }
putDefined x = modify $ \s -> s { defined = x : defined s }

type Analysis a b = ERWS a F [LinearTypeError a] (State a) b

class LinearlyTypeable thing where
  analyse :: thing a -> Analysis a (thing T)

instance LinearlyTypeable Program where
  analyse = undefined
  -- analyse (Function f (p, pt) (t, tt) program) = undefined
  -- analyse (Data t ct program) = Data t ct <$> analyse program
  -- analyse (Main inversion)    = Main      <$> analyse inversion

instance LinearlyTypeable Pattern where
  analyse = undefined

instance LinearlyTypeable Term where
  analyse = undefined

instance LinearlyTypeable Inversion where
  analyse = undefined

-- class Environemental m where
--   infer :: m a -> Either [LinearTypeError a] [(X, T)]
