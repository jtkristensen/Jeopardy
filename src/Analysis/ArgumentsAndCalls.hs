{-|

Module      : Analysis.ArgumentsAndCalls
Description : Properties about arguments and calls.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

We check that no constructor is applied to too many/few arguments,
and that no called function is not defined.

-}

module Analysis.ArgumentsAndCalls where

import Core.Syntax
import Analysis.Definitions

import Data.Maybe     ( fromJust )
import Data.Bifunctor ( bimap    )

data ConflictingCall a
  = UnknownConstructor     C a
  | UnknownFunction        F a
  | WrongNumberOfArguments C a
  deriving (Eq, Ord, Show)

-- This analysis assumes that `duplicateDefinitionsAnalysis` did not find
-- any problems.

problematicArgumentOrCallAnalysis :: Program a -> [ConflictingCall a]
problematicArgumentOrCallAnalysis program = argument_errors <> call_errors
  where
    argument_errors =
      do p <- patterns program
         case p of
           Constructor c' ps a ->
             case lookupConstructorType c' program of
               Nothing -> return $ UnknownConstructor c' a
               _       ->
                 case numberOfArguments c' program of
                   Just n | n == length ps -> []
                   _                       -> return $ WrongNumberOfArguments c' a
           _ -> []
    call_errors =
      do (f, a) <- called program
         case lookupFunction f program of
           Just _ -> []
           _      -> return $ UnknownFunction f a

-- States the number of arguments required by a constructor in a particular program.
numberOfArguments :: C -> Program a -> Maybe Int
numberOfArguments c program =
  do t   <- lookupConstructorType c program
     cts <- lookupDatatype        t program
     case snd <$> filter ((==c) . fst) cts of
       [ts] -> return $ length ts
       _    -> Nothing

-- Lists all equations of a program (without their names).
equations :: Program a -> [(Pattern a, Term a)]
equations p =
  bimap fst fst . fromJust <$> (flip lookupFunction p <$> functionNames p)

-- Enumerates all subpatterns of something.
class Patterns m where
  patterns :: m a -> [Pattern a]

instance Patterns Program where
  patterns p = (args >>= patterns) ++ (bodies >>= patterns)
    where (args, bodies) = unzip $ equations p

instance Patterns Pattern where
  patterns p@Variable{ }          = return p
  patterns p@(Constructor _ ps _) = p : (ps >>= patterns)

instance Patterns Term where
  patterns (Pattern         p) = patterns p
  patterns (Application _ p _) = patterns p
  patterns (Case (t, _) pts _) =
    patterns t               ++
    (pts >>= patterns . fst) ++
    (pts >>= patterns . snd)

-- Enumerates the names of functions called inside something.
class Called m where
  called :: m a -> [(F, a)]

instance Called Program where
  called p =
    called (lookupMain p) ++
    (equations p >>= called . snd)

instance Called Term where
  called (Pattern         _) = []
  called (Application i _ _) = called i
  called (Case (t, _) pts _) = called t ++ (pts >>= called . snd)

instance Called Function where
  called (Conventional f a) = [(f, a)]
  called (Invert       i _) = called i
