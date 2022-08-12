{-|

Module      : Analysis.Definitions.
Description : Properties about definitions in Jeopardy programs.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

This analysis contains the most general of analysis regarding definitions.

-}

module Analysis.Definitions where

import Core.Syntax
import Data.List      (nub)
import Data.Maybe     (fromJust)

-- Checks if a program contains dupilicate definitions.
hasDuplicateDefinitions :: Program a -> Bool
hasDuplicateDefinitions p =
  functionNames    p /= nub (functionNames    p) ||
  datatypeNames    p /= nub (datatypeNames    p) ||
  constructorNames p /= nub (constructorNames p)

-- The names of all functions defined in a program.
functionNames :: Program a -> [F]
functionNames (Function f _ _ p) = f : functionNames p
functionNames (Data     _ _   p) =     functionNames p
functionNames _                  = []

-- The names of all datatypes defined in a program.
datatypeNames :: Program a -> [T]
datatypeNames (Function _ _ _ p) =     datatypeNames p
datatypeNames (Data     c _   p) = c : datatypeNames p
datatypeNames _                  = []

-- The names of all data constructors in a program.
constructorNames :: Program a -> [C]
constructorNames p =
  do t <- datatypeNames p
     fst <$> fromJust (lookupDatatype t p)

-- Looks up a functions definitio. Assumes no duplicated definitions.
lookupFunction :: F -> Program a -> Maybe ((Pattern a, T), (Term a, T))
lookupFunction f (Function g p t _) | f == g = Just (p, t)
lookupFunction f (Function _ _ _ p)          = lookupFunction f p
lookupFunction f (Data     _ _   p)          = lookupFunction f p
lookupFunction _ _                           = Nothing

-- Looks up a datatypes definition. Assumes no duplicated definitions.
lookupDatatype :: T -> Program a -> Maybe [(C, [T])]
lookupDatatype f (Function _ _ _ p)          = lookupDatatype f p
lookupDatatype f (Data     g _   p) | f /= g = lookupDatatype f p
lookupDatatype _ (Data     _ cts _)          = Just cts
lookupDatatype _ _                           = Nothing

-- Looks up the name of the datatype a particular constructor bolongs to.
lookupConstructorType :: C -> Program a -> Maybe T
lookupConstructorType c p =
  case nub candidateTypes of
    [t] -> Just t
    _   -> Nothing
  where
    candidateTypes =
      do t <- datatypeNames p
         if     c `elem` (fst <$> fromJust (lookupDatatype t p))
           then return t
           else []
