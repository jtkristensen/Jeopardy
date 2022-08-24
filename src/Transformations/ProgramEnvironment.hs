{-|

Module      : Transformations.ProgramEnvironment
Description : Transforms a program into a program environment.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

We assume that `duplicateDefinitionsAnalysis` and
`problematicArgumentOrCallAnalysis` have both passed with no errors.  That
is, all function-, datatype- and constructor definitions are unique, and at
no point in the program is an undefined function called, nor is a
constructor applied to the wrong number of arguments and so on..

-}

module Transformations.ProgramEnvironment where

import Core.Syntax
import Analysis.Definitions
import Data.Maybe

data Environment m a
  = Environment
      { function     :: F -> m ((Pattern a, T), (Term a, T))
      , main         ::      m (Inversion a)
      , datatype     :: C -> m T
      , constructors :: T -> m [(C, [T])]
      }

programEnvironment :: Monad m => Program a -> Environment m a
programEnvironment p
  = Environment
      { function     = return . \f -> fromJust $ lookupFunction f p
      , main         = return                  $ lookupMain p
      , datatype     = return . \c -> fromJust $ lookupConstructorType c p
      , constructors = return . \t -> fromJust $ lookupDatatype t p
      }
