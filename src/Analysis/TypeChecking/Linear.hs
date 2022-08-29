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

module Analysis.TypeChecking.Linear where

-- import Core.Syntax
-- import Analysis.Definitions
-- import Analysis.ArgumentsAndCalls
-- import Transformations.ProgramEnvironment

-- type Error a = Either (ConflictingCall a) ConflictingDefinitions

-- check :: Program a -> Either (Error a) (Program (a, T))
-- check = undefined
