{-|

Module      : Analysis.ImplicitArguments.
Description : At every call site, what interprocedural expressions are available as implicit arguments.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

-}

module Analysis.ImplicitArguments where

-- This module assumes that the input program passes all of
-- 1) 'definitions analysis'
-- 2) 'conflicting calls and arguments analysis', and
-- 3) 'type checking'

-- As such, we assume that all functions called in the program, have a
-- corresponding, unique definition. That no constructors are called with
-- too many or too few arguments, and that all type constructors unique, and
-- that not applications of such constructors, use names of constructors
-- that are not defined.

import Core.Syntax
import Transformations.ProgramEnvironment
import Transformations.Labeling

type L = Integer -- Labels.

type Vertex = (F, [[L]])
type Edge   = (Vertex, [L], Vertex)

data Graph =
  Graph { vertices :: [Vertex]
        , edges    :: [Edge] }
  deriving Show

type Flow a = ERWS a () () ()

analysis :: Flow a (Inversion a)
analysis =
  do inversion <- environment >>= main
     return inversion

hello program = a
  where
    program'   = snd <$> fresh id program
    (a, _, _) = runERWS analysis program' () ()

-- first  ([pair a _] : pair) : nat = a.

-- second ([pair _ b] : pair) : nat = b.

-- unswap ([pair b a] : pair) : pair =
--   case (invert second) b : pair of
--   ; _p ->
--     case (invert first) a : pair of
--     ; _p -> _p.

-- main unswap.

