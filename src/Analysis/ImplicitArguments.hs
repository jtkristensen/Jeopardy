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
import Data.List     (sort, (\\))
import Control.Monad (void, join)
import Transformations.ProgramEnvironment

-- We keep track of which function we are in, and what labels we know from
-- previous calls.
type Label   = Integer
type Visited = Label
type Path    = [Visited]

data Direction = Up | Down
  deriving (Show, Eq, Ord)

data Call
  = Call
      { caller    :: F
      , callee    :: F
      , direction :: Direction
      , arguments :: [Label]
      , implicits :: [Label]
      }
  deriving (Show, Eq, Ord)

-- Implying a label, means that if it binds something outside the function
-- call, it will bind the an equal subexpressions under different labels or
-- different names inside the function call. So, the function "implies" serves
-- specify inter procedural equality.
type Implication = (Label, Label)

class Implicable m where
  implies :: m Label -> m Label -> Flow ()

newtype KnowledgeOf l = Knowing l

instance Implicable KnowledgeOf where
  (Knowing l1) `implies` (Knowing l2) = tell [(l1, l2)]

instance Implicable Pattern where
  implies (Variable _ _ l1) p = Knowing l1 `implies` Knowing (meta p)
  implies q (Variable _ _ l2) = Knowing (meta q) `implies` Knowing l2
  implies (Constructor c ps l1) (Constructor c' qs l2)
    | c == c' =
      do Knowing l1 `implies` Knowing l2
         mapM_ (uncurry implies) $ zip ps qs
  implies _ _ = return ()

instance Implicable Term where
  implies (Pattern p) (Pattern q) = p `implies` q
  implies s           t           =
    do Knowing (meta s) `implies` Knowing (meta t)
       mapM_ (uncurry implies) $ (,) <$> leafs s <*> leafs t

-- A `Flow` is a collection of call-paths through the program.  We stop
-- iterating when we recognize a function call. The only way calls can be
-- equal, is
type Flow = ERWS Label ReadOnly [Implication] [Call]

data ReadOnly =
  Reads
    { current   :: F
    , scope     :: Path
    , available :: Path
    }

-- Runs the analysis.
implicitArgumentsAnalysis :: Program Label -> ([Call], [Implication])
implicitArgumentsAnalysis program = (normalize w, normalize s)
  where (_, w, s) = runERWS analysis program (Reads "@main" [] []) []

-- Remember that a certain call was made.
memoize :: Call -> Flow ()
memoize c = modify $ normalize . (c:)

-- Check if we already analysed the call.
recall :: Call -> Flow Bool
recall c = elem c <$> get

-- Update the list of visited labels.
update :: Path -> (Flow a -> Flow a)
update vs = local $ \s -> s { scope = normalize $ scope s <> vs }

path :: Flow Path
path = ask >>= \s -> return $ normalize $ available s <> scope s

whoami :: Flow F
whoami = current <$> ask

-- Computes the "last" or "leaf" expressions of a term.
leafs :: Term a -> [Term a]
leafs t@(Pattern     _) = [t]
leafs t@Application{}   = [t]
leafs (Case _ pts _)    = pts >>= leafs . snd

-- Returns an ordered list without duplications.
normalize :: (Eq a, Ord a) => [a] -> [a]
normalize = nub' . sort
  where nub' (x : y : xys) | x == y =     nub' (y : xys)
        nub' (x : y : xys)          = x : nub' (y : xys)
        nub' xys                    =               xys

-- Flips the direction from Up to Down or visa versa.
switch :: Direction -> Direction
switch Up = Down
switch _  = Up

-- Computes the name and direction of a function inversion.
nameAndDirection :: Inversion a -> (F, Direction)
nameAndDirection (Conventional f _) = (f, Down)
nameAndDirection (Invert       i _) = (f, switch d)
  where (f, d) = nameAndDirection i

-- Returns the call corresponding to an inversion.
call :: Inversion Label -> Pattern Label -> Flow Call
call i p =
  do ((q, _), (t, _)) <- function <$> environment <?> f
     case d of
       Up -> t `implies` Pattern p
       _  -> p `implies` q
     g <- whoami
     Call g f d (labels p) <$> path
  where
    (f, d) = nameAndDirection i

-- Performs the analysis in both directions, starting at `main`.
analysis :: Flow ()
analysis =
  do cIn  <- environment >>= main >>= \i -> call i input
     cOut <- environment >>= main >>= \i -> call (Invert i (-3)) output
     analyseCall cIn
     analyseCall cOut
  where
    input  = Variable Ordinary "input"  (-1)
    output = Variable Ordinary "output" (-2)

-- Considers a single function call.
analyseCall :: Call -> Flow ()
analyseCall c =
  do known <- recall c
     if known
       then return ()
       else do memoize c
               ((p,_), (t,_)) <- function <$> environment <?> callee c
               let body  = labels p <> labels t
               implicit <- ask
               local (const $
                      implicit
                       { current   = callee c
                       , available = (available implicit \\ body) <> scope implicit
                       , scope     = [] }) $
                 case direction c of
                   Down -> update (labels p) $ analyseTerm t
                   Up   -> void              $ unalyseTerm t

-- Analyses a term as interpreted in the conventional direction.
analyseTerm :: Term Label -> Flow ()
analyseTerm (Pattern _) = return ()
analyseTerm (Application i p l) =
  do c <- call i p
     update [l] $ analyseCall c
analyseTerm (Case (t, _) pts l) =
  update [l] $
  do analyseTerm t
     update (labels t) $ mapM_ (\(p, s) -> update (labels p) $ analyseTerm s) pts

-- Analyses a term in a "bottom-up" fashion. (starting at leafs).
unalyseTerm :: Term Label -> Flow [Label]
unalyseTerm (Pattern p)            = return $ labels p
unalyseTerm (Application i p l)    =
  do c <- call i p
     update [l] $
       analyseCall (c { arguments = [l]
                      , direction = switch (direction c)
                      })
     return [l]
unalyseTerm (Case (t, _) pts _l) =
  do lss <- mapM (\(p, s) ->
                    do sls <- unalyseTerm s
                       update (sls <> labels p) $ unalyseTerm t
                 ) pts
     return $ join lss
