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
import Data.List     (sort)
import Control.Monad (void, join)
import Transformations.ProgramEnvironment

type Label   = Integer
type Visited = Label

data Direction = Up | Down
  deriving (Show, Eq, Ord)

type Equality = (Label, Label)

data Call
  = Call
      { name      :: F
      , direction :: Direction
      , arguments :: [Label]
      , available :: [Label]
      }
  deriving (Show, Eq, Ord)

-- Equating things in this setting, means that we require them to bind equal
-- subexpressions under different labels or different names. The function
-- "equals" serves specify inter procedural equality.
class Equatable m where
  equals :: m Label -> m Label -> Flow ()

instance Equatable Pattern where
  equals (Variable _ _ l1) p = tell [(l1, meta p)]
  equals q (Variable _ _ l2) = tell [(meta q, l2)]
  equals (Constructor c ps l1) (Constructor c' qs l2)
    | c == c' =
      do tell [(l1, l2)]
         mapM_ (uncurry equals) $ zip ps qs
  equals _ _ = return ()

instance Equatable Term where
  equals (Pattern p) (Pattern q) = p `equals` q
  equals s           t           =
    do tell [(meta s, meta t)]
       mapM_ (uncurry equals) $ (,) <$> leafs s <*> leafs t

-- If something is labeled, we should be able to read of all of its labels.
class HasLabels m where
  labels :: m Label -> [Label]

instance HasLabels Pattern where
  labels (Variable    _ _  l) = [l]
  labels (Constructor _ ps l) = normalize $ l : (ps >>= labels)

instance HasLabels Term where
  labels (Pattern p) = labels p
  labels (Application _ p l) = l : labels p
  labels (Case (t, _) pts l) = l : labels t <> (pts >>= (\(p, s) -> labels p <> labels s))

-- A `Flow` is a collection of call-paths through the program.  We stop
-- iterating when we recognize a function call. The only way calls can be
-- equal, is
type Flow = ERWS Label [Visited] [Equality] [Call]

-- Runs the analysis.
implicitArgumentsAnalysis :: Program Label -> ([Call], [Equality])
implicitArgumentsAnalysis program = (normalize w, s)
  where (_, w, s) = runERWS analysis program [] []

-- Remember that a certain call was made.
memoize :: Call -> Flow ()
memoize c = modify $ normalize . (c:)

-- Check if we already analysed the call.
recall :: Call -> Flow Bool
recall c = elem c <$> get

-- Clear memory of calls.
clear :: Flow ()
clear = put []

-- Update the list of visited labels.
update :: [Visited] -> (Flow a -> Flow a)
update vs = local (\ws -> normalize $ vs <> ws)

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

-- Returns a call in which nothing is known about the arguments.
-- Alternatively to introducing a "dummy" call.
callable :: Inversion Label -> Flow Call
callable i = Call f d [] <$> ask
  where (f, d) = nameAndDirection i

-- Returns the call corresponding to an inversion.
call :: Inversion Label -> Pattern Label -> Flow Call
call i p =
  do Call f d _ a     <- callable i
     ((q, _), (t, _)) <- function <$> environment <?> f
     case d of
       Up -> Pattern p `equals` t
       _  -> p `equals` q
     return $ Call f d (labels p) a

-- Performs the analysis in both directions, starting at `main`.
analysis :: Flow ()
analysis =
  do c <- environment >>= main >>= callable
     analyseCall c
     s <- get
     clear
     analyseCall $ c { direction = switch $ direction c }
     t <- get
     put (s <> t)

-- Considers a single function call.
analyseCall :: Call -> Flow ()
analyseCall c =
  do known <- recall c
     if known
       then return ()
       else do memoize c
               ((p,_), (t,_)) <- function <$> environment <?> name c
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
unalyseTerm (Application i _ l)    =
  do c <- callable i
     update [l] $ analyseCall (c { arguments = [l] , direction = Up })
     return [l]
unalyseTerm (Case (t, _) pts l) =
  do lss <- mapM (\(p, s) ->
                    do sls <- unalyseTerm s
                       tell [(meta t, meta p), (meta s, l)]
                       update (sls <> labels p) $ unalyseTerm t
                 ) pts
     return $ join lss
