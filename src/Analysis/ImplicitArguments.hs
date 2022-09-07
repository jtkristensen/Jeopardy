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
import Data.List                 (sort)
import Control.Monad             (void)
import Transformations.Labeling  (fresh)
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

type Flow
  = ERWS Label [Visited] [Equality] [Call]

-- Remember that a certain call was made.
memoize :: Call -> Flow ()
memoize c = modify $ normalize . (c:)

-- Check if we already analysed the call.
recall :: Call -> Flow Bool
recall c = elem c <$> get

-- Clear memory of calls.
clear :: Flow ()
clear = put []

update :: [Visited] -> (Flow a -> Flow a)
update vs = local (\ws -> normalize $ vs <> ws)

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

leafs :: Term a -> [Term a]
leafs t@(Pattern     _) = [t]
leafs t@Application{}   = [t]
leafs (Case _ pts _)    = pts >>= leafs . snd

-- Returns an ordered list with out duplications.
normalize :: (Eq a, Ord a) => [a] -> [a]
normalize = nub' . sort
  where nub' (x : y : xys) | x == y =     nub' (y : xys)
        nub' (x : y : xys)          = x : nub' (y : xys)
        nub' xys                    =               xys

-- flips the direction from Up to Down or visa versa.
switch :: Direction -> Direction
switch Up = Down
switch _  = Up

nameAndDirection :: Inversion a -> (F, Direction)
nameAndDirection (Conventional f _) = (f, Down)
nameAndDirection (Invert       i _) = (f, switch d)
  where (f, d) = nameAndDirection i

-- returns the call corresponding to an inversion.
call :: Inversion Label -> Pattern Label -> Flow Call
call i p =
  do define           <- function <$> environment
     ((q, _), (t, _)) <- define f
     case d of
       Up -> Pattern p `equals` t
       _  -> p `equals` q
     Call f d (labels p) <$> ask
  where (f, d) = nameAndDirection i

class CanBeLabeled m where
  labels :: m Label -> [Label]

instance CanBeLabeled Pattern where
  labels (Variable    _ _  l) = [l]
  labels (Constructor _ ps l) = normalize $ l : (ps >>= labels)

instance CanBeLabeled Term where
  labels (Pattern p) = labels p
  labels (Application _ p l) = l : labels p
  labels (Case (t, _) pts l) = l : labels t <> (pts >>= (\(p, s) -> labels p <> labels s))

initiate :: Flow Call
initiate =
  do inversion <- environment >>= main
     call inversion (Variable Existential "_program_input" (-1))

analysis :: Call -> Flow ()
analysis c =
  do analyseCall c
     s <- get
     clear
     analyseCall $ c { direction = switch $ direction c }
     t <- get
     put (s <> t)

analyseCall :: Call -> Flow ()
analyseCall c =
  do b <- recall c
     if b
       then return ()
       else
       do memoize c
          ((p,_), (t,_)) <- environment >>= (\f -> f (name c)) . function
          case direction c of
            Down -> update (labels p) $ analyseTerm t
            Up   -> void $ unalyseTerm t

analyseTerm :: Term Label -> Flow ()
analyseTerm (Pattern _) = return ()
analyseTerm (Application i p l) =
  do c <- call i p
     update [l] $ analyseCall c
analyseTerm (Case (t, _) pts l) =
  update [l] $
  do analyseTerm t
     update (labels t) $ mapM_ (\(p, s) -> update (labels p) $ analyseTerm s) pts

unalyseTerm :: Term Label -> Flow [Label]
unalyseTerm (Pattern p)         = return $ labels p
unalyseTerm (Application _ _ l) = return [l]
unalyseTerm (Case (_t, _) _pts _l) = return []
  -- do lss <- mapM (\(p, s) ->
  --                   do sls <- unalyseTerm s
  --                      case t of
  --                        (Application i q l') ->
  --                          do c <- call (Invert i (meta i)) p
  --                             -- update [meta q, l] $ analyseCall c
  --                             return $ [meta q, l] <> labels p
  --                        _ -> return (meta t : sls <> labels p)
  --                ) pts
  --    return $ lss >>= id

hello :: Program a -> ((), [Call], [Equality])
hello program = (a, normalize w, s)
  where
    program'  = snd <$> fresh id program
    (a, w, s) = runERWS (initiate >>= analysis) program' [] []
