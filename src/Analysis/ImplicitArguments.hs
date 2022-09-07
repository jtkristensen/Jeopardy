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
mem :: Call -> Flow ()
mem c = modify $ normalize . (c:)

-- Check if we already analysed the call.
recall :: Call -> Flow Bool
recall c = (elem c) <$> get

-- Clear memory of calls.
clear :: Flow ()
clear = put []

class Equatable m where
  equals :: m Label -> m Label -> Flow ()

instance Equatable Pattern where
  equals (Variable _ _ l1) p = tell [(l1, meta p)]
  equals q (Variable _ _ l2) = tell [(meta q, l2)]
  equals (Constructor c ps l1) (Constructor c' qs l2)
    | c == c' =
      do tell [(l1, l2)]
         mapM (\(p, q) -> p `equals` q) $ zip ps qs
         return ()
  equals _ _ = return ()

instance Equatable Term where
  equals (Pattern p) (Pattern q) = p `equals` q
  equals s           t           =
    do tell [(meta s, meta t)]
       void $ mapM (uncurry equals) $ (,) <$> leafs s <*> leafs t

leafs :: Term a -> [Term a]
leafs t@(Pattern     _)     = [t]
leafs t@(Application _ _ _) = [t]
leafs (Case _ pts _)        = (snd <$> pts) >>= leafs

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

name_and_direction :: Inversion a -> (F, Direction)
name_and_direction (Conventional f _) = (f, Down)
name_and_direction (Invert       i _) = (f, switch d)
  where (f, d) = name_and_direction i

-- returns the call corresponding to an inversion.
call :: Inversion Label -> Pattern Label -> Flow Call
call i p =
  do define           <- function <$> environment
     ((q, _), (t, _)) <- define f
     case d of
       Up -> (Pattern p) `equals` t
       _  -> p `equals` q
     Call f d (labels p) <$> ask
  where (f, d) = name_and_direction i

class CanBeLabeled m where
  labels :: m Label -> [Label]

instance CanBeLabeled Pattern where
  labels (Variable    _ _  l) = [l]
  labels (Constructor _ ps l) = normalize $ l : (ps >>= labels)

-- initiate :: Flow Call
-- initiate = environment >>= main >>= call

-- analysis :: Call -> Flow ()
-- analysis c@(f, direction, labels) =
--   do analyseCall c
--      clear
--      analyseCall (f, switch direction, labels)

-- analyseCall :: Call -> Flow ()
-- analyseCall (f, d, ls) =
--   do define           <- function <$> environment
--      ((p, _), (t, _)) <- define f
--      ls'              <- collectLabels p
--      local (const $ normalize $ ls <> ls') $ analyseTerm t

-- collectLabels :: Pattern Label -> Flow [Label]
-- collectLabels (Variable    _ _  l) = return [l]
-- collectLabels (Constructor _ ps l) =
--   do ls <- mapM collectLabels ps
--      return $ normalize $ foldl (<>) [l] ls

-- analyseTerm :: Term Label -> Flow ()
-- analyseTerm = undefined

-- hello program = runERWS (initiate >>= analysis) program' [] []
--   where
--     program'  = snd <$> fresh id program
--     -- (a, _, _) = runERWS (initiate >>= analysis) program' [] []
