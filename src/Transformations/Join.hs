{-|

Module      : Transformations.Join.
Description : Join the meta data of two structurally equivalent syntax trees
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

-}


module Transformations.Join where

import Core.Syntax
import Control.Monad  ( zipWithM )
import Data.Bifunctor ( bimap    )

class Joinable m where
  join :: m a -> m b -> Maybe (m (a, b))

instance Joinable Program where
  join (Function f (p,  tp ) (t,  tt ) program )
       (Function g (p', tp') (t', tt') program') | f == g && tp == tp' && tt == tt' =
    do joined_p       <- join p p'
       joined_t       <- join t t'
       joined_program <- join program program'
       return $ Function f (joined_p, tp) (joined_t, tt) joined_program
  join (Data d dt  program)
       (Data t dt' program') | d == t && dt == dt' =
    do joined_program <- join program program'
       return $ Data d dt joined_program
  join (Main f) (Main g) = Main <$> join f g
  join _ _ = Nothing

instance Joinable Pattern where
  join (Variable k x a)     (Variable _ _ b)               = Just (Variable k x (a, b))
  join (Constructor c ps a) (Constructor k qs b) | c == k  =
    do ps' <- zipWithM join ps qs
       return $ Constructor c ps' (a, b)
  join _ _ = Nothing

instance Joinable Inversion where
  join (Conventional f a) (Conventional _ b) = return $ Conventional f (a, b)
  join (Invert       i a) (Invert       j b) =
    do ij <- join i j
       return $ Invert ij (a, b)
  join _ _ = Nothing

instance Joinable Term where
  join (Pattern p) (Pattern q) = Pattern <$> join p q
  join (Application i p a) (Application j q b) =
    Application <$> join i j <*> join p q <*> pure (a, b)
  join (Case (d, td) pts a) (Case (t, tt) qts b) | td == tt =
    do dt   <- join d t
       pqts <- mapM (\((v, x), (u, y)) ->
                       do w <- join v u
                          z <- join x y
                          return (w, z)) $
               zip pts qts
       return $ Case (dt, td) pqts (a, b)
  join _ _ = Nothing

class CoJoinable m where
  cojoin :: m (a, b) -> (m a, m b)

instance CoJoinable Program where
  cojoin (Function f p t program) =
    let g x y   = (y, snd x)
        (q, q') = bimap (g p) (g p) $ cojoin (fst p)
        (s, s') = bimap (g t) (g t) $ cojoin (fst t)
        (r, r') = cojoin program
    in (Function f q s r, Function f q' s' r')
  cojoin (Data d cts program) =
    let (r, r') = cojoin program
    in (Data d cts r, Data d cts r')
  cojoin (Main i) =
    let (j, j') = cojoin i
    in (Main j, Main j')

instance CoJoinable Pattern where
  cojoin (Variable k x (a, b)) =
    (Variable k x a, Variable k x b)
  cojoin (Constructor c ps (a, b)) =
    let (qs, qs') = unzip $ map cojoin ps
    in (Constructor c qs a, Constructor c qs' b)

instance CoJoinable Term where
  cojoin (Pattern p) = bimap Pattern Pattern $ cojoin p
  cojoin (Application i p (a, b)) =
    let (j, j') = cojoin i
        (q, q') = cojoin p
    in (Application j q a, Application j' q' b)
  cojoin (Case (t, dt) pts (a, b)) =
    let (s, s') = cojoin t
        (qts, qts') =
          unzip $ map ((\((l, l'), (r, r')) -> ((l, r), (l', r')))
                . bimap cojoin cojoin) pts
    in (Case (s, dt) qts a, Case (s', dt) qts' b)

instance CoJoinable Inversion where
  cojoin (Conventional f (a, b)) =
    (Conventional f a, Conventional f b)
  cojoin (Invert i (a, b)) =
    let (j, j') = cojoin i
    in (Invert j a, Invert j' b)
