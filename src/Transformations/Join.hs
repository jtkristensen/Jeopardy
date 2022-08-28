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

class Joinable m where
  join :: m a -> m b -> Maybe (m (a, b))

instance Joinable Program where
  join (Function f (p,  tp ) (t,  tt ) program )
       (Function g (p', tp') (t', tt') program') | f == g =
    do joined_p       <- join p p'
       joined_t       <- join t t'
       joined_program <- join program program'
       return $ Function f (joined_p, tp) (joined_t, tt) joined_program
  join (Data d dt  program)
       (Data t dt' program') | d == t =
    do joined_program <- join program program'
       return $ Data d dt joined_program
  join (Main f) (Main g) = join f g >>= return . Main
  join _ _ = Nothing

instance Joinable Pattern where
  join (Variable k x a)     (Variable _ _ b)              = Just (Variable k x (a, b))
  join (Constructor c _  _) (Constructor k _  _) | c /= k = Nothing
  join (Constructor c ps a) (Constructor _ qs b)          =
    do ps' <- mapM (uncurry join) (zip ps qs)
       return $ Constructor c ps' (a, b)

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
