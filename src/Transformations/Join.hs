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

-- instance Joinable Program where
--   join (Function f (p,  tp ) (t,  tt ) program )
--        (Function _ (p', tp') (t', tt') program') =
--     do joined_p       <- join p p'
--        joined_t       <- join t t'
--        joined_program <- join program program'
--        return $ Function f (joined_p, tp) (joined_t, tt) joined_program
--   join (Data d dt  program)
--        (Data _ dt' program') =
--     do joined_program <- join program program'
--        return $ Data d dt joined_program
--   join (Main f) (Main _) = Main f
--   join _ _ = Nothing

-- instance Join Pattern where
--   join (Variable x a) (Variable y b) =
