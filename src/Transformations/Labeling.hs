{-|

Module      : Transformations.Labeling
Description : Annotates programs with various kinds of labels.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

-}

module Transformations.Labeling
  ( fresh
  , annotateWithIntegers )
where

import Core.Syntax

import Control.Arrow       ( second )
import Control.Monad.State ( modify, get, State, evalState )

-- Annotates a thing program with labels generated from unique integer labels.
fresh :: (Integer -> label) -> (Program a -> Program (a, label))
fresh f p = second f <$> evalState (annotateWithIntegers p) 0

-- Annotates a thing `m` with unique integer labels.
class IntegerAnnotatable m where
  annotateWithIntegers :: m a -> State Integer (m (a, Integer))

instance IntegerAnnotatable Program where
  annotateWithIntegers (Function f p t rest) =
    do p'    <- annotateWithIntegers (fst p)
       t'    <- annotateWithIntegers (fst t)
       rest' <- annotateWithIntegers rest
       return $ Function f (p', snd p) (t', snd t) rest'
  annotateWithIntegers (Data t cts rest) =
    Data t cts <$> annotateWithIntegers rest
  annotateWithIntegers (Main i) =
    Main <$> annotateWithIntegers i

instance IntegerAnnotatable Pattern where
  annotateWithIntegers (Variable x a)    = Variable x    . (,) a <$> bump
  annotateWithIntegers (Existential x a) = Existential x . (,) a <$> bump
  annotateWithIntegers (Constructor c ps a) =
    do i   <- bump
       ps' <- mapM annotateWithIntegers ps
       return $ Constructor c ps' (a, i)

instance IntegerAnnotatable Term where
  annotateWithIntegers (Pattern p) = Pattern <$> annotateWithIntegers p
  annotateWithIntegers (Application i p a) =
    do j <- bump
       i' <- annotateWithIntegers i
       p' <- annotateWithIntegers p
       return $ Application i' p' (a, j)
  annotateWithIntegers (Case (t0, t') pts a) =
    do i    <- bump
       t1   <- annotateWithIntegers t0
       pts' <- mapM (\(p, t2) ->
                       do p' <- annotateWithIntegers p
                          t3 <- annotateWithIntegers t2
                          return (p', t3)) pts
       return $ Case (t1, t') pts' (a, i)

instance IntegerAnnotatable Inversion where
  annotateWithIntegers (Conventional f a) =
    do Conventional f . (,) a <$> bump
  annotateWithIntegers (Invert i a) =
    do j <- bump
       Invert <$> annotateWithIntegers i <*> pure (a, j)

-- Simple utility.

bump :: State Integer Integer
bump = get >>= \i -> modify (+1) >> return i
