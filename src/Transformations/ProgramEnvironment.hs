{-|

Module      : Transformations.ProgramEnvironment
Description : Transforms a program into a program environment.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

We assume that `duplicateDefinitionsAnalysis` and
`problematicArgumentOrCallAnalysis` have both passed with no errors.  That
is, all function-, datatype- and constructor definitions are unique, and at
no point in the program is an undefined function called, nor is a
constructor applied to the wrong number of arguments and so on..

-}

module Transformations.ProgramEnvironment where

import Core.Syntax

import Analysis.Definitions
import Data.Maybe

import           Control.Arrow
import qualified Control.Monad.RWS as RWS

data Environment m a
  = Environment
      { function     :: F -> m ((Pattern a, T), (Term a, T))
      , main         ::      m (Function a)
      , datatype     :: C -> m T
      , constructors :: T -> m [(C, [T])]
      }

programEnvironment :: Monad m => Program a -> Environment m a
programEnvironment p
  = Environment
      { function     = return . \f -> fromJust $ lookupFunction f p
      , main         = return                  $ lookupMain p
      , datatype     = return . \c -> fromJust $ lookupConstructorType c p
      , constructors = return . \t -> fromJust $ lookupDatatype t p
      }

-- Convinient Monad: Environment Reader Writer State.
newtype ERWS e r w s a =
  ERWS { coERWS :: RWS.RWS (Environment (ERWS e r w s) e, r) w s a }

instance Monoid w => Monad (ERWS e r w s) where
  return  = ERWS . RWS.return
  m >>= f = ERWS $ coERWS m >>= coERWS . f

instance Monoid w => Applicative (ERWS e r w s) where
  pure = return
  e0 <*> e1 = e0 >>= \f -> f <$> e1

instance Monoid w => Functor (ERWS e r w s) where
  fmap f = ERWS . fmap f . coERWS

runERWS :: Monoid w => ERWS e r w s a -> Program e -> r -> s -> (a, s, w)
runERWS erws p r = RWS.runRWS (coERWS erws) (programEnvironment p, r)

-- Environment.
environment :: Monoid w => ERWS e r w s (Environment (ERWS e r w s) e)
environment = ERWS $ fst <$> RWS.ask

-- Reader.
local :: Monoid w => (r -> r) -> (ERWS e r w s b -> ERWS e r w s b)
local f = ERWS . RWS.local (second f) . coERWS

ask :: Monoid w => ERWS e r w s r
ask = ERWS $ snd <$> RWS.ask

-- Writer.
tell :: Monoid w => w -> ERWS e r w s ()
tell = ERWS . RWS.tell

-- State.
put :: Monoid w => s -> ERWS e r w s ()
put = ERWS . RWS.put

get :: Monoid w => ERWS e r w s s
get = ERWS RWS.get

modify :: Monoid w => (s -> s) -> ERWS e r w s ()
modify f = ERWS $ RWS.modify f

-- "Getter".
infixl 2 <?>

(<?>) :: Monad m => m (a -> m b) -> (a -> m b)
mf <?> a = mf >>= \f -> f a
