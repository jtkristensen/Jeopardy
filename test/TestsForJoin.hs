
module TestsForJoin where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Core.Syntax
import Generators (AnyPattern(..))
import Transformations.Labeling
import Transformations.Join

import Control.Monad.State (evalState)
import Data.Maybe (fromJust)

import qualified TestConfig as Config

type Law = Precondition String Integer -> Bool

-- *| Properties:

newtype Precondition a b = Precondition (Fun a b, Pattern a)
  deriving (Show)

instance ( QC.Function a
         , Arbitrary a
         , CoArbitrary a
         , Arbitrary b ) =>
         Arbitrary (Precondition a b)
  where
  arbitrary =
    do (Fun _ f) <- arbitrary
       p         <- evalState . annotateWithIntegers . unAP <$> arbitrary
       g         <- arbitrary
       return $ Precondition (g, f . snd <$> p 0)

patternCombination :: Law
patternCombination (Precondition (Fun _ f, p)) =
  ((\a -> (a, f a)) <$> p) == fromJust (join p (f <$> p))

patternCoJoinability :: Law
patternCoJoinability (Precondition (Fun _ f, p)) =
  cojoin ((\a -> (a, f a)) <$> p) == (p, f <$> p)

-- TODO : Generalize to be a laws about programs (requires us to write a very
-- involved generator).

-- *| Exports:

laws :: [(String, Law)]
laws =
  [ ("Combination law for patterns",   patternCombination)
  , ("Uncombination law for patterns", patternCoJoinability)
  ]

qcProperties :: TestTree
qcProperties =
  testGroup "Tested by Quick Check" $
    map qc laws
  where
    qc (s, p) = uncurry QC.testProperty (s, withMaxSuccess Config.numberOfJoinTests p)

coreJoinableTests :: TestTree
coreJoinableTests =
  testGroup "Laws for Joinable."
    [ qcProperties
    ]

