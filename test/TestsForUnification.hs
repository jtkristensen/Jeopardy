
module TestsForUnification where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Analysis.Unification (patternMatch, PatternMatch(..))
import Generators

import qualified TestConfig as Config

-- *| Properties:

type Unifies      = APairOfStructurallyEquvialentPatterns -> Bool
type DoesNotUnify = APairOfStructurallyDifferentPatterns  -> Bool

equivalentPatternsUnify :: Unifies
equivalentPatternsUnify (APOSEP (p, q)) =
  case patternMatch p q of
    NoMatch -> False
    _       -> True

substitutionIsIdempotent :: Unifies
substitutionIsIdempotent (APOSEP (p, q)) =
  case patternMatch p q of
    NoMatch   -> False
    MatchBy f ->   f p == f (f p)
                && f q == f (f q)

substitutionUnifies :: Unifies
substitutionUnifies (APOSEP (p, q)) =
  case patternMatch p q of
    NoMatch   -> False
    MatchBy f -> f p == f q

differentPatternsDontUnify :: DoesNotUnify
differentPatternsDontUnify (APOSDP (p, q)) =
  case patternMatch p q of
    NoMatch -> True
    _       -> False

testsOnAPOSEP :: [(String, Unifies)]
testsOnAPOSEP =
  [ ("Equivalent patterns always unify"              , equivalentPatternsUnify)
  , ("Substitution is idempotent"                    , substitutionIsIdempotent)
  , ("The pattern matching substitution is a unifier", substitutionUnifies)
  ]

testsOnAPOSDP :: [(String, DoesNotUnify)]
testsOnAPOSDP =
  [ ("Structurally different patterns never unify"  , differentPatternsDontUnify)
  ]

qcProperties :: TestTree
qcProperties =
  testGroup "Tested by Quick Check" $
    map qc testsOnAPOSEP ++
    map qc testsOnAPOSDP
  where
    qc (s, p) = uncurry QC.testProperty (s, withMaxSuccess Config.numberOfUnificationTests p)

-- *| Exports:

coreUnificationTests :: TestTree
coreUnificationTests =
  testGroup "Unification without existentials."
    [ qcProperties
    ]
