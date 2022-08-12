import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import TestsForCoreParser

main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Jeopardy - main test suite."
    [ coreParserTests
    ]
