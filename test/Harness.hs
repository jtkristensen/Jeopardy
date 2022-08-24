import Test.Tasty

import TestsForCoreParser
import TestsForDefinitionsAnalysis

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Jeopardy - main test suite."
    [ coreParserTests
    , definitionsAnalysisTests
    ]
