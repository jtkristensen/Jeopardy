import Test.Tasty

import TestsForCoreParser
import TestsForDefinitionsAnalysis
import TestsForArgumentsAndCallsAnalysis
import TestsForUnification
import TestsForJoin

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Jeopardy - main test suite."
    [ coreParserTests
    , definitionsAnalysisTests
    , problematicArgumentOrCallAnalysisTests
    , coreUnificationTests
    , coreJoinableTests
    ]
