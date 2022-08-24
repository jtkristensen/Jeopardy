module TestsForArgumentsAndCallsAnalysis
  ( problematicArgumentOrCallAnalysisTests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Syntax ( Program )
import Core.Parser (Source, parseString, program_)

import ExamplePrograms

import Control.Monad (void)

import Analysis.ArgumentsAndCalls

problematicArgumentOrCallAnalysisTests :: TestTree
problematicArgumentOrCallAnalysisTests =
  testGroup "Unit tests about arguments and calls analysis."
    [ numberOfArgumentsTests
    ]

numberOfArgumentsTests :: TestTree
numberOfArgumentsTests =
  testGroup "Tests about lookups in the program text."
  [ testCase "the [zero] constructor takes no arguments" $
    fromExample firstProgram $
    \first ->  numberOfArguments "zero" first @?= return 0
  , testCase "the [suc _] constructor takes one arguments" $
    fromExample firstProgram $
    \first ->  numberOfArguments "suc" first @?= return 1
  , testCase "the [pair _ _] constructor takes two arguments" $
    fromExample firstProgram $
    \first ->  numberOfArguments "pair" first @?= return 2
  , testCase "Nothing to be said about constructors that are not declared." $
    fromExample firstProgram $
    \first ->  numberOfArguments "wut" first @?= Nothing
  ]

fromExample :: Source -> (Program () -> Assertion) -> Assertion
fromExample src f =
  case parseString program_ src of
    Right p -> f $ void p
    _       -> assertBool ("unable to parse " ++ src) False
