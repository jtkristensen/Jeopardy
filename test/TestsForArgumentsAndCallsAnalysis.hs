module TestsForArgumentsAndCallsAnalysis
  ( problematicArgumentOrCallAnalysisTests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Syntax ( Program )
import Core.Parser (Source, parseString, program_)

import ExamplePrograms

import Control.Monad (void)
import Data.List     (sort)

import Analysis.ArgumentsAndCalls

problematicArgumentOrCallAnalysisTests :: TestTree
problematicArgumentOrCallAnalysisTests =
  testGroup "Unit tests about arguments and calls analysis."
    [ numberOfArgumentsTests
    , positiveCallsToConstructorsAndFunctions
    , negativeCallsToConstructorsAndFunctions
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

positiveCallsToConstructorsAndFunctions :: TestTree
positiveCallsToConstructorsAndFunctions =
  testGroup "These programs shouldn't have problems"
  [ testCase "No problems in swap example" $
    fromExample swapProgram $
    \swap -> problematicArgumentOrCallAnalysis swap @?= []
  , testCase "No problems in first example" $
    fromExample firstProgram $
    \first -> problematicArgumentOrCallAnalysis first @?= []
  ]

negativeCallsToConstructorsAndFunctions :: TestTree
negativeCallsToConstructorsAndFunctions =
  testGroup "These programs have problems"
  [ testCase "Constructors in case patterns" $
    fromExample
    ( "data nat  = [zero] [suc k]."       ++
      "data pair = [pair nat nat]."       ++
      "add ([pair m n] : pair) : nat = "  ++
      " case m : nat of"                  ++
      " ; [z]   -> n"                     ++
      " ; [suc] -> add [pair _ [suc m]]." ++
      "main (invert add)."
    ) $
    \program -> sort (problematicArgumentOrCallAnalysis program)
    @?=         sort [ UnknownConstructor "z" ()
                     , WrongNumberOfArguments "suc" ()
                     ]
  , testCase "Constructors in case terms" $
    fromExample
    ( "data nat  = [zero] [suc k]."           ++
      "data pair = [pair nat nat]."           ++
      "add ([pair m n] : pair) : nat = "      ++
      " case [s m] : nat of"                  ++
      " ; [zero]   -> [n]"                    ++
      " ; [suc k] -> add [pair k [suc n] _]." ++
      "main (invert add)."
    ) $
    \program -> sort (problematicArgumentOrCallAnalysis program)
    @?=         sort [ UnknownConstructor "s" ()
                     , UnknownConstructor "n" ()
                     , WrongNumberOfArguments "pair" ()
                     ]
  , testCase "Applications and arguments" $
    fromExample
    ( "data nat  = [zero] [suc k]."           ++
      "data pair = [pair nat nat]."           ++
      "add ([pair m n f] : triple) : nat = "  ++
      " case m : nat of"                      ++
      " ; [zero]   -> n"                      ++
      " ; [suc k]  -> plus [pair k [suc n]]." ++
      "main (invert (invert sum))."
    ) $
    \program -> sort (problematicArgumentOrCallAnalysis program)
    @?=         sort [ UnknownFunction "sum" ()
                     , UnknownFunction "plus" ()
                     , WrongNumberOfArguments "pair" ()
                     ]
  ]


fromExample :: Source -> (Program () -> Assertion) -> Assertion
fromExample src f =
  case parseString program_ src of
    Right p -> f $ void p
    _       -> assertBool ("unable to parse " ++ src) False
