module
  TestsForCoreParser
    ( coreParserTests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad (void)

import Core.Syntax
import Core.Parser
import Data.Either

coreParserTests :: TestTree
coreParserTests =
  testGroup "Unit tests about parsing the Core Jeopardy language."
    [ positivePatterns
    , negativePatterns
    , positivePrograms
    ]

positivePrograms :: TestTree
positivePrograms =
  testGroup "Positive tests for parsing programs"
  [ testCase "Identity function." $
    positive program_
      "id (x : nat) : nat = x . main id ." $
    Function "id"
      (          Variable Ordinary "x" (), "nat")
      (Pattern $ Variable Ordinary "x" (), "nat") $
    Main (Conventional "id" ())
  , testCase "Swap program" $
    positive program_
      ( "data nat = [suc nat] [zero].\n"  ++
        "data pair = [pair nat nat] .\n " ++
        "first ([pair a b] :pair): nat= a\n." ++
        "second([pair a b]: pair) :nat = b .\n " ++
        "swap (p : pair) : pair = \n" ++
        "case first p : nat of" ++
        "  ; a -> " ++
        "case second p : nat of" ++
        "  ; b -> [pair b a]." ++
        "main swap ."
      ) $
    Data "nat" [("suc",["nat"]),("zero",[])] $
    Data "pair" [("pair",["nat","nat"])] $
    Function "first"
      (Constructor "pair" [Variable Ordinary "a" (),Variable Ordinary "b" ()] (),"pair")
      (Pattern (Variable Ordinary "a" ()),"nat") $
    Function "second"
      (Constructor "pair" [Variable Ordinary "a" (),Variable Ordinary "b" ()] (),"pair")
      (Pattern (Variable Ordinary "b" ()),"nat") $
    Function "swap" (Variable Ordinary "p" (),"pair")
      (Case (Application (Conventional "first" ()) (Variable Ordinary "p" ()) (),"nat")
         [(Variable Ordinary "a" (),
           Case (Application (Conventional "second" ()) (Variable Ordinary "p" ()) (),"nat")
             [ (Variable Ordinary "b" ()
              , Pattern (Constructor "pair" [Variable Ordinary "b" (),Variable Ordinary "a" ()] ()))
             ] ())
         ] (),"pair") $
    Main (Conventional "swap" ())
  ]

positivePatterns :: TestTree
positivePatterns =
  testGroup "Positive tests for parsing patterns."
    [ testCase "Variable Pattern" $
      positive pattern_
        "x" $
      Variable Ordinary "x" ()
    , testCase "Existential Pattern" $
      positive pattern_
        "_x" $
      Variable Existential "_x" ()
    , let input = "_" in
        testCase "Underscore Pattern" $
        case parseString pattern_ input of
          Right (Variable Existential _ _) ->
            return ()
          _                       ->
            positive pattern_ input $ Variable Existential "<something>" ()
    , testCase "Underscore should be replaced by globally unique names" $
      case parseString pattern_ "_" of
        Right (Variable Existential x _) ->
          let inner_construction = "[" ++ x ++ " _]"
          in case parseString pattern_ inner_construction of
               Right (Constructor z [Variable Existential y _] _) | x == z ->
                 let final_construction = "[_ " ++ x ++ " " ++ y ++ "]"
                 in case parseString pattern_ final_construction of
                      Right (Constructor "_" [Variable Existential a _, Variable Existential b _] _) ->
                        assertBool (a ++ " should differ from " ++ b) (a /= b)
                      _ -> assertBool ("unable to parse " ++ final_construction) False
               _ -> assertBool ("unable to parse " ++ inner_construction) False
        _ -> assertBool "unable to parse _" False
    , testCase "Variable Pattern followed by space." $
      positive pattern_
        "x " $
      Variable Ordinary "x" ()
    , testCase "Several symbols in varaible name." $
      positive pattern_
        "abc-def" $
        Variable Ordinary "abc-def" ()
    , testCase "Simplest constructor." $
      positive pattern_
        "[nil]" $
        Constructor "nil" [] ()
    , testCase "Successor pattern." $
      positive pattern_
        "[suc n]" $
        Constructor "suc" [Variable Ordinary "n" ()] ()
    , testCase "Pair pattern" $
      positive pattern_
        "[pair [suc n] zero]" $
        Constructor "pair"
          [ Constructor "suc"
             [Variable Ordinary  "n" ()] ()
          , Variable Ordinary  "zero" ()
          ] ()
    ]

negativePatterns :: TestTree
negativePatterns =
  testGroup "Negative tests for parsing patterns."
    [ testCase "Space before variable pattern" $
      negative pattern_
        " x"
    , testCase "Illegal variable name" $
      negative pattern_
        "$x"
    , testCase "Keyword as variable name" $
      negative pattern_
        "invert"
    , testCase "empty constructor" $
      negative pattern_
        "[]"
    ]


-- * Utility

strip :: Functor f => f a -> f ()
strip = void

positive :: (Functor f, Eq (f ()), Show (f ())) => Parser (f a) -> String -> f () -> Assertion
positive p s a = strip <$> parseString p s @?= return (strip a)

negative :: Functor f => Parser (f a) -> String -> Assertion
negative p s = assertBool "should not parse" $ isLeft $ strip <$> parseString p s

