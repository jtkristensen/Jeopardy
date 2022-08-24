module
  TestsForCoreParser
    ( coreParserTests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Syntax
import Core.Parser
import Text.Parsec
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
    positive program
      "id (x : nat) : nat = x . main id ." $
    Function "id"
      (          Variable "x" (), "nat")
      (Pattern $ Variable "x" (), "nat") $
    Main (Conventional "id" ())
  , testCase "Swap program" $
    positive program
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
      (Constructor "pair" [Variable "a" (),Variable "b" ()] (),"pair")
      (Pattern (Variable "a" ()),"nat") $
    Function "second"
      (Constructor "pair" [Variable "a" (),Variable "b" ()] (),"pair")
      (Pattern (Variable "b" ()),"nat") $
    Function "swap" (Variable "p" (),"pair")
      (Case (Application (Conventional "first" ()) (Variable "p" ()) (),"nat")
         [(Variable "a" (),
           Case (Application (Conventional "second" ()) (Variable "p" ()) (),"nat")
             [(Variable "b" (), Pattern (Constructor "pair" [Variable "b" (),Variable "a" ()] ()))] ())] (),"pair") $
    Main (Conventional "swap" ())
  ]

positivePatterns :: TestTree
positivePatterns =
  testGroup "Positive tests for parsing patterns."
    [ testCase "Variable Pattern" $
      positive pattern_
        "x" $
      Variable "x" ()
    , testCase "Existential Pattern" $
      positive pattern_
        "_x" $
      Existential "_x" ()
    -- , testCase "Underscore Pattern" $
    --   positive pattern_
    --     "_" $
    --   Existential "x" ()
    , testCase "Variable Pattern followed by space." $
      positive pattern_
        "x " $
      Variable "x" ()
    , testCase "Several symbols in varaible name." $
      positive pattern_
        "abc-def" $
        Variable "abc-def" ()
    , testCase "Simplest constructor." $
      positive pattern_
        "[nil]" $
        Constructor "nil" [] ()
    , testCase "Successor pattern." $
      positive pattern_
        "[suc n]" $
        Constructor "suc" [Variable "n" ()] ()
    , testCase "Pair pattern" $
      positive pattern_
        "[pair [suc n] zero]" $
        Constructor "pair"
          [ Constructor "suc"
             [Variable "n" ()] ()
          , Variable "zero" ()
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

run :: Parser a -> Source -> Either ParseError a
run p = runParser p () "<no-source-file>"

strip :: Functor f => f a -> f ()
strip = fmap $ const ()

positive :: (Functor f, Eq (f ()), Show (f ())) => Parser (f a) -> String -> (f ()) -> Assertion
positive p s a = (strip <$> run p s) @?= (return (strip a))

negative :: Functor f => Parser (f a) -> String -> Assertion
negative p s = assertBool "should not parse" $ isLeft $ strip <$> run p s

