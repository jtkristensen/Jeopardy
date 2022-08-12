module
  TestsForCoreParser
    ( coreParserTests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Syntax
import Core.Parser
import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Error
import Data.Either

coreParserTests =
  testGroup "Unit tests about parsing the Core Jeopardy language."
    [ positivePatterns
    , negativePatterns
    ]

positivePatterns :: TestTree
positivePatterns =
  testGroup "Positive tests for parsing patterns."
    [ testCase "Variable Pattern" $
      positive pattern_
        "x" $
      Variable "x" ()
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

