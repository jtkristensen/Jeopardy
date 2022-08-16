
module TestsForDefinitionsAnalysis
  ( definitionsAnalysisTests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Syntax
import Core.Parser

import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.Error

import Analysis.Definitions

-- import Data.Either
import Data.List (sort)

definitionsAnalysisTests =
  testGroup "Unit tests checking for properties about definitions."
    [ areConflictFree
    , haveConflicts
    ]

areConflictFree :: TestTree
areConflictFree =
  testGroup "These programs are definitionally conflict free."
  [ testCase "The first program"                 $
    isConflictFree                               $
      "data nat  = [zero] [suc nat]."            ++
      "data pair = [pair nat nat]."              ++
      "first ([pair m n] : pair) : pair = m."    ++
      "main first."
  , testCase "The swap program"                  $
    isConflictFree                               $
      "data nat  = [zero] [suc nat].\n"          ++
      "data pair = [pair nat nat].\n"            ++
      "first  ([pair m n] : pair) : pair = m.\n" ++
      "second ([pair m n] : pair) : pair = n.\n" ++
      "swap (p : pair) : pair = \n"              ++
      "  case first p : nat of \n"               ++
      "  ; a -> \n"                              ++
      "      case second p : nat of \n"          ++
      "      ; b -> [pair b a].\n"               ++
      "main swap."
  ]

haveConflicts :: TestTree
haveConflicts =
  testGroup "These programs are have problems."
  [ testCase "Multiple definitions of function (consecutive)" $
    hasConflicts [MultipleDefinitionsOfFunction "first"]      $
      "data nat  = [zero] [suc nat]."                         ++
      "data pair = [pair nat nat]."                           ++
      "first ([pair m n] : pair) : pair = m."                 ++
      "first ([pair m n] : pair) : pair = m."                 ++
      "main first."
  , testCase "Multiple definitions of function (appart)" $
    hasConflicts [MultipleDefinitionsOfFunction "first"] $
      "data nat  = [zero] [suc nat]."                    ++
      "data pair = [pair nat nat]."                      ++
      "first  ([pair m n] : pair) : pair = m."           ++
      "second ([pair m n] : pair) : pair = n."           ++
      "first  ([pair m n] : pair) : pair = m."           ++
      "main first."
  , testCase "Multiple definitions of function (several)" $
    hasConflicts [ MultipleDefinitionsOfFunction "first"
                 , MultipleDefinitionsOfFunction "second"
                 ]                               $
      "data nat  = [zero] [suc nat].\n"          ++
      "data pair = [pair nat nat].\n"            ++
      "first  ([pair m n] : pair) : pair = m.\n" ++
      "second ([pair m n] : pair) : pair = n.\n" ++
      "first  ([pair m n] : pair) : pair = m.\n" ++
      "swap (p : pair) : pair = \n"              ++
      "  case first p : nat of \n"               ++
      "  ; a -> \n"                              ++
      "      case second p : nat of \n"          ++
      "      ; b -> [pair b a].\n"               ++
      "second ([pair m n] : pair) : pair = n.\n" ++
      "main swap."
  , testCase "Multiple definitions of datatype (consecutive)" $
    hasConflicts [MultipleDefinitionsOfDatatype "pair"]       $
      "data nat  = [zero] [suc nat]."                         ++
      "data pair = [pair nat nat]."                           ++
      "data pair = [foo  nat nat]."                           ++
      "first ([pair m n] : pair) : pair = m."                 ++
      "main first."
  , testCase "Multiple definitions of datatype (appart)" $
    hasConflicts [MultipleDefinitionsOfDatatype "nat"]   $
      "data nat  = [zero1] [suc1 nat]."                  ++
      "data pair = [pair nat nat]."                      ++
      "first  ([pair m n] : pair) : pair = m."           ++
      "second ([pair m n] : pair) : pair = n."           ++
      "data nat  = [zero] [suc nat]."                    ++
      "main first."
  , testCase "Multiple definitions of datatype (several)" $
    hasConflicts [ MultipleDefinitionsOfDatatype "pair"
                 , MultipleDefinitionsOfDatatype "nat"
                 ] $
      "data nat  = [zero1] [suc1 nat].\n"        ++
      "data pair = [pair1 nat nat].\n"           ++
      "first  ([pair m n] : pair) : pair = m.\n" ++
      "second ([pair m n] : pair) : pair = n.\n" ++
      "swap (p : pair) : pair = \n"              ++
      "  case first p : nat of \n"               ++
      "  ; a -> \n"                              ++
      "      case second p : nat of \n"          ++
      "      ; b -> [pair b a].\n"               ++
      "data nat  = [zero] [suc nat].\n"          ++
      "data pair = [pair nat nat].\n"            ++
      "main swap."
  , testCase "Multiple definitions of constructor (consecutive)" $
    hasConflicts [MultipleDefinitionsOfConstructor "pair"]       $
      "data nat  = [zero] [suc nat]."                            ++
      "data pair = [pair nat nat]."                              ++
      "data foo  = [pair nat nat]."                              ++
      "first ([pair m n] : pair) : pair = m."                    ++
      "main first."
  , testCase "Multiple definitions of constructor (appart)"  $
    hasConflicts [MultipleDefinitionsOfConstructor "suc"]    $
      "data pair = [pair nat nat] [suc nat]."                ++
      "first  ([pair m n] : pair) : pair = m."               ++
      "second ([pair m n] : pair) : pair = n."               ++
      "data nat  = [zero] [suc nat]."                        ++
      "main first."
  , testCase "Multiple definitions of constructor (several)" $
    hasConflicts [ MultipleDefinitionsOfConstructor "zero"
                 , MultipleDefinitionsOfConstructor "pair"
                 ]                                    $
      "data int  = [zero] [pos nat] [negsuc nat].\n"  ++
      "data tuple = [pair nat nat].\n"                ++
      "first  ([pair m n] : pair) : pair = m.\n"      ++
      "second ([pair m n] : pair) : pair = n.\n"      ++
      "swap (p : pair) : pair = \n"                   ++
      "  case first p : nat of \n"                    ++
      "  ; a -> \n"                                   ++
      "      case second p : nat of \n"               ++
      "      ; b -> [pair b a].\n"                    ++
      "data nat  = [zero] [suc nat].\n"               ++
      "data pair = [pair nat nat].\n"                 ++
      "main swap."
  ]


-- * Utility

run :: Parser a -> Source -> Either ParseError a
run p = runParser p () "<no-source-file>"

strip :: Functor f => f a -> f ()
strip = fmap $ const ()

isConflictFree :: String -> Assertion
isConflictFree = hasConflicts []

hasConflicts :: [ConflictingDefinitions] -> String -> Assertion
hasConflicts cs s =
      sort . duplicateDefinitionsAnalysis . strip <$> run program s
  @?= return (sort cs)
