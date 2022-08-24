
module TestsForDefinitionsAnalysis
  ( definitionsAnalysisTests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Syntax
import Core.Parser     ( Source, parseString, program_ )
import ExamplePrograms ( swapProgram, firstProgram )

import Control.Monad (void)

import Analysis.Definitions

-- import Data.Either
import Data.List (sort)

definitionsAnalysisTests :: TestTree
definitionsAnalysisTests =
  testGroup "Unit tests checking for properties about definitions."
    [ areConflictFree
    , haveConflicts
    , lookupTests
    ]

areConflictFree :: TestTree
areConflictFree =
  testGroup "These programs are definitionally conflict free."
  [ testCase "The first program" $ isConflictFree firstProgram
  , testCase "The swap program"  $ isConflictFree swapProgram
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

lookupTests :: TestTree
lookupTests =
  testGroup "Tests about lookups in the program text."
  [ testCase "lookup an existing function definition" $
    functionExists
    ( "swap"
    , (Variable "p" (), "pair")
    , (Case (Application (Conventional "first" ()) (Variable "p" ()) (),"nat")
         [( Variable "a" ()
          , Case (Application (Conventional "second" ()) (Variable "p" ()) (), "nat")
              [(Variable "b" ()
               ,Pattern (Constructor "pair" [Variable "b" (),Variable "a" ()] ()))] ())] (), "pair"))
    swapProgram
  , testCase "lookup a non-existing function definition" $
    functionDoesNotExists "multiply" swapProgram
  , testCase "lookup an existing datatype definition" $
    datatypeExists
    ( "nat", [("zero", []), ("suc", ["nat"])] )
    swapProgram
  , testCase "lookup a non-existing datatype definition" $
    functionDoesNotExists "tuple" swapProgram
  , testCase "lookup suc constructor in the swap program" $
    constructorExists
    ( "suc", "nat" )
    swapProgram
  , testCase "lookup zero constructor in the swap program" $
    constructorExists
    ( "zero", "nat" )
    swapProgram
  , testCase "lookup pair constructor in the swap program" $
    constructorExists
    ( "pair", "pair" )
    swapProgram
  , testCase "lookup corresponding type for a non-existing datatype definition" $
    constructorDoesNotExists "one" swapProgram
  , testCase "lookup under ambiguous datatype name" $
    constructorDoesNotExists "pair" $
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
  , testCase "lookup unambiguous datatype name" $
    constructorExists ( "pair" , "pair" ) $
      "data int  = [zero] [pos nat] [negsuc nat].\n"  ++
      "data pair = [pair nat nat].\n"                ++
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

strip :: Functor f => f a -> f ()
strip = void

functionExists :: (F, (Pattern (), T), (Term (), T)) -> Source -> Assertion
functionExists (fname, pattern_, term_) program =
  lookupFunction fname . strip <$> parseString program_ program
  @?= Right (Just (pattern_, term_))

functionDoesNotExists :: F -> Source -> Assertion
functionDoesNotExists fname program =
  lookupFunction fname . strip <$> parseString program_ program
  @?= Right Nothing

datatypeExists :: (T, [(C, [T])]) -> Source -> Assertion
datatypeExists (dname, cts) program =
  lookupDatatype dname . strip <$> parseString program_ program
  @?= Right (Just cts)

-- TODO : improve tests with cases for this.
-- datatypeDoesNotExists :: T -> Source -> Assertion
-- datatypeDoesNotExists dname program_ =
--   lookupDatatype dname . strip <$> run program program_
--   @?= Right Nothing

constructorExists :: (C, T) -> Source -> Assertion
constructorExists (cname, t) program =
  lookupConstructorType cname . strip <$> parseString program_ program
  @?= Right (Just t)

constructorDoesNotExists :: T -> Source -> Assertion
constructorDoesNotExists cname program =
  lookupConstructorType cname . strip <$> parseString program_ program
  @?= Right Nothing


isConflictFree :: Source -> Assertion
isConflictFree = hasConflicts []

hasConflicts :: [ConflictingDefinitions] -> Source -> Assertion
hasConflicts cs s =
      sort . duplicateDefinitionsAnalysis . strip <$> parseString program_ s
  @?= return (sort cs)
