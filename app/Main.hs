
module Main where

-- In the future, the driver program goes here, but for now it is just used
-- for experimentation {^o^}.

import Core.Parser   (Source, parseString, program_)
import Control.Monad (void)
import Transformations.Labeling
import Analysis.AvailableExpressions

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _         = undefined

fibProgram :: Source
fibProgram =
  "data nat  = [zero] [suc nat]."                    ++
  "data pair = [pair nat nat]."                      ++
  "first  ([pair a _] : pair) : nat = a."            ++
  "second ([pair _ b] : pair) : nat = b."            ++
  "add (p : pair) : nat ="                           ++
  " case second p : nat of"                          ++
  " ; n ->"                                          ++
  "   case first p : nat of"                         ++
  "   ; [zero]  -> n"                                ++
  "   ; [suc k] -> add [pair k [suc n]]."            ++
  "fibber (p : pair) : pair ="                       ++
  "  case first p : nat of"                          ++
  "  ; m ->"                                         ++
  "    case add p : nat of"                          ++
  "    ; sum -> [pair sum m]."                       ++
  "fib_pair (n : nat) : pair ="                      ++
  " case n : nat of"                                 ++
  " ; [zero ]   -> [pair [suc [zero]] [suc [zero]]]" ++
  " ; [suc n-1] ->"                                  ++
  "   case fib_pair n-1 : pair of"                   ++
  "   ; p -> fibber p."                              ++
  "fib (n : nat) : pair ="                           ++
  "  case fib_pair n : pair of"                      ++
  "  ; p ->"                                         ++
  "    case first p : nat of"                        ++
  "    ;  fib-n -> [pair n fib-n]."                  ++
  "main fib."

unswapProgram :: Source
unswapProgram =
  "data nat  = [zero] [suc nat]."                    ++
  "data pair = [pair nat nat]."                      ++
  "first  ([pair a _] : pair) : nat = a."            ++
  "second ([pair _ b] : pair) : nat = b."            ++
  "unswap ([pair b a] : pair) : pair ="              ++
  "  case (invert second) b : pair of"               ++
  "  ; _p ->"                                        ++
  "    case (invert first) a : pair of"              ++
  "    ; _p -> _p."                                  ++
  "main unswap."

main :: IO ()
main =
  print $
  fmap snd $
  fresh id $
  void $
  fromRight $
  parseString program_ $
  unswapProgram

-- main = print "Driver not yet implemented."
