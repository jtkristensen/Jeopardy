
module ExamplePrograms where

import Core.Parser (Source)

firstProgram :: Source
firstProgram =
  "data nat  = [zero] [suc nat]."            ++
  "data pair = [pair nat nat]."              ++
  "first ([pair m n] : pair) : pair = m."    ++
  "main first."

swapProgram :: Source
swapProgram =
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
