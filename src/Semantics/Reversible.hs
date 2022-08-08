{-|

Module      : Semantics.Reversible
Description : A per function invertible semantics for Jeopardy.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

An interpreter for Jeopardy that only accepts linearly typable programs, and
on the other hand promises that functions are locally invertible.

-}

module Semantics.Reversible where
