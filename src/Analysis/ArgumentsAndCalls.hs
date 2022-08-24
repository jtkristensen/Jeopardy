{-|

Module      : Analysis.ArgumentsAndCalls
Description : Properties about arguments and calls.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

We check that no constructor is applied to too many/few arguments,
and that no called function is not defined.

-}

module Analysis.ArgumentsAndCalls where

import Core.Syntax
import Analysis.Definitions

data ConflictingCall a
  = UnknownConstructor     C a
  | UnknownFunction        F a
  | WrongNumberOfArguments C a

-- This analysis assumes that `duplicateDefinitionsAnalysis` did not find
-- any problems.

-- problematicArgumentOrCallAnalysis :: Program a -> [ConflictingCall a]
-- problematicArgumentOrCallAnalysis p =


numberOfArguments :: C -> Program a -> Maybe Integer
numberOfArguments c p =
  do t   <- lookupConstructorType c p
     cts <- lookupDatatype        t p
     case snd <$> filter ((==c) . fst) cts of
       [ts] -> return $ fromIntegral $ length ts
       _    -> Nothing

