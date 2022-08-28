module TestConfig where

-- TODO.. Sized should be a transformation on generators.

-- This is an argument for QuickCheck, if it is larger, then the testcases
-- are larger, but they take significantly more time to run.
sizeOfGeneratedPatterns :: Int
sizeOfGeneratedPatterns = 10

-- This decides how many testcases QuickCheck should run.
numberOfUnificationTests :: Int
numberOfUnificationTests = 100

-- ..
numberOfJoinTests :: Int
numberOfJoinTests = 20
