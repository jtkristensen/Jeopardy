module TestConfig where

-- This is an argument for QuickCheck, if it is larger, then the testcases
-- are larger, but they take significantly more time to run.
sizeOfGeneratedPatterns :: Int
sizeOfGeneratedPatterns = 5

-- This decides how many testcases QuickCheck should run.
numberOfTests :: Int
numberOfTests = 100
