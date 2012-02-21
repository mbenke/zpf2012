# Testowanie programów w Haskellu
* HUnit
* Quickceck

# HUnit

~~~~ {.haskell}
import Test.HUnit

run = runTestTT tests
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)
~~~~

~~~~
*Main Test.HUnit> run
Cases: 2  Tried: 2  Errors: 0  Failures: 0
Counts {cases = 2, tried = 2, errors = 0, failures = 0}

*Main Test.HUnit> :t runTestTT
runTestTT :: Test -> IO Counts
~~~~