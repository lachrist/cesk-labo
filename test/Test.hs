import qualified EvaluateTest (tests)
import qualified ParseTest (tests)
import Test.HUnit (Counts, Test (TestList), runTestTT)

main :: IO Counts
main = runTestTT $ TestList [ParseTest.tests, EvaluateTest.tests]
