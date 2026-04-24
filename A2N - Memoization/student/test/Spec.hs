-- This module will use QuickCheck to test the validity of our optimizations
import Test.QuickCheck

import System.Exit (exitFailure, exitSuccess)

-- We import the memoization module
import Memoization

-- TESTING FIBONACCI

-- This is a function to generate
-- Integers between 1 and 20
-- (We'll use that to test our fibonacci functions)
smallInteger :: Gen Int
smallInteger = elements [1..20]

-- First tests,
-- Fibo 0 should always be 1
-- Fibo 1 should always be 1
-- (===) will print the values if left and right aren't equal
fibo0, fibo1 :: Property
fibo0 = fibo 0 === 0
fibo1 = fibo 1 === 1

-- Second test,
-- For any n above 2, fibo n == fibo (n-1) + fibo (n-2)
fiboDef :: Int -> Property
-- (==>) will only check the property if (n > 2) (otherwise, discard)
fiboDef n = n > 2 ==> fibo n === fibo (n-1) + fibo (n-2) -- Very costly, runs fibo three times

-- We will only test our property with tiny values of n
-- (Otherwise the tests take too long)
-- We use smallInteger to generate small random n's to try
-- "forAllShrink" is a function which checks the property with the provided generator
-- (here, 'smallInteger')
-- "shrink" is a function which will try to find small counter-examples.
quickFiboDef :: Property
quickFiboDef = forAllShrink smallInteger shrink fiboDef

-- Quickcheck is great for checking optimizations
-- We throw random data to the two functions and check they return the same results!
-- (Note we have to use tiny numbers, because "fibo" can take a long time)...
fiboEquivalence :: (Int -> Int) -- The optimized function we want to test
  -> Int  -- A random integer
  -> Property -- The result of the equality check
fiboEquivalence fastFibo n =
  -- Fibo is the slow function
  -- fastFibo is the fast one (a parameter, since we'll try with different versions)
  -- Our test is that both functions return the same result
  -- If there's a conter-example, we'll print the results of the two functions
  fibo n === fastFibo n

-- The function above would be slow if we generated
-- numbers that are too big, so we make a variant
-- Which will use a generator for small integers
-- to generate a small integer, and test with fiboEquivalence
fastFiboEquivalence :: (Int -> Int) -> Property
fastFiboEquivalence = forAllShrink smallInteger shrink . fiboEquivalence

-- TESTING LONGEST PALYNDROMIC SEQUENCE
-- We need a generator for creating small strings
-- (Again, the reference implementation is slow).
letterString :: Gen String
letterString = resize 20 $ listOf $ elements ['a' .. 'z']

-- First test, lps "" == ""
lpsEmpty :: Property
lpsEmpty = lps "" === ""

-- Second test: The lps returns a palyndrome
lpsPalindrome :: String -> Property
lpsPalindrome s =
  lps s === reverse (lps s)

-- Third test: If we give the function a palindrome
-- It returns the full string
lpsPalindrome2 :: String -> Property
lpsPalindrome2 s =
  let palindrome = s ++ (reverse s)
  in lps palindrome === palindrome

-- We check that the slow function returns the same results
-- As the fast one
lpsEquivalence :: String -> Property
lpsEquivalence s = lps s === fastLPS s

tryWithShortStrings :: (String -> Property) -> Property
tryWithShortStrings p = forAll letterString p

safeQuickCheck :: Testable prop => prop -> IO ()
safeQuickCheck p = do
  result <- quickCheckResult p
  if isSuccess result then return () else exitFailure

-- Main: Run the test suite.
main :: IO ()
main = do
  putStrLn ""
  putStrLn "Testing Fibonacci..."
  safeQuickCheck fibo0
  safeQuickCheck fibo1
  safeQuickCheck quickFiboDef
  -- Test the three functions
  safeQuickCheck (fastFiboEquivalence fastFibo1)
  safeQuickCheck (fastFiboEquivalence fastFibo2)
  safeQuickCheck (fastFiboEquivalence fastFibo3)
  putStrLn "Testing LPS..."
  safeQuickCheck lpsEmpty
  safeQuickCheck (tryWithShortStrings lpsPalindrome)
  safeQuickCheck (tryWithShortStrings lpsPalindrome2)
  safeQuickCheck (tryWithShortStrings lpsEquivalence)
  exitSuccess

-- If there is a problem, will print something like...

-- Testing Fibonacci...
-- *** Failed! Falsified (after 1 test and 5 shrinks):
-- 1 <- Inputs for which the property failed
-- 2 /= 1 <- the results of the function in the property
-- Testing LPS...
-- +++ OK, passed 100 tests.
