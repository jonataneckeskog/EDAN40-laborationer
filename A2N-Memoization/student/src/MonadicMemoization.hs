module MonadicMemoization where

import Control.Monad.State
import qualified Data.Map as Map

import Data.List (nub)

-- An example of 
-- memoized Fibonacci function using State monad
fastFibo3State :: Int -> Int
fastFibo3State n = evalState (fib n) Map.empty
  where
    fib :: Int -> State (Map.Map Int Int) Int
    fib 0 = return 0
    fib 1 = return 1
    fib k = do
      cache <- get -- get the cache map
      case Map.lookup k cache of
        Just result -> return result -- We found the result, return
        -- If we don't we need to calculate, and put in the map
        Nothing -> do 
          a <- fib (k-1)
          b <- fib (k-2)
          let result = a + b
          modify (Map.insert k result) -- Put in the map
          return result 

-- The same approach, for a function which gets all substring of a string
fastSubStrings :: String -> [String]
fastSubStrings s = evalState (substrings s) Map.empty
  where
    substrings :: String -> State (Map.Map String [String]) [String]
    substrings [] = return []
    substrings [x] = return [[], [x]]
    substrings s = do
      cache <- get
      case Map.lookup s cache of
        Just result -> return result
        Nothing -> do
            c1 <- substrings (drop 1 s)
            c2 <- substrings (init s)
            let result = nub $ c1 ++ c2 ++ [s]
            modify (Map.insert s result)
            return result

-- Example usage
main :: IO ()
main = do
  print $ fastFibo3State 35  -- Should be much faster than fibo 35
