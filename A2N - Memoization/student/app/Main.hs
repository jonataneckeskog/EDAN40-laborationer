module Main (main) where

import Memoization (fastLPS, s1, s2)

main :: IO ()
main = do
  putStrLn "Running the function..."
  putStrLn s1
  putStrLn (fastLPS s1)
  putStrLn s2
  putStrLn (fastLPS s2)
