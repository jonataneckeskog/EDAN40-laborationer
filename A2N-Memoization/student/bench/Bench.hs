module Main where

-- This module is for benchmarking functions from the Memoization module.
-- It uses Criterion as a benchmarking harness

import Memoization
import Criterion.Main

s1 = "writers"
s2 = "vintner"
s3 = "thequickfox"
s4 = "kingofthehill"

main :: IO ()
-- The defaultMain is provided by criterion
-- We make benchmark groups
-- 'bench' creates a benchmark, with a label,
-- and 'whnf' will evaluate the expression it's provided, measuring how long it takes.

-- When we measure something, we don't run it once and measure the time,
-- We run it as many times as possible for a fixed time, count the iterations,
-- and give the mean / stddev.

main = defaultMain [
  -- bgroup "slowFib" [ bench "5" $ whnf fibo 5,
  --                    bench "9" $ whnf fibo 9,
  --                    bench "11" $ whnf fibo 11],
  -- bgroup "simple-cache" [ bench "5" $ whnf fastFibo1 5,
  --                         bench "9" $ whnf fastFibo1 9,
  --                         bench "11" $ whnf fastFibo1 11],
  -- bgroup "cache-lookup" [ bench "5" $ whnf fastFibo2 5,
  --                         bench "9" $ whnf fastFibo2 9,
  --                         bench "11" $ whnf fastFibo2 11],
  -- bgroup "with-memoize" [ bench "5" $ whnf fastFibo3 5,
  --                         bench "9" $ whnf fastFibo3 9,
  --                         bench "11" $ whnf fastFibo3 11],
  bgroup "lps" [ bench "writers" $ nf lps "writers",
                 bench "vintner" $ nf lps "vintner",
                 bench s3 $ nf lps s3,
                 bench s4 $ nf lps s4],
  bgroup "fastLPS" [ bench "writers" $ nf fastLPS "writers",
                     bench "vintner" $ nf fastLPS "vintner",
                     bench s3 $ nf fastLPS s3,
                     bench s4 $ nf fastLPS s4]
  ]
