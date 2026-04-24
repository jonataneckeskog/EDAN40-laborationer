module Memoization where

-- This is an assignment where we try to memoize functions
-- To speed up computations.

import Data.Maybe (fromJust)

-- The problem
-- If we write a recursive function, it may be slow
fibo :: Int -> Int
{- TO BE WRITTEN -}
fibo = undefined

-- In GHCI, do,
-- > :set +s
-- (Will print the time it takes to run something)
-- > fibo 30
-- You'll see this runs slow
runSlow :: Int
runSlow = fibo 30

-- We can make it faster, by creating a cache
-- This takes a function and creates an infinite list of the results
-- (This works because Haskell doesn't evaluate things before we need them)
mkCache :: (Num a , Enum a ) => (a -> b) -> [b]
mkCache f = map f [0..]

-- And we make a function to look for a value in a cache
evalCache :: [a] -> Int -> a
evalCache c n = c !! n

-- We create a cache for the fibonacci function
-- Where we assume we have a fast function available
-- Notice how the cache doesn't know anything about the n we'll look for
-- This will be important later.
fiboCache :: [Int]
fiboCache = mkCache fastFibo1

-- Now, we write a function that looks up in the cache instead
-- (Cache is infinite so we should never get a miss...)
fastFibo1 :: Int -> Int
fastFibo1 0 = 0
fastFibo1 1 = 1
fastFibo1 n =
  let fib = evalCache fiboCache
  in fib (n-1) + fib (n-2)

-- We can see the cache growing by doing, in GHCI
-- > :print fiboCache -- Prints just 'fiboCache = (_t1::[Integer])' since the list isn't evaluated yet
-- > fastFibo1 10
-- > :print fiboCache
-- Prints the list, and we can see we just evaluated what needed it.
runFast :: Int
runFast = fastFibo1 30

-- We can make another cache, that works with keys and values
-- We pass the function and the domain of the keys as an argument
listCache :: [a] -> (a -> b) -> [(a, b)]
{- TO BE WRITTEN -}
listCache domain f = undefined

-- We create a function which looks up the
-- result in the cache
-- and use fromJust to get an error if the cache misses.
listLookup :: Eq a => [(a, b)] -> a -> b
{- TO BE WRITTEN -}
listLookup cache value = undefined

-- Create the cache for all integers...
-- We use a 'fast fibonacci function' even if we haven't defined it yet!
fibCache :: [(Int, Int)]
{- TO BE WRITTEN -}
fibCache = undefined

-- And the fast function looks in the cache!
fastFibo2 :: Int -> Int
{- TO BE WRITTEN -}
fastFibo2 n = undefined

-- Pause:
-- We make the solution in two parts:
-- 1. We have a cache that uses a 'fast function' we don't have yet (but we know it's type!)
-- 2. We have a fast function that looks for the result in the cache
-- This wouldn't work in other languages, but because of lazy evaluation, it works, magic!

-- Using criterion, we can see that the fastFib function is a little slower than fasterFibo.
-- by using "stack bench --benchmark-arguments="--output bench.html"
-- And looking at the plots.

-- Now, for something cool
-- What if we make a function that creates the cache, and immediately looks in it?
memoizeWithList :: Eq a => [a] -> (a -> b) -> (a -> b)
memoizeWithList domain = listLookup . listCache domain

-- Maybe we can use it to memoize the old fibo function?
testMemoize :: Int -> Int
testMemoize n =
  let fibo2 = memoizeWithList [0..] fibo
  in fibo2 n

-- It doesn't work... because the old fibo function calls itself
-- So even if we use memoize, the recursive calls don't use the cache...

-- To fix this, we need open recursion.
-- It looks tricky, but it's simple
-- 1. Look at the recursive function
-- 2. Replace the function calls to calls to another function
-- 3. And his 'other function' is a function f, passed as a parameter.

-- Here is Fibonacci in open recursion
-- This function can do other things than compute fibonacci, but
-- It isn't really recursive anymore
-- And it's easy to implement fibonacci again: (openFib fibo) does that.
openFib :: (Int -> Int) -> Int -> Int
{- TO BE WRITTEN -}
openFib f n = undefined

-- We use openFib to create a cached function, and make sure
-- The recursive calls call the fast version!
fastFibo3 :: Int -> Int
fastFibo3 = memoizeWithList [0..] (openFib fastFibo3)

-- The memoize function creates the cache and looks in it immediately
-- And because of Lazy evaluation, we get a function that takes a slow
-- version, and returns a fast version.

-- LONGEST PALINDROMIC SUBSEQUENCE

-- Utility
dropLast :: [a] -> [a]
dropLast l = take (length l - 1) l

-- Slow version
lps :: String -> String
lps s = undefined

-- CACHES FOR LISTS OF THINGS

-- If we want caches for lists, it's more complicated...
-- Having a list of all possible lists will not work
-- (Cause we meet an infinity of strings like 'a', 'aa', 'aaaaaa'
-- when we look up)
-- Instead, we need a trie
-- A Trie node edge will store a value for the node, and edges
-- to its children, where each label has type 'edge'
-- 'node' and 'edge' can be any types.
data Trie node edge = Trie node [(edge, Trie node edge)]
  deriving Show

-- First, looking for a list in a trie...
trieLookup :: Eq e => Trie a e -> [e] -> a
{- TO BE WRITTEN -}
trieLookup t l = undefined

-- Get a subset of a trie, with limited depth
-- (Provided: Useful for debugging)
limitTrie :: Int -> Trie n e -> Trie n e
limitTrie 0 (Trie v _) = Trie v []
limitTrie n (Trie v edges) =
  Trie v [(l, limitTrie (n-1) t) | (l, t) <- edges]

-- Map a function over all values in the trie
-- Edge labels stay the same.
mapTrie :: (a -> b) -> Trie a e -> Trie b e
{- TO BE WRITTEN -}
mapTrie f (Trie v cs) = undefined

-- To build an infinite trie, we start from the root
-- The root starts with the empty list...
-- And from that, we have a number of edges
-- The domain 'dom' defines how many edges we have per node
rootTrie :: [a] -> Trie [a] a
{- TO BE WRITTEN -}
rootTrie domain = undefined

-- How do we create the edges?
-- We look at the domain,
-- and create an edge for each value, with:
-- 1. a label
-- 2. a subtree build with
-- the domain, the current label
-- and the current node
edges :: [a] -> [a] -> [(a, Trie [a] a)]
{- TO BE WRITTEN -}
edges domain currentNode = undefined

-- How do we build the subtree?
-- We use the label we just followed
-- and the parent node
-- And each child creates more edges!
-- (using the edges function)
subtree :: [a] -> a -> [a] -> Trie [a] a
{- TO BE WRITTEN -}
subtree domain label parent =
  undefined

-- Important: the trie is infinite because edges calls subtree, and subtree calls edges.

-- trieCache builds a cache for a function
-- provided with a domain (for the list elements)
trieCache :: [e] -> ([e] -> b) -> Trie b e
{- TO BE WRITTEN -}
trieCache domain function = undefined

{--
You can inspect the cache with GHCI!

> let cache = trieCache ['a' .. 'z'] reverse
> :print cache

Prints the cache, and we can see nothing is evaluated
(Warning: Make sure function doesn't return a generic type, otherwise it doesn't work)

> trieLookup cache []
> :print cache

Prints the cache, and also the unevaluated thunks.
Print is very useful when you want to debug calculations with infinite structures
Cause it doesn't evaluate the whole thing.

And then, you can do

> trieLookup cache "hej"
> :print cache

Prints the cache, and you should be able to see it has grown
--}

testTrie =
  let cache = mapTrie reverse $ rootTrie ['a'..'z']
  in trieLookup cache

-- We can test it on the LPS function
-- Computes the "longest palyndromic subsequence"
-- So, The length of the sequence of characters so that:
-- It's a palyndrome
-- It contains characters of the input (but you can skip some)
-- (Is slow)

-- First, some test strings
k1 = "writers"
k2 = "vintner"
l1 = "aferociousmonadatemyhamster"
l2 = "functionalprogrammingrules"
s1 = "bananrepubliksinvasionsarmestabsadjutant"
s2 = "kontrabasfiolfodralmakarmästarlärling"

openLPS :: (String -> String) -> (String -> String)
openLPS s = undefined -- look at 'lps' for inspiration

-- Fast!
fastLPS :: String -> String
fastLPS s =
  undefined

-- So, what were the tricks?
-- The first one is to build an infinite data-structure, to memoize the function
-- And then your function looks in the cache for the answer!
