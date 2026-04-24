-- This is a solution for an older version of the assignment
-- Where we use the new memoization library
module StringAlignment where

import Memoization (trieLookup, rootTrie, mapTrie)

import Data.Maybe (fromJust)


-- OPTIONAL (more complicated):
-- String alignment with memoization
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

score :: Char -> Char -> Int
score '-' y = scoreSpace
score x '-' = scoreSpace
score x y
  | x == y = scoreMatch
  | otherwise = scoreMismatch

-- Takes two strings and computes their similarity
similarityScore :: String -> String -> Int
similarityScore s1 s2 = sum $ uncurry (zipWith score) $ head $ optAlignments s1 s2

-- Attaches h1 and h2 to the lists in tuples in aList
-- λ> attachHeads "h1" "h2" [(["x1", "x2"], ["y1", "y2"])]
-- [(["h1","x1","x2"],["h2","y1","y2"])]
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs, ys) <- aList]

-- Takes a value function and a list of a's
-- And returns the ones with max score
-- (Important, they all have the same score!)
maximaBy :: (Show a, Ord b) => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = let withScores = zip xs $ Prelude.map valueFcn xs
                           maxScore = maximum $ Prelude.map snd withScores
                       in Prelude.map fst $ Prelude.filter ((== maxScore) . snd) withScores

type Alignment = (String, String)

getScore :: Alignment -> Int
getScore = sum . (uncurry (zipWith score))

-- Slow version
optAlignments :: String -> String -> [Alignment]
optAlignments [] [] = [("", "")]
optAlignments [] ys = [(replicate (length ys) '-', ys)]
optAlignments xs [] = [(xs, replicate (length xs) '-')]
optAlignments (x:xs) (y:ys) =
  let nospace = attachHeads x y $ optAlignments xs ys
      space1 = attachHeads x '-' $ optAlignments xs (y:ys)
      space2 = attachHeads '-' y $ optAlignments (x:xs) ys
  in maximaBy (sum . (uncurry (zipWith score))) $ concat [nospace, space1, space2]

-- Fast version ?
openOptAlignments :: (String -> String -> [Alignment]) -> String -> String -> [Alignment]
openOptAlignments _ [] [] = [("", "")]
openOptAlignments _ [] ys = [(replicate (length ys) '-', ys)]
openOptAlignments _ xs [] = [(xs, replicate (length xs) '-')]
openOptAlignments f (x:xs) (y:ys) =
  let nospace = attachHeads x y $ f xs ys
      space1 = attachHeads x '-' $ f xs (y:ys)
      space2 = attachHeads '-' y $ f (x:xs) ys
  in maximaBy getScore $ concat [nospace, space1, space2]


-- This function takes TWO arguments, so we need to write something that makes the right trie structure!
memoF2Trie :: (String -> String -> a) -> (String -> String -> a)
memoF2Trie f =
  let mkCache = flip mapTrie $ rootTrie [minBound .. maxBound]
      c = mkCache (mkCache . f)  -- Makes a cache of caches...
  in trieLookup . (trieLookup c)

fastOptAlignments = memoF2Trie (openOptAlignments fastOptAlignments)

-- This is not as fast as we'd wish
-- If we run profiling on this, we can see that maximaBy is expensive!
-- I'm thinking we can speed up things, if we used another
-- return type than AlignmentType

-- We can make the function return a list of alignments with all the same score
-- This probably makes sense, cause maximaBy tries to rule out alignments with low scores
type AlignmentWithScore = (Int, [Alignment])

-- Some functions to work with alignments
mkAlignment :: Alignment -> AlignmentWithScore
mkAlignment x = (getScore x, [x])

emptyAlign :: AlignmentWithScore
emptyAlign = (minBound, [])

-- We can merge two scores together
mergeAlignments :: AlignmentWithScore -> AlignmentWithScore -> AlignmentWithScore
mergeAlignments (score1, align1) (score2, align2)
  | score1 == score2 = (score1, align1 ++ align2)
  | score1 > score2 = (score1, align1)
  | otherwise = (score2, align2)

attachHeads2 :: Char -> Char -> AlignmentWithScore -> AlignmentWithScore
attachHeads2 c1 c2 (oldScore, aligns) = (score c1 c2 + oldScore, attachHeads c1 c2 aligns)

-- Here, we replaced maximaBy by a fold, and that makes things way faster
openOptAlignments2 :: (String -> String -> AlignmentWithScore) -> String -> String -> AlignmentWithScore
openOptAlignments2 _ [] [] = (0, [("", "")])
openOptAlignments2 _ [] ys = mkAlignment (replicate (length ys) '-', ys)
openOptAlignments2 _ xs [] = mkAlignment (xs, replicate (length xs) '-')
openOptAlignments2 f (x:xs) (y:ys) =
  let nospace = attachHeads2 x y  $ f xs ys
      space1 = attachHeads2 x '-' $ f xs (y:ys)
      space2 = attachHeads2 '-' y $ f (x:xs) ys
  in foldr mergeAlignments emptyAlign $ [nospace, space1, space2]

optAlignments2 = openOptAlignments2 optAlignments2

fastOptAlignments2 = memoF2Trie (openOptAlignments2 fastOptAlignments2)

-- The memoization for stringAlignments was trickier to find out, you need:
  -- To see that the costly computation is maximaBy
  -- That maximaBy calculates the scores and doesn't save them
  -- (But memoizing that function to calculate the scores doesn't work... eats all your memory)
  -- But that you can store that in the return type of the function...
  -- And replace maximaBy by a fold and that's usually a good approach!
