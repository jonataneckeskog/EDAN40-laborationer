-- Jonatan Eckeskog, Anders Persson

module Chatterbot where

-- If you're not sure what this is, it's ok.
import Control.Monad (mapM)
import Data.Char
import Data.Maybe
import System.Random
import Utilities

-- A pattern is a list of things
-- Where we have either a value or a wildcard
data PatternElem a = Wildcard | Item a
  deriving (Eq, Show)

-- A pattern is a list of pattern elements
newtype Pattern a = Pattern [PatternElem a]
  deriving (Eq, Show)

-- Templates are the same as patterns
type Template a = Pattern a

-- A phrase is a list of string
type Phrase = [String]

newtype Rule = Rule (Pattern String, [Template String])
  deriving (Eq, Show)

type BotBrain = [Rule]

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
  putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
  botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

-- This takes a brain, and returns a function
-- Which will take a phrase as an input and calculate the result
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind b =
  fmap rulesApply (mapM makePair b)

-- A rule maps a pattern to many answers, so we choose one
-- at random, and that's our bot
makePair :: Rule -> IO (Pattern String, Template String)
makePair (Rule (pat, templates)) = do
  rand <- randomIO :: IO Float
  let randTemplate = pick rand templates
  return (pat, randTemplate)

rulesApply :: [(Pattern String, Template String)] -> Phrase -> Phrase
--rulesApply pairs input = fromMaybe [] (transformationsApply reflect pairs input)
rulesApply pairs = fromMaybe [] . transformationsApply reflect pairs
-- >>> reflect ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"]
-- ["you","will","never","see","your","reflection","in","my","eyes"]
reflect :: Phrase -> Phrase
reflect = map (try (flip lookup reflections))

reflections =
  [ ("am", "are"),
    ("was", "were"),
    ("i", "you"),
    ("i'm", "you are"),
    ("i'd", "you would"),
    ("i've", "you have"),
    ("i'll", "you will"),
    ("my", "your"),
    ("me", "you"),
    ("are", "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your", "my"),
    ("yours", "mine"),
    ("you", "me")
  ]

---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (== "quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = map ruleCompile

ruleCompile :: (String, [String]) -> Rule
ruleCompile (pat, ts) = Rule (compPattern, templates)
  where
    compPattern = starPattern (lowerCase pat)
    templates = map starPattern ts
    lowerCase = map toLower

--------------------------------------

-- We can make a pattern from a list of elements
-- If we choose one element that represents the wildcard
-- mkPattern '*' "Hi *!" => [Item 'H', Item 'i', Wildcard, Item '!']
mkPattern :: (Eq a) => a -> [a] -> Pattern a
mkPattern _ [] = Pattern []
mkPattern wc (x : xs) = Pattern (newItem : ps)
  where
    newItem = if x == wc then Wildcard else Item x
    Pattern ps = mkPattern wc xs

stringToPattern :: String -> String -> Pattern String
stringToPattern wc = mkPattern wc . words

starPattern :: String -> Pattern String
starPattern = stringToPattern "*"

reductions :: [(Pattern String, Pattern String)]
reductions =
  (map . map2)
    (starPattern, starPattern)
    [ ("please *", "*"),
      ("could you *", "*"),
      ("can you *", "*"),
      ("tell me if you are *", "are you *"),
      ("tell me who * is", "who is *"),
      ("tell me what * is", "what is *"),
      ("do you know who * is", "who is *"),
      ("do you know what * is", "what is *"),
      ("are you very *", "are you *"),
      ("i am very *", "i am *"),
      ("hi *", "hello *")
    ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [(Pattern String, Pattern String)] -> Phrase -> Phrase
reductionsApply pairs = fix (try (transformationsApply id pairs)) 

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a template with the list given as the third argument
-- >>> substitute (mkPattern 'x' "3*cos(x) + 4 - x") "5.37"
-- "3*cos(5.37) + 4 - 5.37"

substitute :: (Eq a) => Template a -> [a] -> [a]
substitute _ [] = []
substitute (Pattern []) _ = []
substitute (Pattern (Wildcard : ps)) x = x ++ substitute (Pattern ps) x
substitute (Pattern (Item a : ps)) x = a : substitute (Pattern ps) x

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
-- >>> match (mkPattern '*' "What is Haskell") "What is Haskell" 
-- Just ""
-- >>> match (Pattern []) []
-- Just []
-- >>> match (mkPattern 'x' "2*x+3") "2*7+3"
-- Just "7"
-- >>> match (mkPattern '*' "* and *") "you and me"
-- Just "you"

match :: (Eq a) => Pattern a -> [a] -> Maybe [a]
match (Pattern []) [] = Just []
match (Pattern []) x = Nothing
match ps [] = Nothing
match (Pattern (Item a : ps)) (x : xs) = if a == x 
  then match (Pattern ps) xs
  else Nothing 
match p x = orElse (singleWildcardMatch p x) (longerWildcardMatch p x)

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: (Eq a) => Pattern a -> [a] -> Maybe [a]
singleWildcardMatch (Pattern (Wildcard : ps)) (x : xs) =
  case match (Pattern ps) xs of
    Nothing -> Nothing
    Just _ -> Just [x]

longerWildcardMatch ps (x : xs) = 
  case match ps xs of
    Nothing -> Nothing
    Just result -> Just (x : result)

-------------------------------------------------------
-- Applying patterns transformations
--------------------------------------------------------

-- Helper function: Matches a pattern and applies the transformation
matchAndTransform :: (Eq a) => ([a] -> [a]) -> Pattern a -> [a] -> Maybe [a]
matchAndTransform transform pat = (mmap transform) . (match pat)

-- Applying a single pattern
transformationApply :: (Eq a) => ([a] -> [a]) -> [a] -> (Pattern a, Template a) -> Maybe [a]
transformationApply transform input (inputPattern, outputTemplate) = 
  case matchAndTransform transform inputPattern input of
    Nothing -> Nothing
    Just result -> Just (substitute outputTemplate result)

-- Applying a list of patterns until one succeeds
transformationsApply :: (Eq a) => ([a] -> [a]) -> [(Pattern a, Template a)] -> [a] -> Maybe [a]
transformationsApply _ [] _ = Nothing
transformationsApply transform ((inputPattern, outputTemplate) : ps) input = 
  orElse (transformationApply transform input (inputPattern, outputTemplate)) 
  (transformationsApply transform ps input)

