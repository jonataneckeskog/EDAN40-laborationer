module Program (T, parse, fromString, toString, exec) where

import qualified Dictionary
import Parser hiding (T)
import qualified Statement
import Prelude hiding (fail, return)

newtype T = Program [Statement.T] deriving (Eq)

instance Show T where
  show = toString

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program stmts) = concatMap Statement.toString stmts

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.execute stmts Dictionary.empty
