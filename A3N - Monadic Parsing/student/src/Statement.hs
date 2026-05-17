module Statement (T, parse, toString, fromString, execute) where

import qualified Dictionary
import qualified Expr
import Parser hiding (T)
import Prelude hiding (fail, return)

type T = Statement

data Statement
  = Assignment String Expr.T
  | If Expr.T Statement Statement
  | Skip
  | Begin [Statement]
  | While Expr.T Statement
  | Read String
  | Write Expr.T
  deriving (Show)

-- x =: 5
-- "x", förvänta ":=", parsa med 5 -> "x", 5 -> förvänta ";", omvandla till Assignment ("x", 5)
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment

-- 'if' expr 'then' statement 'else' statement
ifStatement =
  accept "if"
    -# Expr.parse
    #- require "then"
    # parse
    #- require "else"
    # parse
    >-> \((cond, thenStmts), elseStmts) -> If cond thenStmts elseStmts

-- 'skip' ';'
skip = accept "skip" #- require ";" >-> \_ -> Skip

-- 'begin' statements 'end'
begin = accept "begin" -# iter parse #- require "end" >-> \xs -> Begin xs

-- 'while' expr 'do' statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> \(cond, stmt) -> While cond stmt

-- 'read' variable ';'
read = accept "read" -# word #- require ";" >-> \var -> Read var

-- 'write' expr ';'
write = accept "write" -# Expr.parse #- require ";" >-> \expr -> Write expr

class Executable t where
  execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

instance Executable Statement where
  -- execute :: [Statement] -> Dictionary.T String Integer -> [Integer] -> [Integer]
  execute (If cond thenStmts elseStmts : stmts) dict input =
    case (Expr.value cond dict) of
      Left err -> error err
      Right v ->
        if v > 0
          then
            execute (thenStmts : stmts) dict input
          else
            execute (elseStmts : stmts) dict input

instance Parse Statement where
  parse = error "Statement.parse not implemented"
  toString = error "Statement.toString not implemented"
