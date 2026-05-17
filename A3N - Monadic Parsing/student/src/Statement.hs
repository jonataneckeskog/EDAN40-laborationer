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
    >-> \((cond, thenStmt), elseStmt) -> If cond thenStmt elseStmt

-- 'skip' ';'
skip = accept "skip" #- require ";" >-> \_ -> Skip

-- 'begin' statements 'end'
begin = accept "begin" -# iter parse #- require "end" >-> \xs -> Begin xs

-- 'while' expr 'do' statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> \(cond, stmt) -> While cond stmt

-- 'read' variable ';'
readStatement = accept "read" -# word #- require ";" >-> \var -> Read var

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
  parse = assignment ! ifStatement ! skip ! begin ! while ! readStatement ! write
  toString statement =
    case statement of
      Assignment s expr -> s ++ " := " ++ toString expr ++ ";"
      If expr thenStmt elseStmt -> "if " ++ toString expr ++ " then " ++ toString thenStmt ++ " else " ++ toString elseStmt
      Skip -> "skip;"
      Begin stmts -> "begin " ++ concatMap toString stmts ++ " end"
      While expr stmt -> "while " ++ toString expr ++ " do " ++ toString stmt
      Read s -> "read " ++ s ++ ";"
      Write expr -> "write " ++ toString expr ++ ";"
