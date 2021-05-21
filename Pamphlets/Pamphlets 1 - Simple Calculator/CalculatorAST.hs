-- | AST for simple integer calculator.

module CalculatorAST where

-----------------------

-- | Expressions for a simple calculator.
-- The calculator supports literals and operations
-- Addition, multiplication, and subtraction/negation.
data CalcExprAST
  = Lit Integer
  | Add CalcExprAST CalcExprAST
  | Mult CalcExprAST CalcExprAST
  | Sub CalcExprAST CalcExprAST
  | Neg CalcExprAST
  deriving (Eq, Read, Show)

-----------------------

-- | A couple ASTs for CalcExprAST.
calculatorAST1 = Lit 4
calculatorAST2
  = Neg (Mult (Add (Lit 3) (Sub (Lit 7) (Lit 13))) (Lit 19))

-----------------------
