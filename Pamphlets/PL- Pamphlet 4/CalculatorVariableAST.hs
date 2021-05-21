-- | AST for variable based integer calculator.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module CalculatorVariableAST where


-----------------------

-- | Expressions for a calculator with variables.
-- The calculator supports literals and operations
-- Addition, multiplication, and subtraction/negation.
data CalcExprAST
  = Lit Integer
  | Add CalcExprAST CalcExprAST
  | Mult CalcExprAST CalcExprAST
  | Sub CalcExprAST CalcExprAST
  | Neg CalcExprAST
  | Var String
  deriving (Eq, Read, Show)

-- | Statement for setting and changing a variable
data CalcStmtAST
  = SetVar String CalcExprAST
  | AssVar String CalcExprAST
  deriving (Eq, Read, Show)


-----------------------

-- | A few ASTs for variable based CalcExprAST.
calculatorVariableAST1
  = Lit 4
calculatorVariableAST2
  = Neg (Mult (Add (Lit 3) (Sub (Lit 7) (Lit 13))) (Lit 19))
calculatorVariableAST3
  = Add (Var "Reg1") (Var "Reg4")
calculatorVariableAST4
  = Var "Reg2"
  
-- | A few CalcStmtASTs for setting and assigning variables.
calculatorSetVariableAST1
  = SetVar "Reg4" calculatorVariableAST1
calculatorSetVariableAST2
  = SetVar "Reg1" calculatorVariableAST2
calculatorSetVariableAST3
  = AssVar "Reg2" calculatorVariableAST3
calculatorSetVariableAST4
  = AssVar "Reg1" calculatorVariableAST4

-----------------------
