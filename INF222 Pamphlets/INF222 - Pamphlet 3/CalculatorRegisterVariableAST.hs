-- | AST for register based integer calculator.
--
-- Author Magne Haveraaen
-- Since 2020-03-14

module CalculatorRegisterVariableAST where


-----------------------

-- | Expressions for a calculator with 10 registers.
-- The calculator supports literals and operations
-- Addition, multiplication, and subtraction/negation.
data CalcExprAST
  = Lit Integer
  | Add CalcExprAST CalcExprAST
  | Mult CalcExprAST CalcExprAST
  | Sub CalcExprAST CalcExprAST
  | Neg CalcExprAST
  | Reg String
  deriving (Eq, Read, Show)

-- | Statement for setting a register
data CalcStmtAST
  = SetReg String CalcExprAST
  deriving (Eq, Read, Show)


-----------------------

-- | A few ASTs for register based CalcExprAST.
calculatorRegisterAST1
  = Lit 4
calculatorRegisterAST2
  = Neg (Mult (Add (Lit 3) (Sub (Lit 7) (Lit 13))) (Lit 19))
calculatorRegisterAST3
  = Add (Reg "Reg1") (Reg "Reg4")
calculatorRegisterAST4
  = Reg "Reg2"
  
-- | A few ASTs for setting registers CalcStmtAST.
calculatorSetRegisterAST1
  = SetReg "Reg4" calculatorRegisterAST1
calculatorSetRegisterAST2
  = SetReg "Reg1" calculatorRegisterAST2
calculatorSetRegisterAST3
  = SetReg "Reg2" calculatorRegisterAST3
calculatorSetRegisterAST4
  = SetReg "Reg1" calculatorRegisterAST4

-----------------------
