-- | AST for variable based integer calculator with open ended set of primitive functions.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module CalculatorExternalPrimitivesAST where


-----------------------
-- | Expressions for a calculator with variables.
-- The calculator supports integer literals Lit,
-- an open ended set of primitive functions Fun, and
-- an open ended set of variables Var.
data CalcExprAST
  = Lit Integer
  | Fun String [CalcExprAST]
  | Var String
  deriving (Eq, Read, Show)


-- | Statement for declaring (setting) and changing (assigning) a variable
data CalcStmtAST
  = SetVar String CalcExprAST
  | AssVar String CalcExprAST
  deriving (Eq, Read, Show)



-----------------------
-- | A primitive function declaration defines 
-- a function name (String), a list of parameter types (Strings), and a return type (String).
type Primitive = (String,[String],String)

-- | The semantics of a call of a primitive function is a mapping 
-- from the function name (String) and argument list of integers to the resulting integer.
type PrimitiveSemantics = String -> [Integer] -> Integer



-----------------------
-- | A few ASTs for variable based CalcExprAST.
-- Note that primitive function names in AST2 and AST3 are
-- chosen for historical reasons (the same as in pamphlet 1).
-- These examples show the notation, but are not practically useful.
calculatorExprVPAST1
  = Lit 4
calculatorExprVPAST2
  = Fun "Neg" [Fun "Mult" [Fun "Add" [(Lit 3),(Fun "Sub" [(Lit 7),(Lit 13)])],(Lit 19)]]
calculatorExprVPAST3
  = Fun "Add" [(Var "Reg1"), (Var "Reg4")]
calculatorExprVPAST4
  = Var "Reg2"
  
-- | A few CalcStmtASTs for setting and assigning variables.
-- These examples are not practically useful.
-- They only show that the AST notation is understood.
calculatorStmtVEPAST1
  = SetVar "Reg4" calculatorExprVPAST1
calculatorStmtVEPAST2
  = SetVar "Reg1" calculatorExprVPAST2
calculatorStmtVEPAST3
  = AssVar "Reg2" calculatorExprVPAST3
calculatorStmtVEPAST4
  = AssVar "Reg1" calculatorExprVPAST4


-----------------------
