-- | AST for variable based integer calculator with open ended set of primitive functions.
--
-- Author Magne Haveraaen
-- Since 2020-03-26

module CalculatorExternalPrimitivesDocumentationAST where


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
-- a function name, a list of parameter types, a return type, and a documentation string.
-- A documentation string can have embedded new lines.
type Primitive = (FunctionName,[TypeName],TypeName,DocString)

-- | Specific type names to differentiate between distinct purposes: function names.
type FunctionName = String
-- | Specific type names to differentiate between distinct purposes: type names.
type TypeName = String
-- | Specific type names to differentiate between distinct purposes: documentation strings.
type DocString = String

-- | The semantics of a call of a primitive function is a mapping 
-- from the function name (String) and argument list [Integer] to the resulting Integer.
type PrimitiveSemantics = FunctionName -> [Integer] -> Integer

