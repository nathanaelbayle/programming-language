-- | Semantics for variable based integer calculator with an open ended set of primitive functions.
-- The variables are linked to values in State.
-- The open ended set of primitive functions is handled by external parameters.
--
-- Author Magne Haveraaen
-- Since 2020-03-26

module CalculatorExternalPrimitivesDocumentationInterpreter where

-- Use variable calculator with open ended primitives AST and documentation.
import CalculatorExternalPrimitivesDocumentationAST

-- Use variable calculator state.
import CalculatorState


-----------------------
-- | Evaluate a Calculator expression to an Integer using a state for the variables.
-- The semantics of the primitive functions are given by the primsem argument.
evaluate :: PrimitiveSemantics -> CalcExprAST -> State -> Integer
evaluate primsem (Lit i) state = i
evaluate primsem (Fun str args) state = primsem str (evaluatelist primsem args state)
evaluate primsem (Var vname) state = getvalue vname state
-- evaluate primsem instr state = error $ "Unknown instruction " ++ (show instr)

-- | Evaluate the argument list for a primitive function.
-- The first argument is the semantics of the primitive functions.
evaluatelist :: PrimitiveSemantics -> [CalcExprAST] -> State -> [Integer]
evaluatelist primsem (arg:args) state 
  = (evaluate primsem arg state):(evaluatelist primsem args state)
evaluatelist primsem [] state = []

-- | Declare/assign a variable in state from a calculator expression.
-- The first argument is the semantics of the primitive functions.
execute :: PrimitiveSemantics -> CalcStmtAST -> State -> State
execute primsem (SetVar vname exp) state
  = addvariable vname (evaluate primsem exp state) state
execute primsem (AssVar vname exp) state
  = changevalue vname (evaluate primsem exp state) state
-- execute primsem instr state = error $ "Unknown statement " ++ (show instr)


-----------------------
-- | Unit test for calculator with variables and open ended set of primitive functions.
-- Can only test the structural part of the calculator:
-- declaring, assigning and accessing variables using "fake" semantic function testprimsem.
unittestCalculatorExternalPrimitivesInterpreter = do
  print $ "-- unittestCalculatorExternalPrimitivesInterpreter"
  -- Create variables in order, but with the wrong values.
  let state1 = execute testprimsem (SetVar "x" (Lit 11)) newstate
  let state2 = execute testprimsem (SetVar "y" (Lit 10)) state1
  let state3 = execute testprimsem (SetVar "z" (Lit 23)) state2
  -- Correct the values for the variables out of order.
  let state4 = execute testprimsem (AssVar "y" (Lit 37)) state3
  let state5 = execute testprimsem (AssVar "z" (Lit 39)) state4
  let state6 = execute testprimsem (AssVar "x" (Lit 31)) state5
  -- Create the expected set of variable-values.
  let state1' = execute testprimsem (SetVar "x" (Lit 31)) newstate
  let state2' = execute testprimsem (SetVar "y" (Lit 37)) state1'
  let state3' = execute testprimsem (SetVar "z" (Lit 39)) state2'
  -- putStrLn $ "state3'=" ++ (show state3')
  putStrLn $
    if (state3' == state6)
    && ((evaluate testprimsem (Var "x") state6) == 31)
    && ((evaluate testprimsem (Var "y") state6) == 37)
    && ((evaluate testprimsem (Var "z") state6) == 39)
    && ((testprimsem "func" []) == 5)
    then "Unit tests hold"
    else "Tests failed"

-- | Fake integer semantics function: always returns 5.
testprimsem :: PrimitiveSemantics
testprimsem str plist = 5


-----------------------
-- | Interactive calculator with variables and open ended set of primitive functions.
-- • Stops if input string is empty.
-- • Shows current value of state when "show" is input.
-- • Parses and executes CalcStmtAST and prints value of updated variable otherwise.
-- The semantics of the primitive functions comes via the primsem function.
mainCalculatorVariable :: PrimitiveSemantics -> IO ()
mainCalculatorVariable primsem = do
  print $ "-- Interactive calulator with variables."
  mainCalculatorVariableAsk primsem newstate


-- | Interactive part of calculator with variables and open ended set of primitive functions.
-- Reads user input and handles the different cases:
-- • Stops if input string is empty.
-- • Shows current value of state when "show" is input.
-- • Parses and executes CalcStmtAST and prints value of updated variable otherwise.
mainCalculatorVariableAsk :: PrimitiveSemantics -> State -> IO ()
mainCalculatorVariableAsk primsem state = do
  putStr $ "¢ "
  str <- getLine
  if str /= ""
  then 
    if str == "show"
    then mainCalculatorVariableShowstate primsem state
    else mainCalculatorVariableExc primsem str state
  else putStrLn $ "Finished"

-- | Shows current value of state and continues with interactive variable calculator.
mainCalculatorVariableShowstate :: PrimitiveSemantics -> State -> IO ()
mainCalculatorVariableShowstate primsem state = do
  putStrLn $ "state = " ++ (show state)
  mainCalculatorVariableAsk primsem state

-- | Parses and executes CalcStmtAST and prints subexpression value.
-- Then continues with interactive register calculator.
mainCalculatorVariableExc :: PrimitiveSemantics -> String -> State -> IO ()
mainCalculatorVariableExc primsem str state = do
  -- puStrLn $ "str=" ++ (show str)
  let stmt = (read str)::CalcStmtAST
  let vname = getstatementupdatevariable stmt
  let state' = execute primsem stmt state
  -- puStrLn $ "execute(" ++ (show stmt) ++ ") state == " ++ (show $ state')
  putStrLn $ "Variable " ++ vname ++ " = " ++ (show $ getvalue vname state')
  mainCalculatorVariableAsk primsem state'

-- | Helper function to extract updated variable name from statement.
getstatementupdatevariable :: CalcStmtAST -> String
getstatementupdatevariable (SetVar vname expr) = vname
getstatementupdatevariable (AssVar vname expr) = vname
-- getstatementupdatevariable stmt = error $ "Unknown statement " ++ (show stmt)

