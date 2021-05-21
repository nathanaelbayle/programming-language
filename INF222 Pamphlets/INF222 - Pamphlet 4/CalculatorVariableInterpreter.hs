-- | Semantics for variable based integer calculator.
-- The values linked to the variables are kept in a State.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module CalculatorVariableInterpreter where

-- Use variable calculator AST.
import CalculatorVariableAST

-- Use variable calculator state.
import CalculatorState


-----------------------


-- | Evaluate a Calculator expression to an Integer using a state for the variables.
evaluate :: CalcExprAST -> State -> Integer
evaluate (Lit i) state = i
evaluate (Add i j) state = evaluate i state + evaluate j state
evaluate (Mult i j) state = evaluate i state * evaluate j state
evaluate (Sub i j) state = evaluate i state - evaluate j state
evaluate (Neg i) state = - evaluate i state
evaluate (Var vname) state = getvalue vname state
-- evaluate instr state = error $ "Unknown instruction " ++ (show instr)


-- | Declare/assign a variable in state from a calculator expression
execute :: CalcStmtAST -> State -> State
execute (SetVar vname exp) state
  = addvariable vname (evaluate exp state) state
execute (AssVar vname exp) state
  = changevalue vname (evaluate exp state) state
-- execute instr state = error $ "Unknown statement " ++ (show instr)


-----------------------

-- | Unit test for interpreter functions.
unittestCalculatorVariableInterpreter = do
  print $ "-- unittestCalculatorVariableInterpreter"
  -- print $ "newstate=" ++ (show newstate)
  let state = addvariable "Reg4" 43 
             (addvariable "Reg2" 21 
             (addvariable "Reg1" 11 newstate))
  -- print $ "state=" ++ (show state)
  -- print $ "evaluate (" ++ (show calculatorVariableAST1) ++ ") state " ++ " == "
  --           ++ (show $ evaluate calculatorVariableAST1 state)
  -- print $ "evaluate (" ++ (show calculatorVariableAST2) ++ ") state " ++ " == "
  --           ++ (show $ evaluate calculatorVariableAST2 state)
  -- print $ "evaluate (" ++ (show calculatorVariableAST3) ++ ") state " ++ " == "
  --           ++ (show $ evaluate calculatorVariableAST3 state)
  -- print $ "evaluate (" ++ (show calculatorVariableAST4) ++ ") state " ++ " == "
  --           ++ (show $ evaluate calculatorVariableAST4 state)
  let state1 = execute (SetVar "Reg1" (Lit 11)) newstate
  let state2 = execute (SetVar "Reg2" (Add (Var "Reg1") (Lit 10))) state1
  let state3 = execute (SetVar "Reg4" (Sub (Mult (Var "Reg2") (Lit 2)) (Lit (-1)))) state2
  -- print $ "state3=" ++ (show state3)
  -- print $ "execute calculatorSetVariableAST1 newstate == " ++ 
  --  (show $ execute calculatorSetVariableAST1 newstate)
  -- print $ "execute calculatorSetVariableAST2 newstate == " ++
  --  (show $ execute calculatorSetVariableAST2 newstate)
  -- print $ "execute calculatorSetVariableAST3 state == " ++
  --  (show $ execute calculatorSetVariableAST3 state)
  -- print $ "execute calculatorSetVariableAST4 state == " ++
  --  (show $ execute calculatorSetVariableAST4 state)
  print $
    if  (4 == (evaluate calculatorVariableAST1 newstate))
    && (57 == (evaluate calculatorVariableAST2 newstate))
    && (54 == (evaluate calculatorVariableAST3 state))
    && (21 == (evaluate calculatorVariableAST4 state))
    && (state == state3)
    && (4 == evaluate (Var "Reg4") (execute calculatorSetVariableAST1 newstate))
    && (57 == evaluate (Var "Reg1") (execute calculatorSetVariableAST2 newstate))
    && (54 == evaluate (Var "Reg2") (execute calculatorSetVariableAST3 state3))
    && (21 == evaluate (Var "Reg1") (execute calculatorSetVariableAST4 state3))
    then "Unit tests hold"
    else "Tests failed"


-----------------------

-- | Interactive calculator with variables.
{- | Run the following commands in sequence at the prompt
SetVar "Reg4" (Lit 4)
SetVar "Reg1" (Neg (Mult (Add (Lit 3) (Sub (Lit 7) (Lit 13))) (Lit 19)))
SetVar "Reg2" (Add (Var "Reg1") (Var "Reg4"))
AssVar "Reg1" (Var "Reg2")
AssVar "Reg1" (Add (Var "Reg1") (Var "Reg4"))
show
AssVar "Reg1" (Add (Var "Reg1") (Var "Reg4"))
show

-}
main = do
  print $ "-- Interactive calulator with variables."
  mainCalculatorVariableAsk newstate


-- | Interactive part of a calculator with variables.
-- • Stops if input string is empty.
-- • Shows current value of state when "show" is input.
-- • Parses and executes CalcStmtAST and prints value of updated variable otherwise.
mainCalculatorVariableAsk state = do
  putStr $ "¢ "
  str <- getLine
  if str /= ""
  then 
    if str == "show"
    then mainCalculatorVariableShowstate state
    else mainCalculatorVariableExc str state
  else putStrLn $ "Finished"

-- | Shows current value of state and continues with interactive variable calculator.
mainCalculatorVariableShowstate state = do
  putStrLn $ "state = " ++ (show state)
  mainCalculatorVariableAsk state

-- | Parses and executes CalcStmtAST and prints subexpression value.
-- Then continues with interactive calculator with variables.
mainCalculatorVariableExc str state = do
  -- puStrLn $ "str=" ++ (show str)
  let stmt = (read str)::CalcStmtAST
  let vname = getstatementupdatevariable stmt
  let state' = execute stmt state
  -- puStrLn $ "execute(" ++ (show stmt) ++ ") state == " ++ (show $ state')
  putStrLn $ "Variable " ++ vname ++ " = " ++ (show $ getvalue vname state')
  mainCalculatorVariableAsk $ state'

-- | Helper function to extract updated variable from statement.
getstatementupdatevariable :: CalcStmtAST -> String
getstatementupdatevariable (SetVar vname expr) = vname
getstatementupdatevariable (AssVar vname expr) = vname
-- getstatementupdatevariable stmt = error $ "Unknown statement " ++ (show stmt)

