-- | Semantics for register based integer calculator.
-- The values of the registers are stored in a Store.
--
-- Author Magne Haveraaen
-- Since 2020-03-14

module CalculatorRegisterVariableInterpreter where

-- Use register calculator AST.
import CalculatorRegisterVariableAST

-- Use register calculator store.
import CalculatorRegisterStore


-----------------------


-- | Evaluate a Calculator expression to an Integer using a store for the registers.
evaluate :: CalcExprAST -> Store -> Integer
evaluate (Lit i) store = i
evaluate (Add i j) store = evaluate i store + evaluate j store
evaluate (Mult i j) store = evaluate i store * evaluate j store
evaluate (Sub i j) store = evaluate i store - evaluate j store
evaluate (Neg i) store = - evaluate i store
evaluate (Reg reg) store = getstore store (getregisterindex reg)
-- evaluate instr store = error $ "Unknown instruction " ++ (show instr)


-- | Set a register in store from a Calculator expression
execute :: CalcStmtAST -> Store -> Store
execute (SetReg reg exp) store
  = setstore (getregisterindex reg) (evaluate exp store) store
-- execute instr store = error $ "Unknown statement " ++ (show instr)


-- | Get the numerical index corresponding to a register.
getregisterindex :: String -> Integer
getregisterindex "Reg0" = 0
getregisterindex "Reg1" = 1
getregisterindex "Reg2" = 2
getregisterindex "Reg3" = 3
getregisterindex "Reg4" = 4
getregisterindex "Reg5" = 5
getregisterindex "Reg6" = 6
getregisterindex "Reg7" = 7
getregisterindex "Reg8" = 8
getregisterindex "Reg9" = 9
getregisterindex reg = error $ "Unknown register " ++ (show reg)


-----------------------

-- | Unit test for interpreter functions and register store.
unittestCalculatorRegisterVariableInterpreter = do
  print $ "-- unittestCalculatorRegisterVariableInterpreter"
  -- print $ "registerstore=" ++ (show registerstore)
  let store = setstore (getregisterindex "Reg4") 43 
             (setstore (getregisterindex "Reg2") 21 
             (setstore (getregisterindex "Reg1") 11 registerstore))
  -- print $ "store=" ++ (show store)
  -- print $ "evaluate (" ++ (show calculatorRegisterAST1) ++ ") store " ++ " == "
  --           ++ (show $ evaluate calculatorRegisterAST1 store)
  -- print $ "evaluate (" ++ (show calculatorRegisterAST2) ++ ") store " ++ " == "
  --           ++ (show $ evaluate calculatorRegisterAST2 store)
  -- print $ "evaluate (" ++ (show calculatorRegisterAST3) ++ ") store " ++ " == "
  --           ++ (show $ evaluate calculatorRegisterAST3 store)
  -- print $ "evaluate (" ++ (show calculatorRegisterAST4) ++ ") store " ++ " == "
  --           ++ (show $ evaluate calculatorRegisterAST4 store)
  let store1 = execute (SetReg "Reg1" (Lit 11)) registerstore
  let store2 = execute (SetReg "Reg2" (Add (Reg "Reg1") (Lit 10))) store1
  let store3 = execute (SetReg "Reg4" (Sub (Mult (Reg "Reg2") (Lit 2)) (Lit (-1)))) store2
  -- print $ "store3=" ++ (show store3)
  -- print $ "execute calculatorSetRegisterAST1 registerstore == " ++ 
  --  (show $ execute calculatorSetRegisterAST1 registerstore)
  -- print $ "execute calculatorSetRegisterAST2 registerstore == " ++
  --  (show $ execute calculatorSetRegisterAST2 registerstore)
  -- print $ "execute calculatorSetRegisterAST3 registerstore == " ++
  --  (show $ execute calculatorSetRegisterAST3 registerstore)
  -- print $ "execute calculatorSetRegisterAST4 registerstore == " ++
  --  (show $ execute calculatorSetRegisterAST4 registerstore)
  print $
    if  (4 == (evaluate calculatorRegisterAST1 store))
    && (57 == (evaluate calculatorRegisterAST2 store))
    && (54 == (evaluate calculatorRegisterAST3 store))
    && (21 == (evaluate calculatorRegisterAST4 store))
    && (store == store3)
    && (4 == evaluate (Reg "Reg4") (execute calculatorSetRegisterAST1 registerstore))
    && (57 == evaluate (Reg "Reg1") (execute calculatorSetRegisterAST2 registerstore))
    && (0 == evaluate (Reg "Reg2") (execute calculatorSetRegisterAST3 registerstore))
    && (0 == evaluate (Reg "Reg1") (execute calculatorSetRegisterAST4 registerstore))
    then "Unit tests hold"
    else "Tests failed"


-----------------------

-- | Interactive calculator with registers.
{- | Run the following commands in sequence at the prompt
SetReg "Reg4" (Lit 4)
SetReg "Reg1" (Neg (Mult (Add (Lit 3) (Sub (Lit 7) (Lit 13))) (Lit 19)))
SetReg "Reg2" (Add (Reg "Reg1") (Reg "Reg4"))
SetReg "Reg1" (Reg "Reg2")
SetReg "Reg1" (Add (Reg "Reg1") (Reg "Reg4"))
show
SetReg "Reg1" (Add (Reg "Reg1") (Reg "Reg4"))
show

-}
main = do
  print $ "-- Interactive register calulator."
  mainCalculatorRegisterAsk registerstore


-- | Interactive part of register calculator.
-- • Stops if input string is empty.
-- • Shows current value of state when "show" is input.
-- • Parses and executes CalcStmtAST and prints subexpression value otherwise.
mainCalculatorRegisterAsk state = do
  putStr $ "¢ "
  str <- getLine
  if str /= ""
  then 
    if str == "show"
    then mainCalculatorRegisterShowstate state
    else mainCalculatorRegisterExc str state
  else putStrLn $ "Finished"

-- | Shows current value of state and continues with interactive register calculator.
mainCalculatorRegisterShowstate state = do
  putStrLn $ "state = " ++ (show state)
  mainCalculatorRegisterAsk state

-- | Parses and executes CalcStmtAST and prints subexpression value.
-- Then continues with interactive register calculator.
mainCalculatorRegisterExc str state = do
  -- print $ "str=" ++ (show str)
  let stmt = (read str)::CalcStmtAST
  -- print $ "execute(" ++ (show stmt) ++ ") state == " ++ (show $ execute stmt state)
  let SetReg reg expr = stmt
  print $ "evaluate (" ++ (show expr) ++ ") state == " ++ (show $ evaluate expr state)
  mainCalculatorRegisterAsk $ execute stmt state

  
