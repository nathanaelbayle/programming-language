-- | Semantics for variable based integer calculator with an open ended set of primitive functions.
-- The variables are linked to values in State.
-- The open ended set of primitive functions is handled by external parameters.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module CalculatorValueDomainInterpreter where

-- Use variable calculator with open ended primitives AST.
import CalculatorValueDomainAST

-- Use variable calculator state.
import CalculatorValueDomainState


-----------------------
-- | Evaluate a Calculator expression to a value domain using a state for the variables.
-- The semantics of the primitive functions are given by the primsem argument.
evaluate :: Show valuedomain => PrimitiveSemantics valuedomain -> CalcExprAST valuedomain -> State valuedomain -> valuedomain
evaluate primsem (Lit i) state = i
evaluate primsem (Fun str args) state = primsem str (evaluatelist primsem args state)
evaluate primsem (Var vname) state = getvalue vname state
-- evaluate primsem instr state = error $ "Unknown instruction " ++ (show instr)

-- | Evaluate the argument list for a primitive function.
-- The first argument is the semantics of the primitive functions.
evaluatelist :: Show valuedomain => PrimitiveSemantics valuedomain -> [CalcExprAST valuedomain] -> State valuedomain -> [valuedomain]
evaluatelist primsem (arg:args) state 
  = (evaluate primsem arg state):(evaluatelist primsem args state)
evaluatelist primsem [] state = []

-- | Declare/assign a variable in state from a calculator expression.
-- The first argument is the semantics of the primitive functions.
execute :: Show valuedomain => PrimitiveSemantics valuedomain -> CalcStmtAST valuedomain -> State valuedomain -> State valuedomain
execute primsem (SetVar vname exp) state
  = addvariable vname (evaluate primsem exp state) state
execute primsem (AssVar vname exp) state
  = changevalue vname (evaluate primsem exp state) state
-- execute primsem instr state = error $ "Unknown statement " ++ (show instr)


-----------------------
-- | Unit test for calculator with variables and open ended set of primitive functions.
-- Can only test the structural part of the calculator:
-- declaring, assigning and accessing variables.
-- Uses integers as our test value domain.
unittestCalculatorValueDomainInterpreter = do
  print $ "-- unittestCalculatorValueDomainInterpreter"
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
    && ((evaluate testprimsem (Var "z") state6) == 39)
    && ((evaluate testprimsem (Var "x") state6) == 31)
    && ((evaluate testprimsem (Var "y") state6) == 37)
    && ((testprimsem "func" []) == 5)
    then "Unit tests hold"
    else "Tests failed"

-- | Fake integer semantics function: always returns 5.
testprimsem :: PrimitiveSemantics Integer
testprimsem str plist = 5

