-- | AST for simple integer calculator.

module CalculatorInterpreter where

-- | Use the Calculator AST
import CalculatorAST

-- | Evaluate a Calculator expression to an Integer
evaluate :: CalcExprAST -> Integer
evaluate ( Lit i ) = i
evaluate (Add i j ) = evaluate i + evaluate j
evaluate (Mult i j ) = evaluate i * evaluate j
evaluate (Sub i j ) = evaluate i - evaluate j
evaluate (Neg i ) = - evaluate i

-- | Unit test: show some calculator ASTs and their evaluation (commented out) and
-- validate those against expected values.
unittestCalculatorInterpreter =
  do
    print $ "−− unittestCalculatorInterpreter "
    print $ "evaluate " ++ (show calculatorAST1) ++ " == "
      ++ (show $ evaluate calculatorAST1)
    print $ "evaluate " ++ (show calculatorAST2) ++ " == "
      ++ (show $ evaluate calculatorAST2)
    print $ if (4 == (evaluate calculatorAST1 )) && (57 == (evaluate calculatorAST2 ))
      then "Unit tests hold"
      else "Tests failed "

-- | Interactive calculator.
-- At the prompt provide a (Haskell) CalcExprAST and see how it is evaluated.

main =
  do
    print $ "−− Interactive calulator "
    str <- getLine
    let expr = (read str ) :: CalcExprAST
    print $ " evaluate (" ++ (show expr) ++ ")=" ++ (show $ evaluate expr)
