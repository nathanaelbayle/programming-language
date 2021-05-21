# Simple Calculator

## Interpreter for calculator expressions
The interpreter for `CalcExprAST` consists of one `evaluate :: CalcExprAST −> Integer` function taking 
the calculator expression AST to the integer value it represents. 

## Unit test
In addition to the evaluator itself, a unit test has been added that checks whether the two
named expressions (from the `CalculatorAST` source file) evaluate as expected.
````haskell
calculatorAST1 = Lit 4
calculatorAST2 = Neg (Mult (Add (Lit 3) (Sub ( Lit 7) ( Lit 13))) ( Lit 19))
````
If something unexpected happens, just uncomment the two commented lines in the unit test
`unittestCalculatorInterpreter` to see the evaluation of these ASTs.

## Interactive calculator
There is also a `main` function that implements an interactive calculator. It will read an AST
(in Haskell `CalcExprAST` notation), parse it, evaluate it, and then stop. Note that the line
editor used for input `getLine` is horrible, and does not allow any editing of the input.

The `main :: IO()` function reads a string from the terminal using Haskell’s `getLine ::IO String` function. 
The string is extracted using the assignment `str <− getLine` (which is only allowed inside a do block). 
It then uses Haskell’s `read :: Read a => String −> a` function. This function is able to parse the 
string into a Haskell data value of the desired target type, if that type is readable. Since the `CalcExprAST`
is deriving Read (among others), such AST expressions can be parsed from `str`. If this parse fails, 
the program crashes.


You can try Haskell’s string parsing feature on some simple examples.
````haskell
(read "123") :: Integer
(read "123") :: Float
(read "123") :: Double
import Data.Complex
(read "123:+0") :: (Complex Int)
(read "123:+0") :: (Complex Float)
````
