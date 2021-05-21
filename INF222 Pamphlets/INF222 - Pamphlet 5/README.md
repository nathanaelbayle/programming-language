# Calculator with more operations

## Interpreter for extended calculator with variables
In this solution the Haskell `quot` and `rem` operations have been used for integer division
and remainder. The semantic functions also tests against division/remainder by 0.
An alternative would have been the Haskell `div` and `mod` operations.


## Unit test
The unit test `unittestCalculatorVariableIdivremInterpreter` has been modified by adding
the parameterised unit test `checkIdivRem` to check the relationship between `Idiv` and `Rem`,
which is checked 81002 times for a selection of positive and negative integers. The test expresses the expectation 
that `mult( idiv (x,y) ,y) + rem(x,y)` should give back the value `x` (assuming`y/=0` ). 
This relationship holds in Haskell for the pair `quot` & `rem `and the pair
`div` & `mod` . Mixing `quot` & `mod` or `div` & `rem` will work for positive numbers, but
may fail if one or both arguments are negative numbers.

The `main` function has not been changed, but another test example has been added to
the documentation comment:
````haskell
SetVar "v9" (Add (Mult ( Idiv (Var "Reg1") (Var "Reg4")) (Var "Reg4")) (Rem (Var "Reg1") (Var "Reg4")))
````
