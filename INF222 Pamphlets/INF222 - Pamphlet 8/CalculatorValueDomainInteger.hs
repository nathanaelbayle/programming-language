-- | A selection of integer function declarations and their semantics.
--
-- Author Magne Haveraaen
-- Since 2020-03-26, revised 2021-03-03

module CalculatorValueDomainInteger where

-- Use the AST format for calculators with an open set of documented operations.
import CalculatorValueDomainAST

-- Use the interpreter for calculators with an open set of operations.
import CalculatorValueDomainInterpreter
import CalculatorValueDomainMain

-- Use calculator state for variables and store.
import CalculatorValueDomainState


-----------------------
-- | Declaration of operations and their argument list & return type.
integeroperations :: [Primitive]
integeroperations
  = [ 
      ("Add", ["Integer","Integer"],"Integer","Add two integers"),
      ("Mult", ["Integer","Integer"],"Integer","Multiply two integers"),
      ("Sub", ["Integer","Integer"],"Integer","Subtract two integers"),
      ("Neg", ["Integer"],"Integer","Negate (change sign of) an integer"),
      ("Abs", ["Integer"],"Integer","The absolute value (positive) a integer"),
      ("Sqr", ["Integer"],"Integer","Squaring an integer"),
      ("Idiv", ["Integer","Integer"],"Integer","Integer division rounded towards zero"),
      ("Rem", ["Integer","Integer"],"Integer","Remainder when doing integer division"),
      ("Gcd", ["Integer","Integer"],"Integer",
       "Greatest common divisior of two integers,\ne.g., Gcd [10,15]==5"),
      ("Lcm", ["Integer","Integer"],"Integer",
       "Least common multiplier of two integers,\ne.g., Lcm [10,15]==30"),
      ("Succ", ["Integer"],"Integer","Successor, adding 1 to a number"),
      ("Pred", ["Integer"],"Integer","Predessor, subtracting 1 from a number")
    ]

-----------------------
-- | Semantics of chosen integer operations.
integersemantics :: PrimitiveSemantics Integer
integersemantics "Add" [i1,i2] = i1 + i2
integersemantics "Mult" [i1,i2] = i1 * i2
integersemantics "Sub" [i1,i2] = i1 - i2
integersemantics "Neg" [i] = - i
integersemantics "Abs" [i] = abs i
integersemantics "Sqr" [i] = i * i
integersemantics "Idiv" [i1,0]
  = error $ "Cannot do integer division of " ++ (show i1) ++ " by 0."
integersemantics "Idiv" [i1,i2] = quot i1 i2
integersemantics "Rem" [i1,0] 
  = error $ "Cannot do remainder of " ++ (show i1) ++ " by 0."
integersemantics "Rem" [i1,i2] = rem i1 i2
integersemantics "Gcd" [i1,i2] = gcd i1 i2
integersemantics "Lcm" [i1,i2] = lcm i1 i2
integersemantics "Succ" [i] = i + 1
integersemantics "Pred" [i] = i - 1
integersemantics fname alist 
  = error $ "Unknown function name/arg list " ++ fname ++ (show alist)


-----------------------
-- | Properties of some the integer functions.
-- The arguments are: property name (String), argument list (integer numbers).
integerproperties :: String -> [Integer] -> Bool
-- The absolute value of a positive number is always positive.
integerproperties "AbsPositive" [n] = 
  if 0 <= integersemantics "Abs" [n]
    then True 
    else error $ "Property AbsPositive failes for n=" ++ (show n)
-- Integer division and remainder relate as follows: n2 == (n1 idiv n2) * n2 + (n1 rem n2) when n2 /= 0.
integerproperties "IdivRem" [n1,n2] = 
  if n2 == 0 
  || n1 == (integersemantics "Add" [integersemantics "Mult" [integersemantics "Idiv" [n1,n2],n2], integersemantics "Rem" [n1,n2]])
    then True 
    else error $ "Property IdivRem failes for n1=" ++ (show n1) ++ " n2=" ++ (show n2)



-----------------------
-- | Unit test of the integeroperations:
-- for each declaration in integeroperations checks that there is a corresponding integersemantics.
unittestCalculatorIntegerPrimitives = do
  print $ "-- unittestCalculatorIntegerPrimitives"
  print $
    if  testapplication == testanswers
      && (foldl (&&) True [integerproperties "AbsPositive" [n] | 
                 n<-[-14568766308,-397537950,-5605646,33456745,1329348346255]++[-200 .. 200]])
      && (foldl (&&) True [integerproperties "IdivRem" [n1,n2] | 
                 n1<-[-545679652357775417,-709,-560,-350,24377,334,709782,545634717,187654567653]++[-200 .. 200], 
                 n2<-[-545679652357775415,-256,-235,-334,334,1876,3467419,123114216,7097825456717]++[-200 .. 200]])
    then "Unit tests hold"
    else "Tests failed"


-- | Applying each integer operation to a computed test data list for its arguments.
-- This allows checking that every operation declared in integeroperations has a semantics.
testapplication :: [Integer]
testapplication = (map testfunction integeroperations)

-- | Expected result of calling each integer operation on a computed test data list for its arguments.
testanswers :: [Integer]
testanswers = [
  -- _+_ _*_ _-_ -_ abs sqr
  21, 110, -1, -10, 10, 100,
  -- idiv rem gcd lcm
  0, 10, 1, 110,
  -- succ pred
  11, 9
  ]

-- | Turns a Primitive declaration into a call of the primitive function with an argument list
-- and calls integersemantics to compute the resulting value of the function with arguments.
testfunction :: Primitive -> Integer
testfunction (fname,plist,res,doc) = integersemantics fname (testdatalist plist)

-- | Creates test data (integer numbers) from a declared parameter list: one integer per argument
testdatalist :: [TypeName] -> [Integer]
testdatalist plist = [10..9+toInteger (length plist)]


-----------------------
-- | Interactive calculator with variables and given selection of integer operations.
{- | Run the following commands in sequence at the prompt
SetVar "Reg4" (Lit 4)
SetVar "Reg1" (Fun "Neg" [Fun "Mult" [Fun "Add" [Lit 3,Fun "Sub" [Lit 7,Lit 13]],Lit 19]])
SetVar "Reg2" (Fun "Add" [Var "Reg1",Var "Reg4"])
AssVar "Reg1" (Var "Reg2")
AssVar "Reg1" (Fun "Add" [Var "Reg1",Var "Reg4"])
AssVar "Reg1" (Fun "Add" [Var "Reg1",Var "Reg4"])
SetVar "v9" (Fun "Add" [Fun "Mult" [Fun "Idiv" [Var "Reg1",Var "Reg4"],Var "Reg4"], Fun "Rem" [Var "Reg1",Var "Reg4"]])
show

-}
main = do
  mainCalculatorPrint integeroperations
  mainCalculatorVariableAsk integersemantics newstate

