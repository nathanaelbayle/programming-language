-- | A selection of real function declarations and their semantics.
--
-- Author Magne Haveraaen
-- Since 2020-03-26, revised 2021-03-03

module CalculatorValueDomainReal where

-- Use the AST format for calculators with an open set of documented operations.
import CalculatorValueDomainAST

-- Use the interpreter for calculators with an open set of operations.
import CalculatorValueDomainInterpreter
import CalculatorValueDomainMain

-- Use calculator state for variables and store.
import CalculatorValueDomainState


-----------------------
-- | Declaration of operations and their argument list & return type.
realoperations :: [Primitive]
realoperations
  = [ 
      ("Add", ["Real","Real"],"Real","Add two reals"),
      ("Mult", ["Real","Real"],"Real","Multiply two reals"),
      ("Sub", ["Real","Real"],"Real","Subtract two reals"),
      ("Neg", ["Real"],"Real","Negate (change sign of) a real"),
      ("Slash", ["Real","Real"],"Real","Division of two reals"),
      ("Abs", ["Real"],"Real","The absolute value (positive) a real"),
      ("Sqr", ["Real"],"Real","Squaring a real"),
      ("Sin", ["Real"],"Real","Sine of a real (radians)"),
      ("Cos", ["Real"],"Real","Cosine of a real (radians)"),
      ("Exp", ["Real"],"Real","Exponent of a real (natural exponent)"),
      ("Ln", ["Real"],"Real","Natural logarithm of a real"),
      ("Sqrt", ["Real"],"Real","Square root of a non-negative real"),
      ("Arctan", ["Real"],"Real","Arc tangent (inverse tangent) of a real (radians)\nfirst and fourth quadrant"),
      ("Idiv", ["Real","Real"],"Real","Integer division rounded towards zero"),
      ("Rem", ["Real","Real"],"Real","Remainder when doing integer division"),
      -- ("Gcd", ["Real","Real"],"Real",
      --  "Greatest common divisior of two reals,\ne.g., Gcd [10,15]==5"),
      -- ("Lcm", ["Real","Real"],"Real",
      --  "Least common multiplier of two reals,\ne.g., Lcm [10,15]==30"),
      ("Succ", ["Real"],"Real","Successor, adding 1 to a number"),
      ("Pred", ["Real"],"Real","Predessor, subtracting 1 from a number")
    ]

-----------------------
-- | Semantics of chosen real operations.
realsemantics :: PrimitiveSemantics Double
realsemantics "Add" [i1,i2] = i1 + i2
realsemantics "Mult" [i1,i2] = i1 * i2
realsemantics "Sub" [i1,i2] = i1 - i2
realsemantics "Neg" [i] = - i
realsemantics "Slash" [i1,0]
  = error $ "Cannot do real division (slash) of " ++ (show i1) ++ " by 0."
realsemantics "Slash" [i1,i2] = i1 / i2
realsemantics "Abs" [i] = abs i
realsemantics "Sqr" [i] = i * i
realsemantics "Sin" [i] = sin i
realsemantics "Cos" [i] = cos i
realsemantics "Exp" [i] = exp i
realsemantics "Ln" [i] = log i
realsemantics "Sqrt" [i] = sqrt i
realsemantics "Arctan" [i] = atan i
realsemantics "Idiv" [i1,0]
  = error $ "Cannot do integer division of " ++ (show i1) ++ " by 0."
realsemantics "Idiv" [i1,i2] = fromIntegral (truncate (i1 / i2))
realsemantics "Rem" [i1,0] 
  = error $ "Cannot do remainder of " ++ (show i1) ++ " by 0."
realsemantics "Rem" [i1,i2] = i1 - fromInteger (truncate (i1 / i2)) * i2
-- realsemantics "Gcd" [i1,i2] = gcd i1 i2
-- realsemantics "Lcm" [i1,i2] = lcm i1 i2
realsemantics "Succ" [i] = i + 1
realsemantics "Pred" [i] = i - 1
realsemantics fname alist 
  = error $ "Unknown function name/arg list " ++ fname ++ (show alist)


-----------------------
-- | Properties of some the real functions.
-- The arguments are: property name (String), argument list (real numbers).
realproperties :: String -> [Double] -> Bool
-- The absolute value of a positive number is always positive.
realproperties "AbsPositive" [n] = 
  if 0 <= realsemantics "Abs" [n]
    then True 
    else error $ "Property AbsPositive failes for n=" ++ (show n)
-- The square root of the square of a number is the number itself.
realproperties "SqrtSqr" [n] = 
  if realsemantics "Abs" [n] == realsemantics "Sqrt" [realsemantics "Sqr" [n]]
    then True 
    else error $ "Property SqrtSqr failes for n=" ++ (show n)
-- The natural logarithm of the natural exponent of a number is the number itself.
realproperties "LnExp" [n] = 
  if n == realsemantics "Ln" [realsemantics "Exp" [n]]
    then True 
    else error $ "Property LnExp failes for n=" ++ (show n)
-- Integer division and remainder relate as follows: n2 == (n1 idiv n2) * n2 + (n1 rem n2) when n2 /= 0.
realproperties "IdivRem" [n1,n2] = 
  if n2 == 0 
  || n1 == (realsemantics "Add" [realsemantics "Mult" [realsemantics "Idiv" [n1,n2],n2], realsemantics "Rem" [n1,n2]])
    then True 
    else error $ "Property IdivRem failes for n1=" ++ (show n1) ++ " n2=" ++ (show n2)



-----------------------
-- | Unit test of the realoperations:
-- for each declaration in realoperations checks that there is a corresponding realsemantics.
unittestCalculatorRealPrimitives = do
  print $ "-- unittestCalculatorRealPrimitives"
  print $
    if  testapplication == testanswers
      && (foldl (&&) True [realproperties "AbsPositive" [n] | n<-[-1.75e308,-3e50,-5e-60,3.34e-45,1.3e245]++[-200 .. 200]])
      && (foldl (&&) True [realproperties "SqrtSqr" [n] | n<-[-34567,12345678]++[-200 .. 200]])
      && (foldl (&&) True [realproperties "LnExp" [n] | n<-[-709.782,-560,-350,334,709.782]++[-200 .. 200]])
      && (foldl (&&) True [realproperties "IdivRem" [n1,n2] | 
                 n1<-[-5.4567e17,-709.782,-560,-350,-2.437e-7,5.4567e-17,1.87654e-3,334,709.782]++[-200 .. 200], 
                 n2<-[-709.782,-560,-3.50,-1.876e-6,4.34674e-19,1.234e-16,334,709.782,5.4567e17]++[-200 .. 200]])
    then "Unit tests hold"
    else "Tests failed"


-- | Applying each real operation to a computed test data list for its arguments.
-- This allows checking that every operation declared in realoperations has a semantics.
testapplication :: [Double]
testapplication = (map testfunction realoperations)

-- | Expected result of calling each real operation on a computed test data list for its arguments.
testanswers :: [Double]
testanswers = [
  -- _+_ _*_ _-_ -_ _/_ abs sqr
  21.0, 110.0, -1.0, -10.0, 0.9090909090909091, 10.0, 100.0,
  -- sin cos exp ln sqrt arctan
  -0.5440211108893699, -0.8390715290764524, 22026.465794806718, 2.302585092994046, 3.1622776601683795, 1.4711276743037345,
  -- idiv rem -- gcd lcm
  0, 10, -- 1, 110,
  -- succ pred
  11.0,9.0
  ]

-- | Turns a Primitive declaration into a call of the primitive function with an argument list
-- and calls realsemantics to compute the resulting value of the function with arguments.
testfunction :: Primitive -> Double
testfunction (fname,plist,res,doc) = realsemantics fname (testdatalist plist)

-- | Creates test data (real numbers) from a declared parameter list: one real per argument
testdatalist :: [TypeName] -> [Double]
testdatalist plist = [10.0 .. 9.0+fromIntegral (length plist)]


-----------------------
-- | Interactive calculator with variables and given selection of real operations.
{- | Run the following commands in sequence at the prompt
SetVar "Reg4" (Lit 4)
SetVar "Reg1" (Fun "Neg" [Fun "Mult" [Fun "Add" [Lit 3,Fun "Sub" [Lit 7,Lit 13]],Lit 19]])
SetVar "Reg2" (Fun "Add" [Var "Reg1",Var "Reg4"])
AssVar "Reg1" (Var "Reg2")
AssVar "Reg1" (Fun "Add" [Var "Reg1",Var "Reg4"])
AssVar "Reg1" (Fun "Add" [Var "Reg1",Var "Reg4"])
SetVar "v9" (Fun "Add" [Fun "Mult" [Fun "Idiv" [Var "Reg1",Var "Reg4"],Var "Reg4"], Fun "Rem" [Var "Reg1",Var "Reg4"]])
SetVar "ca0" (Fun "Sin" [Fun "Mult" [Fun "Arctan" [Lit 1], Lit 4]])
SetVar "ca1" (Fun "Ln" [Fun "Exp" [Lit 1]])
SetVar "π" (Fun "Mult" [Fun "Arctan" [Lit 1], Lit 4])
SetVar "e" (Fun "Exp" [Lit 1])
show

-}
main = do
  mainCalculatorPrint realoperations
  mainCalculatorVariableAsk realsemantics newstate
