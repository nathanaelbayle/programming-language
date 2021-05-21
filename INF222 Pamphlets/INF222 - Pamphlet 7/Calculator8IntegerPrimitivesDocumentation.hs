-- | A selection of integer function declarations and their semantics.
--
-- Author Magne Haveraaen
-- Since 2020-03-26

module Calculator8IntegerPrimitivesDocumentation where

-- Use the AST format for calculators with an open set of operations, 
-- and documentation support in the function primitive declarations.
import CalculatorExternalPrimitivesDocumentationAST

-- Use the interpreter for calculators with an open set of operations.
import CalculatorExternalPrimitivesDocumentationInterpreter

-- Use calculator state for variables and store.
import CalculatorState


-----------------------
-- | Declaration of operations and their argument list & return type.
integeroperations :: [Primitive]
integeroperations
  = [ 
      ("Add", ["Integer","Integer"],"Integer","Add two integers"),
      ("Mult", ["Integer","Integer"],"Integer","Multiply two integers"),
      ("Sub", ["Integer","Integer"],"Integer","Subtract two integers"),
      ("Neg", ["Integer"],"Integer","Negate (change sign of) an integer"),
      ("Idiv", ["Integer","Integer"],"Integer","Integer division rounded towards zero"),
      ("Rem", ["Integer","Integer"],"Integer","Remainder when doing integer division"),
      ("Gcd", ["Integer","Integer"],"Integer",
       "Greatest common divisior of two integers,\ne.g., Gcd [10,15]==5"),
      ("Lcm", ["Integer","Integer"],"Integer",
       "Least common multiplier of two integers,\ne.g., Lcm [10,15]==30")
    ]

-----------------------
-- | Semantics of chosen integer operations.
integersemantics :: PrimitiveSemantics
integersemantics "Add" [i1,i2] = i1 + i2
integersemantics "Mult" [i1,i2] = i1 * i2
integersemantics "Sub" [i1,i2] = i1 - i2
integersemantics "Neg" [i] = - i
integersemantics "Idiv" [i1,0]
  = error $ "Cannot do integer division of " ++ (show i1) ++ " by 0."
integersemantics "Idiv" [i1,i2] = quot i1 i2
integersemantics "Rem" [i1,0] 
  = error $ "Cannot do remainder of " ++ (show i1) ++ " by 0."
integersemantics "Rem" [i1,i2] = rem i1 i2
integersemantics "Gcd" [i1,i2] = gcd i1 i2
integersemantics "Lcm" [i1,i2] = lcm i1 i2
integersemantics fname alist 
  = error $ "Unknown function name/arg list " ++ fname ++ (show alist)


-----------------------
-- | Unit test of the integeroperations:
-- for each declaration in integeroperations checks that there is a corresponding integersemantics.
unittestCalculatorIntegerPrimitives = do
  print $ "-- unittestCalculatorIntegerPrimitives"
  -- print $ show $ map testfunction integeroperations
  print $
    -- Expected result of calling the declared functions on relevant argument lists.
    if (map testfunction integeroperations) == [21,110,-1,-10,0,10,1,110]
    then "Unit tests hold"
    else "Tests failed"


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
  putStrLn $ "-- Interactive calulator with variables and open ended set of operations."
  putStrLn $ "The set of primitive functions (and their arguments):"
  -- putStrLn $ "  " ++ (show integeroperations)
  putStr $ foldl (++) "" 
    ( map 
      (\(fn,args,res,doc) -> 
        (printdoc doc) ++ 
        "  " ++ fn ++ " :: " ++ (foldr listargcomma "" args) ++ " -> " ++ res ++ "\n"
      ) 
      integeroperations
    )
  mainCalculatorVariableAsk integersemantics newstate

-- | Split a list into a list of lists on the delimiter element.
-- For instance splitOn 0 [1,2,0,4,0,0,5,7] = [[1,2],[4],[],[5,7]]
splitOn delimiter list = foldr f [[]] list
  where
    f c (x:xs) | c == delimiter = []:(x:xs)
               | otherwise = (c:x):xs

-- | Create a (multiline) documentation with "  -- |" prefix at each line.
printdoc doc = res
   where
     strs = splitOn '\n' doc
     lines = map (\str -> "  -- | " ++ str ++ "\n") strs
     res = foldr (++) "" lines


-- | Function for inserting a comma between two nonempty strings.
listargcomma str1 str2 = 
  if str1 == "" && str2 == ""
  then ""
  else
    if str1 == ""
    then str2
    else
      if str2 == ""
      then str1
      else str1 ++ ", " ++ str2

