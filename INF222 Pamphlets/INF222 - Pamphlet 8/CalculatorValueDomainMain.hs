-- | Semantics for variable based calculator with an open ended set of primitive functions for any value domain.
-- The variables are linked to values in State.
-- The open ended set of primitive functions on an arbitrary value domain is handled by external parameters.
--
-- Author Magne Haveraaen
-- Since 2020-03-19, revised 2021-03-05

module CalculatorValueDomainMain where

-- Use variable calculator with open ended primitives AST.
import CalculatorValueDomainAST
import CalculatorValueDomainInterpreter

-- Use variable calculator state.
import CalculatorValueDomainState


-----------------------
-- | Print help information for calculator: welcome message and listing of operations.
mainCalculatorPrint :: [Primitive] -> IO ()
mainCalculatorPrint operations = do
  putStrLn $ "-- Interactive calulator with variables and open ended set of operations."
  putStrLn $ "-- The set of primitive functions (and their arguments):"
  putStr $ foldl (++) "" 
    ( map 
      (\(fn,args,res,doc) -> 
        (printdoc doc) ++ 
        "  " ++ fn ++ " :: " ++ (foldr listargcomma "" args) ++ " -> " ++ res ++ "\n"
      ) 
      operations
    )

-- | Interactive calculator with variables and open ended set of primitive functions.
-- • Stops if input string is empty.
-- • Shows current value of state when "show" is input.
-- • Parses and executes  CalcStmtAST and prints value of updated variable otherwise.
-- The semantics of the primitive functions comes via the primsem function.
mainCalculatorVariable :: (Show valuedomain, Read valuedomain) => PrimitiveSemantics valuedomain -> IO ()
mainCalculatorVariable primsem = do
  print $ "-- Interactive calulator with variables."
  mainCalculatorVariableAsk primsem newstate


-----------------------
-- | Interactive part of calculator with variables and open ended set of primitive functions.
-- Reads user input and handles the different cases:
-- • Stops if input string is empty.
-- • Shows current value of state when "show" is input.
-- • Parses and executes CalcStmtAST and prints value of updated variable otherwise.
mainCalculatorVariableAsk :: (Show valuedomain, Read valuedomain) => PrimitiveSemantics valuedomain -> State valuedomain -> IO ()
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
mainCalculatorVariableShowstate :: (Show valuedomain, Read valuedomain) => PrimitiveSemantics valuedomain -> State valuedomain -> IO ()
mainCalculatorVariableShowstate primsem state = do
  putStrLn $ "state = " ++ (show state)
  mainCalculatorVariableAsk primsem state

-- | Parses and executes CalcStmtAST and prints subexpression value.
-- Then continues with interactive register calculator.
mainCalculatorVariableExc :: (Show valuedomain, Read valuedomain) => 
  PrimitiveSemantics valuedomain -> String -> State valuedomain -> IO ()
mainCalculatorVariableExc primsem str state = do
  let stmt = (read str)
  let vname = getstatementupdatevariable stmt
  let state' = execute primsem stmt state
  -- puStrLn $ "execute(" ++ (show stmt) ++ ") state == " ++ (show $ state')
  putStrLn $ "Variable " ++ vname ++ " = " ++ (show $ getvalue vname state')
  mainCalculatorVariableAsk primsem state'

-- | Helper function to extract updated variable name from statement.
getstatementupdatevariable :: CalcStmtAST valuedomain -> String
getstatementupdatevariable (SetVar vname expr) = vname
getstatementupdatevariable (AssVar vname expr) = vname
-- getstatementupdatevariable stmt = error $ "Unknown statement " ++ (show stmt)



-----------------------
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

