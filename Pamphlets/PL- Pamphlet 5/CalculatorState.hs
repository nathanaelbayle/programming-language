-- | Semantics for variable based integer calculator.
-- It uses State to keep track of variables and their values.
-- This is separated into a environment which keeps track of variables, and
-- a store which keeps track of their (changing) values.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module CalculatorState where

-- Use Haskell's array data structure
import Data.Array


-----------------------
-- | A state is an environment of variable-store index associations, and
-- a store which at each store index keeps a value (for that variable).
type State = (Environment,Store)

-- | A new state is an empty environment with an empty store.
newstate :: State
newstate = (emptyenvironment,emptystore)

-- | Gets the value linked to the variable in the state.
getvalue :: String -> State -> Integer
getvalue vname (env,store) = 
  case lookup vname env of
    Just loc -> getstore store loc
    Nothing ->  error $ "Variable " ++ vname ++ " not found in state " ++ (show env)

-- | Add a new variable with value to the state.
addvariable :: String -> Integer -> State -> State
addvariable vname value (env,store) = (newenv,newstore)
  where
    (newhigh,newstore) = enlargestore store value
    newenv = addenv vname newhigh env

-- | Changes the value associated with a known variable.
changevalue :: String -> Integer -> State -> State
changevalue vname value (env,store) = (env,newstore) where
  newstore = case lookup vname env of
    Just loc -> setstore loc value store
    Nothing ->  error $ "Variable " ++ vname ++ " not found in state " ++ (show env)

-----------------------
-- | An Environemnt for a calculator with variables.
-- It stores an association list of distinct variable names and their store index.
-- As such, it can be searched by the Haskell standard function 
--   lookup :: Eq a => a -> [(a, b)] -> Maybe b
type Environment = [(String,Integer)]

-- | Defines an empty environment
emptyenvironment :: Environment
emptyenvironment = []

-- | Add a new variable (and a store index) to the environment.
addenv :: String -> Integer -> Environment -> Environment
addenv vname ind env =
  case lookup vname env of
    Just loc -> error $ "New variable " ++ (show (vname,ind)) 
            ++ " already registered in " ++ (show env)
    Nothing ->  (vname,ind):env 


-----------------------
-- | A Store for a calculator is an array where the number of indices
-- corresponds to the number of distinct variables.
type Store = Array Integer Integer

-- | Defines an empty store
emptystore :: Store
emptystore = array (0,-1) []

-- | Get the value stored for the given index.
getstore :: Store -> Integer -> Integer
getstore store ind = 
  if low <= ind && ind <= high 
  then store ! ind 
  else error $ "Not a store index " ++ (show ind)
  where (low,high) = bounds store

-- | Set the value stored at the given index.
setstore :: Integer -> Integer -> Store -> Store
setstore ind val store =
  if low <= ind && ind <= high 
  then store // [(ind,val)] 
  else error $ "Not a store index " ++ (show ind) ++ " for " ++ (show val)
  where (low,high) = bounds store

-- | Get next store index and increase store size with one and set value at new location.
enlargestore :: Store -> Integer -> (Integer,Store)
enlargestore store value = (newhigh,newstore)
  where
    (low,high) = bounds store
    newhigh = high + 1
    storelist = assocs store
    newstore = array (low,newhigh) (storelist++[(newhigh,value)])


-----------------------
-- | Unit tests for State.
unittestCalculatorState = do
  print $ "-- unittestCalculatorState"
  -- putStrLn \$ "Empty state = " ++ (show newstate)
  let state1 = addvariable "v1" 1 newstate
  let state2 = addvariable "v2" 4 state1
  let state3 = addvariable "v3" 9 state2
  let state4 = changevalue "v2" 25 state3
  -- putStrLn \$ "Value of v1 == " ++ (show \$ getvalue "v1" state4)
  -- putStrLn \$ "Value of v2 == " ++ (show \$ getvalue "v2" state4)
  -- putStrLn \$ "Value of v3 == " ++ (show \$ getvalue "v3" state4)
  -- putStrLn \$ "State3 = " ++ (show state3)
  putStrLn $
    if (1 == (getvalue "v1" state4) )
    && (25 == (getvalue "v2" state4) )
    && (9 == (getvalue "v3" state4) )
    then "Unit tests hold"
    else "Tests failed"
