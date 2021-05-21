-- | AST for variable based calculator with 
-- • open ended set of primitive functions
-- • on an arbirtrary value domain.
--
-- Author Magne Haveraaen
-- Since 2020-03-26, revised 2021-03-03

module CalculatorValueDomainAST where


-----------------------
-- | Expressions for a calculator with variables.
-- The calculator supports literals Lit for any selected value domain,
-- an open ended set of primitive functions Fun, and
-- an open ended set of variables Var.
data CalcExprAST valuedomain
  = Lit valuedomain
  | Fun String [CalcExprAST valuedomain]
  | Var String
  deriving (Eq, Read, Show)


-- | Statement for declaring (setting) and changing (assigning) a variable
data CalcStmtAST valuedomain
  = SetVar String (CalcExprAST valuedomain)
  | AssVar String (CalcExprAST valuedomain)
  deriving (Eq, Read, Show)



-----------------------
-- | A primitive function declaration defines 
-- a function name, a list of parameter types, a return type, and a documentation string.
-- A documentation string can have embedded new lines.
type Primitive = (FunctionName,[TypeName],TypeName,DocString)

-- | Specific type names to differentiate between distinct purposes: function names.
type FunctionName = String
-- | Specific type names to differentiate between distinct purposes: type names.
type TypeName = String
-- | Specific type names to differentiate between distinct purposes: documentation strings.
type DocString = String

-- | The semantics of a call of a primitive function is a mapping 
-- from the function name (String) and argument list [valuedomain] to the resulting value.
type PrimitiveSemantics valuedomain = FunctionName -> [valuedomain] -> valuedomain


-----------------------
-- | The declarations of primitive functions is a list of primitives.
-- | Here we have getters on such declarations and we have a consistency checker.

-- | Get the names of all declared primitive operations
getNames :: [Primitive] -> [FunctionName]
getNames (decl@(fname,plist,res,doc):ops) = fname:getNames ops
getNames [] = []

-- | Gets the full declaration for a primitive function.
getDeclaration :: [Primitive] -> String -> Maybe Primitive
getDeclaration (decl@(fname,plist,res,doc):ops) str = if fname == str then Just decl else getDeclaration ops str
getDeclaration [] str = Nothing

-- | Gets the parameter list types for a primitive function.
getParams :: [Primitive] -> String -> Maybe [TypeName]
getParams ((fname,plist,res,doc):ops) str = if fname == str then Just plist else getParams ops str
getParams [] str = Nothing

-- | Gets the result type for a primitive function.
getResult :: [Primitive] -> String -> Maybe TypeName
getResult ((fname,plist,res,doc):ops) str = if fname == str then Just res else getResult ops str
getResult [] str = Nothing

-- | Gets the result type for a primitive function.
getDoc :: [Primitive] -> String -> Maybe DocString
getDoc ((fname,plist,res,doc):ops) str = if fname == str then Just res else getDoc ops str
getDoc [] str = Nothing

-- | Find duplicate declarations for a function name in a set of operation declarations.
findDuplicate :: [Primitive] -> Maybe String
findDuplicate ((fname,plist,res,doc):ops) = case getDeclaration ops fname of
  Just decl -> Just ("Function name " ++ (show fname) ++ " declared multiple times.")
  Nothing -> findDuplicate ops
findDuplicate [] = Nothing

-- | Check the property that looking up a declaration is the same as looking up a declaration's components.
checkGetterProperty :: [Primitive] -> String -> Bool
checkGetterProperty prims str =
  checkMaybePrimitiveTuple 
    (getDeclaration prims str) 
    (Just str, getParams prims str,getResult prims str,getDoc prims str)
-- | Check if a primitive declaration (possibly missing) is equal to the tuple its getters
checkMaybePrimitiveTuple :: Maybe Primitive -> (Maybe FunctionName, Maybe [TypeName], Maybe TypeName, Maybe DocString) -> Bool
checkMaybePrimitiveTuple (Just decl) (Just fname, Just plist, Just res, Just doc) = True 
checkMaybePrimitiveTuple Nothing (fname,Nothing,Nothing,Nothing) = True 
checkMaybePrimitiveTuple decl prim = error $ "Operations lookup mismatch: decl=" ++ (show decl) ++ " prim=" ++ (show prim)

-- | Unit tests for the operation list getters and find duplicate function.
unittestprimitives = do
  print $ "unittestprimitives"
  let f0 = "False"
  let f1 = "Not"
  let f2 = "And"
  let b1 = "no such function"
  if   checkGetterProperty noPrimitives b1
    && checkGetterProperty boolPrimitives b1
    && checkGetterProperty boolPrimitives f0
    && checkGetterProperty boolPrimitives f1
    && checkGetterProperty boolPrimitives f2
    && findDuplicate noPrimitives == Nothing 
    && findDuplicate boolPrimitives == Nothing 
    && case findDuplicate (boolPrimitives++boolPrimitives) of Nothing -> False ; Just _ -> True
    && case findDuplicate (("False",[],"Boolean","Falsehood."):boolPrimitives) of Nothing -> False ; Just _ -> True
    && case findDuplicate (boolPrimitives++("Xor",[],"Boolean","Falsehood."):[]) of Nothing -> False ; Just _ -> True
  then print $ "Unit tests passed!"
  else print $ "FAILURE"


-----------------------

-----------------------
-- | A default for primitive functions is the empty list of declarations.
noPrimitives :: [Primitive]
noPrimitives = []

-- | Declaring a test example with booleans.
boolPrimitives :: [Primitive]
boolPrimitives = [
   ("And",["Boolean","Boolean"],"Boolean","Conjunction of the truth values."),
   ("Not",["Boolean"],"Boolean","Negates the truth value."),
   ("Or",["Boolean","Boolean"],"Boolean","Disjunction of the truth values."),
   ("Xor",["Boolean","Boolean"],"Boolean","Exclusive or of the truth values."),
   ("=>",["Boolean","Boolean"],"Boolean","Implication for the truth values."),
   ("<=>",["Boolean","Boolean"],"Boolean","Equivalance of the truth value."),
   ("False",[],"Boolean","Falsehood.")
  ]

-- | Semantics of chosen real operations.
boolsemantics :: PrimitiveSemantics Bool
boolsemantics "And" [i1,i2] = i1 && i2
boolsemantics "Not" [i] = not i
boolsemantics "Or" [i1,i2] = i1 || i2
boolsemantics "Xor" [i1,i2] = not (i1 == i2)
boolsemantics "=>" [i1,i2] = (not i1) || i2
boolsemantics "<=>" [i1,i2] = i1 == i2
boolsemantics "False" [] = False
boolsemantics fname alist 
  = error $ "Unknown function name/arg list " ++ fname ++ (show alist)


unittestboolprimitives = do
  print $ "unittestboolprimitives"
  if foldl (&&) True (map (\(x,y)->x==y) 
       (zip 
        [boolsemantics op (map (\x -> True) (case getParams boolPrimitives op of Just params -> params ; Nothing -> [])) | op<-getNames boolPrimitives] 
        [True,False,True,False,True,True,False]))
  -- if True
  then print $ "Unit tests passed!"
  else print $ "FAILURE"
