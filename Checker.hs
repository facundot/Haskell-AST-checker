----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores, 
-- o la lista de errores encontrados en otro caso.   
----------------------------------------------------------------------------


module Checker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Monoid (Monoid(..))

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type


instance Semigroup Checked where
  Ok <> Ok = Ok
  Ok <> (Wrong ws) = Wrong ws
  (Wrong ws) <> Ok = Wrong ws
  (Wrong ws1) <> (Wrong ws2) = Wrong (ws1 ++ ws2)

instance Monoid Checked where
  mempty = Ok
            
instance Eq Checked where
  Ok == Ok = True
  Wrong _ == Wrong _ = True
  _ == _ = False

instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)
   = "The number of parameters in the definition of "++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)
   = "The number of arguments in the application of: " ++ n ++
     " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty')
   = "Expected: " ++ show ty ++ " Actual: " ++ show ty'

checkProgram :: Program -> Checked
checkProgram program =
  let
    result1 = checkUniqueNames program
    result2 = checkNumParams program
    result3 = checkProgramUndefinedParams program
    result4 = checkTypes program
  in
    if result1 /= Ok
      then result1
      else if result2 /= Ok
             then result2
             else if result3 /= Ok
                    then result3
                    else result4


----------------------------------------------------------------------------
-- unique names
----------------------------------------------------------------------------

checkUniqueNames :: Program -> Checked
checkUniqueNames (Program defs _) =
  let allNames = getFunctionNames defs
      duplicatedNames = findAllDuplicates allNames
      argDuplicates = findDuplicatesInFunDefs defs
      allDuplicates = duplicatedNames ++ argDuplicates
  in
    if null allDuplicates
      then Ok
      else Wrong (map Duplicated allDuplicates)

getFunctionNames :: Defs -> [Name]
getFunctionNames defs = map getFunctionName defs

getFunctionName :: FunDef -> Name
getFunctionName (FunDef (name, _) _ _) = name

findDuplicatesInFunDefs :: Defs -> [Name]
findDuplicatesInFunDefs defs = concatMap getArgumentDuplicates defs

getArgumentDuplicates :: FunDef -> [Name]
getArgumentDuplicates (FunDef _ args _) = findAllDuplicates args

findAllDuplicates :: Eq a => [a] -> [a]
findAllDuplicates xs = go xs [] []
  where
    go [] _ result = reverse result
    go (x:xs) seen result
      | x `elem` seen = go xs seen (insert x result)
      | otherwise = go xs (x:seen) result

    insert x xs = x:xs

----------------------------------------------------------------------------
-- Number of params
----------------------------------------------------------------------------

checkNumParams :: Program -> Checked
checkNumParams (Program defs _) = foldMap checkFunDefNumParams defs

checkFunDefNumParams :: FunDef -> Checked
checkFunDefNumParams (FunDef (name, sig) params _) =
  if numParamsSig == numParamsFunc
    then Ok
    else Wrong [ArgNumDef name numParamsSig numParamsFunc]
  where
    numParamsSig = countParamsSig sig
    numParamsFunc = length params

countParamsSig :: Sig -> Int
countParamsSig (Sig params _) = length params


----------------------------------------------------------------------------
-- Undefined variables 
----------------------------------------------------------------------------

checkProgramUndefinedParams :: Program -> Checked
checkProgramUndefinedParams (Program defs expr) =
  checkDefsUndefinedParams defs (collectDefinedNames defs)  <> checkExprUndefinedParams expr (collectDefinedNames defs)

checkDefsUndefinedParams :: Defs -> [Name] -> Checked
checkDefsUndefinedParams defs env = foldMap checkFunDefUndefinedParams defs env 

checkFunDefUndefinedParams :: FunDef -> [Name] -> Checked
checkFunDefUndefinedParams (FunDef (name, _) params expr) env =
  checkExprUndefinedParams expr (env ++ params)

collectDefinedNames :: Defs -> [Name]
collectDefinedNames = concatMap collectDefinedNamesFromDef

collectDefinedNamesFromDef :: FunDef -> [Name]
collectDefinedNamesFromDef (FunDef (name, _) _ _) = [name]

checkExprUndefinedParams :: Expr -> [Name] -> Checked
checkExprUndefinedParams (Var name) definedNames
  | name `elem` definedNames = Ok
  | otherwise = Wrong [Undefined name]

checkExprUndefinedParams (Infix _ e1 e2) definedNames =
  checkExprUndefinedParams e1 definedNames <> checkExprUndefinedParams e2 definedNames

checkExprUndefinedParams (If cond e1 e2) definedNames =
  checkExprUndefinedParams cond definedNames <>
  checkExprUndefinedParams e1 definedNames <>
  checkExprUndefinedParams e2 definedNames

checkExprUndefinedParams (Let (name, _) e1 e2) definedNames =
  checkExprUndefinedParams e1 definedNames <>
  checkExprUndefinedParams e2 (name : definedNames)

checkExprUndefinedParams (App name exprs) definedNames =
  checkUndefinedFunction name definedNames <> foldMap (`checkExprUndefinedParams` definedNames) exprs

checkExprUndefinedParams _ _ = Ok

checkUndefinedFunction :: Name -> [Name] -> Checked
checkUndefinedFunction name definedNames
  | name `elem` definedNames = Ok
  | otherwise = Wrong [Undefined name]

----------------------------------------------------------------------------
-- Type check
----------------------------------------------------------------------------  
checkTypes :: Program -> Checked
checkTypes (Program defs expr) =
  let
    definedFunctionsEnv = collectFunctionsDefsReturnType defs 
    result1 = checkDefs defs definedFunctionsEnv 
    result2 = checkExpr expr definedFunctionsEnv []
  in  
    result1 <> result2 


emptyEnv::Env
emptyEnv = []

type FunctionsEnv = [(Name,Sig)]

collectFunctionsDefsReturnType :: Defs -> FunctionsEnv        
collectFunctionsDefsReturnType defs = concatMap collectFunDefReturn defs

collectFunDefReturn :: FunDef -> FunctionsEnv                  
collectFunDefReturn (FunDef (name, sig) params _) = [(name,sig)]


getReturnTypes :: Sig -> Type
getReturnTypes (Sig _ returnType) = returnType

getArgumentsTypes ::Sig -> [Type]
getArgumentsTypes (Sig types _) = types


checkDefs :: Defs -> FunctionsEnv -> Checked
checkDefs defs functionsEnv = foldMap (\def -> checkFunDef def functionsEnv (buildLocalEnv def)) defs

buildLocalEnv::FunDef -> Env
buildLocalEnv (FunDef (name, sig) params expr) = 
  let
    domian = getArgumentsTypes sig
    localEnv = zip params domian
  in
    localEnv

checkFunDef :: FunDef -> FunctionsEnv -> Env -> Checked
checkFunDef (FunDef (name, sig) params expr) functionsEnv localEnv = checkdefActualReturnType (getReturnTypes sig) expr functionsEnv localEnv <>  checkExpr expr functionsEnv localEnv

checkdefActualReturnType ::Type -> Expr -> FunctionsEnv -> Env -> Checked
checkdefActualReturnType ret expr functionsEnv localEnv = 
  let
    actualRetType = getTypeOfExpr expr functionsEnv localEnv
    returnTypeCheck = case actualRetType of
                        Right actualRetType -> actualRetType == ret
                        Left _ -> False
  in
    case (actualRetType,returnTypeCheck) of
      (Right ty,False) -> Wrong [Expected ret ty]
      (Right ty,True) -> Ok 


checkExpr :: Expr -> FunctionsEnv -> Env -> Checked
checkExpr (Var name) _ env =
  case lookup name env of
    Just _ -> Ok
    Nothing -> Wrong [Undefined name]

checkExpr (IntLit _) _ _ = Ok

checkExpr (BoolLit _) _ _ = Ok

checkExpr (Infix op e1 e2) functionsEnv env =
  let
    result1 = checkExpr e1 functionsEnv env
    result2 = checkExpr e2 functionsEnv env
    type1 = getTypeOfExpr e1 functionsEnv env
    type2 = getTypeOfExpr e2 functionsEnv env
  in
    case (op, type1, type2) of
      (Add, Right TyInt, Right TyInt) -> Ok <> result1 <> result2
      (Add, Right TyBool, Right TyInt) -> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Add, Right TyInt, Right TyBool) -> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Add, Right TyBool, Right TyBool) -> Wrong [Expected TyInt TyBool] <> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Sub, Right TyInt, Right TyInt) -> Ok <> result1 <> result2
      (Sub, Right TyBool, Right TyInt) -> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Sub, Right TyInt, Right TyBool) -> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Sub, Right TyBool, Right TyBool) -> Wrong [Expected TyInt TyBool] <> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Mult, Right TyInt, Right TyInt) -> Ok <> result1 <> result2
      (Mult, Right TyBool, Right TyInt) -> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Mult, Right TyInt, Right TyBool) -> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Mult, Right TyBool, Right TyBool) -> Wrong [Expected TyInt TyBool] <> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Div, Right TyInt, Right TyInt) -> Ok <> result1 <> result2
      (Div, Right TyBool, Right TyInt) -> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Div, Right TyInt, Right TyBool) -> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Div, Right TyBool, Right TyBool) -> Wrong [Expected TyInt TyBool] <> Wrong [Expected TyInt TyBool] <> result1 <> result2
      (Eq, Right ty1, Right ty2) | ty1 == ty2 -> Ok <> result1 <> result2
      (Eq, Right ty1, Right ty2)  -> Wrong [Expected ty1 ty2] <> result1 <> result2
      (NEq, Right ty1, Right ty2) | ty1 == ty2 -> Ok <> result1 <> result2
      (NEq, Right ty1, Right ty2)  -> Wrong [Expected ty1 ty2] <> result1 <> result2
      (GTh, Right ty1, Right ty2) | ty1 == ty2 -> Ok <> result1 <> result2
      (GTh, Right ty1, Right ty2)  -> Wrong [Expected ty1 ty2] <> result1 <> result2
      (LTh, Right ty1, Right ty2) | ty1 == ty2 -> Ok <> result1 <> result2
      (LTh, Right ty1, Right ty2)  -> Wrong [Expected ty1 ty2] <> result1 <> result2
      (GEq, Right ty1, Right ty2) | ty1 == ty2 -> Ok <> result1 <> result2
      (GEq, Right ty1, Right ty2)  -> Wrong [Expected ty1 ty2] <> result1 <> result2
      (LEq, Right ty1, Right ty2) | ty1 == ty2 -> Ok <> result1 <> result2
      (LEq, Right ty1, Right ty2)  -> Wrong [Expected ty1 ty2] <> result1 <> result2
      

checkExpr (If cond e1 e2) functionsEnv env =
  let
    result1 = checkExpr cond functionsEnv env
    result2 = checkExpr e1 functionsEnv env
    result3 = checkExpr e2 functionsEnv env
    typeCond = getTypeOfExpr cond functionsEnv env
    type1 = getTypeOfExpr e1 functionsEnv env
    type2 = getTypeOfExpr e2 functionsEnv env
  in
    case (typeCond, type1, type2) of
      (Right TyBool, Right type1, Right type2) | type1 == type2 -> Ok <> result1 <> result2 <> result3
      (Right TyBool, Right type1, Right type2) -> Wrong [Expected  type1  type2] <> result1 <> result2 <> result3
      (Right TyInt, Right type1, Right type2) | type1 == type2 -> Wrong [Expected TyBool TyInt] <> result1 <> result2 <> result3
      (Right TyInt, Right type1, Right type2) -> Wrong [Expected TyBool TyInt] <> Wrong [Expected  type1  type2] <> result1 <> result2 <> result3
      

checkExpr (Let (name, ty) e1 e2) functionsEnv env =
  let
    extendedEnv = (name, ty) : env
    result1 = checkExpr e1 functionsEnv env
    result2 = checkExpr e2 functionsEnv extendedEnv
    type1 = getTypeOfExpr e1 functionsEnv env
    in
    case type1 of
      Right type1 | type1 == ty -> Ok <> result1 <> result2
      Right type1 ->  Wrong [Expected ty type1]<>  result1 <> result2 


checkExpr (App name exprs) functionsEnv env =
  case lookup name functionsEnv of
    Just (Sig argTypes retType) ->
     let
        argResults = zipWith (\expr argType -> checkExpr expr functionsEnv env <> checkType expr argType functionsEnv env) exprs argTypes
        expectedArgNum = length argTypes
        actualArgNum = length exprs
      in
        if expectedArgNum == actualArgNum
          then foldMap id argResults
          else Wrong [ArgNumApp name expectedArgNum actualArgNum] <> foldMap id argResults
    Nothing -> Wrong [Undefined name]

checkType :: Expr -> Type -> FunctionsEnv -> Env -> Checked
checkType expr expectedType functionsEnv env =
  case getTypeOfExpr expr functionsEnv env of
    Right actualType ->
      if actualType == expectedType
        then Ok
        else Wrong [Expected expectedType actualType]
    Left error -> Wrong [error]    

getTypeOfExpr :: Expr -> FunctionsEnv -> Env -> Either Error Type
getTypeOfExpr (Var name) _ env =
  case lookup name env of
    Just ty -> Right ty
    Nothing -> Left (Undefined name)

getTypeOfExpr (IntLit _) _ _ = Right TyInt

getTypeOfExpr (BoolLit _) _ _ = Right TyBool

getTypeOfExpr (Infix op e1 e2) functionsEnv env =
    case op of
      Add -> Right TyInt
      Sub -> Right TyInt
      Mult -> Right TyInt
      Div -> Right TyInt
      Eq -> Right TyBool
      NEq -> Right TyBool
      GTh ->Right TyBool
      LTh -> Right TyBool
      GEq -> Right TyBool
      LEq -> Right TyBool
      

getTypeOfExpr (If cond e1 e2) functionsEnv env =
  let
    type1 = getTypeOfExpr e1 functionsEnv env
  in
    case type1 of
      Right type1 -> Right type1
      
      
      
getTypeOfExpr (Let (var,ty) e1 e2) functionsEnv env =
  let
    type1 = getTypeOfExpr e2 functionsEnv ( (var,ty): env )
    in
    case type1 of
      Right ty -> Right ty
      

getTypeOfExpr (App name exprs) functionsEnv env =
  case lookup name functionsEnv of
    Just (Sig _ retType) -> Right retType
    Nothing -> Left (Undefined name)