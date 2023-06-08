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
import Data.List (nub, (\\))
import qualified Data.Map as Map
import Data.Monoid (Monoid(..))

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type

instance Eq Checked where
  Ok == Ok = True
  Wrong _ == Wrong _ = True
  _ == _ = False

instance Semigroup Checked where
  Ok <> Ok = Ok
  Ok <> (Wrong ws) = Wrong ws
  (Wrong ws) <> Ok = Wrong ws
  (Wrong ws1) <> (Wrong ws2) = Wrong (ws1 ++ ws2)

instance Monoid Checked where
  mempty = Ok
            
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
    result4 = Ok
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
  checkDefsUndefinedParams defs <> checkExprUndefinedParams expr (collectDefinedNames defs)

checkDefsUndefinedParams :: Defs -> Checked
checkDefsUndefinedParams = foldMap checkFunDefUndefinedParams

checkFunDefUndefinedParams :: FunDef -> Checked
checkFunDefUndefinedParams (FunDef (name, _) params expr) =
  checkExprUndefinedParams expr (name:params)

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
{-checkTypes :: Program -> Checked
checkTypes (Program defs expr) = checkExpr expr env
  where
    env = buildEnv defs

-- Construye el ambiente (entorno) a partir de las definiciones de funciones
buildEnv :: Defs -> Env
buildEnv = foldr extendEnv []

-- Extiende el ambiente (entorno) con una nueva variable y su tipo
extendEnv :: FunDef -> Env -> Env
extendEnv (FunDef (name, Sig argTypes returnType) _ _) env = (name, returnType) : env

-- Comprueba el tipo de una expresión en un ambiente dado
checkExpr :: Expr -> Env -> Checked
checkExpr (Var name) env =
  case lookup name env of
    Just _ -> Ok
    Nothing -> Wrong [Undefined name]
checkExpr (IntLit _) _ = Ok
checkExpr (BoolLit _) _ = Ok
checkExpr (Infix op e1 e2) env =
  case op of
    Add -> checkBinaryOp e1 e2 TyInt
    Sub -> checkBinaryOp e1 e2 TyInt
    Mult -> checkBinaryOp e1 e2 TyInt
    Div -> checkBinaryOp e1 e2 TyInt
    Eq -> checkBinaryOp e1 e2 TyBool
    NEq -> checkBinaryOp e1 e2 TyBool
    GTh -> checkBinaryOp e1 e2 TyBool
    LTh -> checkBinaryOp e1 e2 TyBool
    GEq -> checkBinaryOp e1 e2 TyBool
    LEq -> checkBinaryOp e1 e2 TyBool
  where
    checkBinaryOp e1 e2 expectedType =
      let result1 = checkExpr e1 env
          result2 = checkExpr e2 env
       in result1 <> result2 <> checkTypeMatch e1 e2 expectedType
checkExpr (If cond e1 e2) env =
  let result1 = checkExpr cond env
      result2 = checkExpr e1 env
      result3 = checkExpr e2 env
   in result1 <> result2 <> result3 <> checkTypeMatch e1 e2 (getType e1 env)
checkExpr (Let (name, varType) e1 e2) env =
  let result1 = checkExpr e1 env
      result2 = checkExpr e2 ((name, varType) : env)
   in result1 <> result2 <> checkTypeMatch e1 e2 varType
checkExpr (App name args) env =
  case lookup name env of
    Just (Sig argTypes returnType) ->
      let argResults = map (\arg -> checkExpr arg env) args
          argMismatchErrors = zipWith3 checkArgNum name argTypes args argResults
       in foldr (<>) (checkReturnType returnType (getType (last args) env)) argResults <> foldr (<>) mempty argMismatchErrors
    Nothing -> Wrong [Undefined name]

-- Comprueba si dos expresiones tienen el mismo tipo
checkTypeMatch :: Expr -> Expr -> Type -> Checked
checkTypeMatch e1 e2 expectedType =
  if getType e1 env == getType e2 env && getType e1 env == expectedType
    then Ok
    else Wrong [Expected (getType e1 env) expectedType]

-- Comprueba si el número de argumentos en la aplicación coincide con la definición de la función
checkArgNum :: Name -> [Type] -> [Expr] -> Checked -> Checked
checkArgNum name argTypes args argResults =
  let expectedNum = length argTypes
      actualNum = length args
   in if expectedNum == actualNum
        then Ok
        else Wrong [ArgNumApp name expectedNum actualNum]

-- Obtiene el tipo de una expresión en un ambiente dado
getType :: Expr -> Env -> Type
getType (Var name) env = case lookup name env of
  Just varType -> varType
  Nothing -> error "Variable not found in environment"
getType (IntLit _) _ = TyInt
getType (BoolLit _) _ = TyBool
getType (Infix _ e1 _) env = getType e1 env
getType (If _ e1 _) env = getType e1 env
getType (Let _ _ e2) env = getType e2 env
getType (App _ _) env = case lookup name env of
  Just (Sig _ returnType) -> returnType
  Nothing -> error "Function not found in environment"

-- Comprueba si el tipo de retorno de una expresión coincide con el tipo esperado
checkReturnType :: Type -> Type -> Checked
checkReturnType expectedType actualType =
  if expectedType == actualType
    then Ok
    else Wrong [Expected actualType expectedType] -}

