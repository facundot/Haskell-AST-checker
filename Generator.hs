----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de generación de código C
--
-- Se debe implementar la función genProgram,
-- que dado un AST que representa un programa válido
-- genera el código C correspondiente.
----------------------------------------------------------------------------

module Generator where

import Syntax
import Control.Monad.State
import Data.List

-- CODE GENERATOR

type UniqueID = Int
type GenState = State UniqueID


genProgram :: Program -> String
genProgram (Program defs expr) =
  let (generatedExpr,_) = runState (genExpr expr) 0
  in
    "#include <stdio.h>\n" ++
    genFunctionDefs defs ++ 
    "int main() {\n" ++ runGenOptionalLetFunctions expr ++
    "printf(\"%d\\n\"," ++generatedExpr ++ ");" ++ 
    " }\n"

genFunctionDefs :: Defs -> String
genFunctionDefs [] = ""
genFunctionDefs (def:defs) = genFunctionDef def ++ genFunctionDefs defs

genFunctionDef :: FunDef -> String
genFunctionDef (FunDef (name, sig) params expr) =
  let (result, _) = runState (genOptionalLetFunctions expr) 0
      (generatedExpr,_) = runState (genExpr expr) 0
  in
    genType (returnType sig) ++ " _" ++ name ++ "(" ++ genParams params ++ "){\n" ++
    result ++ "return (" ++ generatedExpr ++ ");" ++
    " };\n"

genOptionalLetFunctions :: Expr -> GenState String
genOptionalLetFunctions (Let (var, _) bindExpr bodyExpr) = do
  bindingLet <- genOptionalLetFunctions bindExpr
  letFunc <- genLetFunction var bodyExpr
  return (bindingLet ++ letFunc)
genOptionalLetFunctions (If condExpr thenExpr elseExpr) = do
  cond <- genOptionalLetFunctions condExpr
  thenPart <- genOptionalLetFunctions thenExpr
  elsePart <- genOptionalLetFunctions elseExpr
  return (cond ++ thenPart ++ elsePart)
genOptionalLetFunctions (Infix _ expr1 expr2) = do
  left <- genOptionalLetFunctions expr1
  right <- genOptionalLetFunctions expr2
  return (left ++ right)
genOptionalLetFunctions (App _ args) = do
  results <- mapM genOptionalLetFunctions args
  return (concat results)
genOptionalLetFunctions _ = return ""

genLetFunction :: Name -> Expr -> GenState String
genLetFunction var bodyExpr = do
  letFuncBody <- genOptionalLetFunctions bodyExpr
  if isLetExpr bodyExpr
    then do
      uniqueID <- get
      modify (+1)    
      let genExprState = genExpr bodyExpr
      generatedBindExpr <- lift $ evalStateT genExprState (uniqueID-1) 
      return ("int _let" ++ show uniqueID ++ "(int _" ++ var ++ ") {\n" ++
          letFuncBody ++
          "return " ++ "(" ++ generatedBindExpr ++ ")"++ ";" ++
          " };\n")
  else 
    do  
      uniqueID <- get
      modify (+1)
      let genExprState = genExpr bodyExpr
      generatedBindExpr <- lift $ evalStateT genExprState 0
      return ("int _let" ++ show uniqueID ++ "(int _" ++ var ++ ") {\n" ++
          letFuncBody ++
          "return " ++ "(" ++ generatedBindExpr ++ ")"++ ";" ++
          " };\n")

runGenOptionalLetFunctions :: Expr -> String
runGenOptionalLetFunctions expr =
  evalState (genOptionalLetFunctions expr) 0



genParams :: [Name] -> String
genParams [] = ""
genParams [param] = genParam param
genParams (param:params) = genParam param ++ ", " ++ genParams params

genParam :: Name -> String
genParam name = "int _" ++ name

genLocalVars :: [Name] -> String
genLocalVars [] = ""
genLocalVars (var:vars) = "  int _" ++ var ++ ";\n" ++ genLocalVars vars

genExpr :: Expr -> GenState String
genExpr (Var name) = return $ "_" ++ name
genExpr (IntLit n) = return $ show n
genExpr (BoolLit True) = return "1"
genExpr (BoolLit False) = return "0"
genExpr (Infix op expr1 expr2) = do
  left <- genExpr expr1
  right <- genExpr expr2
  return $ "(" ++ left ++ genOp op ++ right ++ ")"
genExpr (If condExpr thenExpr elseExpr) = do
  cond <- genExpr condExpr
  thenPart <- genExpr thenExpr
  elsePart <- genExpr elseExpr
  return $ "" ++ cond ++ "?" ++ thenPart ++ ":" ++ elsePart ++ ""
genExpr (Let (var, ty) bindExpr bodyExpr) = do
  if isLetExpr bindExpr && isLetExpr bodyExpr
    then do
      bind <- genExpr bindExpr
      modify (+1)
      counter <- get
      let letExpr = "_let" ++ show counter 
      return $ letExpr ++ "(" ++ bind ++ ")" 
  else if isLetExpr bindExpr
    then do
      bind <- genExpr bindExpr  
      counter <- get
      modify (+1)
      let letExpr = "_let" ++ show counter 
      return $ letExpr ++ "(" ++ bind ++ ")"
  else if isLetExpr bodyExpr
    then do
      modify (+1)
      bind <- genExpr bindExpr
      counter <- get
      let letExpr = "_let" ++ show counter 
      return $ letExpr ++ "(" ++ bind ++ ")"
  else 
    do
      counter <- get
      bind <- genExpr bindExpr 
      modify (+1)
      let letExpr = "_let" ++ show counter 
      return $ letExpr ++ "(" ++ bind ++ ")"   

genExpr (App name args) = do
  argStrings <- mapM genExpr args
  return $ "_" ++ name ++ "(" ++ intercalate "," argStrings ++ ")"

isLetExpr :: Expr -> Bool
isLetExpr (Let _ _ _) = True
isLetExpr _ = False

genOp :: Op -> String
genOp Add = " + "
genOp Sub = " - "
genOp Mult = " * "
genOp Div = " / "
genOp Eq = " == "
genOp NEq = " != "
genOp GTh = " > "
genOp LTh = " < "
genOp GEq = " >= "
genOp LEq = " <= "

genArgs :: [Expr] -> String
genArgs [] = ""
genArgs [arg] =
  let (generatedExpr,_) = runState (genExpr arg) 0
  in 
    generatedExpr
genArgs (arg:args) =
  let (generatedExpr,_) = runState (genExpr arg) 0
  in
    generatedExpr ++ "," ++ genArgs args

genType :: Type -> String
genType TyInt = "int"

returnType :: Sig -> Type
returnType (Sig _ returnType) = returnType