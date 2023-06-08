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
import Data.IORef
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

import Data.List

-- CODE GENERATOR

genProgram :: Program -> String
genProgram (Program defs expr) =
  "#include <stdio.h>\n" ++
  genFunctionDefs defs ++
  "int main() {\n" ++
  "printf(\"%d\\n\", " ++ genExpr expr ++ ");" ++
  " }\n"

genFunctionDefs :: Defs -> String
genFunctionDefs [] = ""
genFunctionDefs (def:defs) = genFunctionDef def ++ genFunctionDefs defs

genFunctionDef :: FunDef -> String
genFunctionDef (FunDef (name, sig) params expr) =
  genType (returnType sig) ++ " _" ++ name ++ "(" ++ genParams params ++ "){\n" ++
  genOptionalLetFunctions expr ++"return" ++ genExpr expr ++ ";" ++
  "};\n"

genOptionalLetFunctions :: Expr -> String
genOptionalLetFunctions (Let (var, _) bindExpr bodyExpr) =
  genLetFunction var bodyExpr 
genOptionalLetFunctions (If condExpr thenExpr elseExpr) =
  genOptionalLetFunctions condExpr ++ genOptionalLetFunctions thenExpr ++ genOptionalLetFunctions elseExpr
genOptionalLetFunctions (Infix _ expr1 expr2) =
  genOptionalLetFunctions expr1 ++ genOptionalLetFunctions expr2
genOptionalLetFunctions (App _ args) =
  concatMap genOptionalLetFunctions args
genOptionalLetFunctions _ = ""

genLetFunction :: Name -> Expr -> String
genLetFunction var bindExpr =
  "int _let" ++ show uniqueID ++ "(int _" ++ var ++ ") {\n" ++ genOptionalLetFunctions bindExpr ++
  "return " ++ "(" ++ genExpr bindExpr ++ ")"++ ";" ++ {-quizas un case para cuando la bind exp es una q ya genera parentesis-}
  " };\n"



genParams :: [Name] -> String
genParams [] = ""
genParams [param] = genParam param
genParams (param:params) = genParam param ++ ", " ++ genParams params

genParam :: Name -> String
genParam name = "int _" ++ name

genLocalVars :: [Name] -> String
genLocalVars [] = ""
genLocalVars (var:vars) = "  int _" ++ var ++ ";\n" ++ genLocalVars vars

genExpr :: Expr -> String
genExpr (Var name) = "_" ++ name
genExpr (IntLit n) = show n
genExpr (BoolLit True) = "1"
genExpr (BoolLit False) = "0"
genExpr (Infix op expr1 expr2) =
  "(" ++ genExpr expr1 ++ genOp op ++ genExpr expr2 ++ ")"
genExpr (If condExpr thenExpr elseExpr) =
  "(" ++ genExpr condExpr ++ " ? " ++ genExpr thenExpr ++ " : " ++ genExpr elseExpr ++ ")"
genExpr (Let (var, ty) bindExpr bodyExpr) =
  "(" ++ genLet (var,ty) bindExpr bodyExpr ++ ")"
genExpr (App name args) =
  "_" ++ name ++ "(" ++ genArgs args ++ ")"

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

genLet :: TypedVar -> Expr -> Expr -> String
genLet (var, _) bindExpr bodyExpr =
  "_let" ++ show uniqueID ++ "(" ++ genExpr bindExpr ++ ")" 

genArgs :: [Expr] -> String
genArgs [] = ""
genArgs [arg] = genExpr arg
genArgs (arg:args) = genExpr arg ++ ", " ++ genArgs args

genType :: Type -> String
genType TyInt = "int"

returnType :: Sig -> Type
returnType (Sig _ returnType) = returnType

uniqueID :: Int
uniqueID = 0
