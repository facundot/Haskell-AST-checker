----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de eliminación de LETs
--
-- Un LET (let x = e1 in e2) es eliminado si e1 es 
-- un literal entero o booleano. En ese caso se 
-- sustituyen las ocurrencias de x en e2 por e1, 
-- o sea, e2[e1/x]. 
----------------------------------------------------------------------------

module LetElim where

import Syntax
import Data.List


-- ELIMINACION DE LETs


letElimP :: Program -> Program
letElimP (Program defs expr) = Program (map letElimDef defs) (letElimExpr expr)

letElimDef :: FunDef -> FunDef
letElimDef (FunDef fun params expr) = FunDef fun params (letElimExpr expr)

letElimExpr :: Expr -> Expr
letElimExpr expr@(Var _) = expr
letElimExpr expr@(IntLit _) = expr
letElimExpr expr@(BoolLit _) = expr
letElimExpr (Infix op e1 e2) = Infix op (letElimExpr e1) (letElimExpr e2)
letElimExpr (If cond e1 e2) = If (letElimExpr cond) (letElimExpr e1) (letElimExpr e2)
letElimExpr (Let (name, _) bindExpr bodyExpr) =
  case bindExpr of
    IntLit n -> let bodyExpr' = subst name bindExpr bodyExpr
                in letElimExpr bodyExpr'
    BoolLit b -> let bodyExpr' = subst name bindExpr bodyExpr
                 in letElimExpr bodyExpr'
    _ -> Let (name, TyInt) (letElimExpr bindExpr) (letElimExpr bodyExpr)
letElimExpr (App name expr) = App name (map letElimExpr expr)


subst :: Name -> Expr -> Expr -> Expr
subst name bindExpr expr@(Var varName)
  | varName == name = bindExpr
  | otherwise = expr
subst _ _ expr@(IntLit _) = expr
subst _ _ expr@(BoolLit _) = expr
subst name bindExpr (Infix op e1 e2) =
  Infix op (subst name bindExpr e1) (subst name bindExpr e2)
subst name bindExpr (If cond e1 e2) =
  If (subst name bindExpr cond) (subst name bindExpr e1) (subst name bindExpr e2)
subst name bindExpr (Let (var, ty) bindExpr' bodyExpr)
  | var == name = Let (var, ty) (subst name bindExpr bindExpr') bodyExpr
  | otherwise = Let (var, ty) (subst name bindExpr bindExpr') (subst name bindExpr bodyExpr)
subst name bindExpr (App name' args) =
  App name' (map (subst name bindExpr) args)












exampleProgram :: Program
exampleProgram = Program [] expr

expr :: Expr
expr = Let ("x", TyInt) (IntLit 3) (Let ("x", TyInt) (Infix Add (IntLit 4) (Var "x")) (Infix Add (Var "x") (IntLit 2)))

exampleProgram1 :: Program
exampleProgram1 = Program [] expr1
expr1 :: Expr
expr1 = Let ("x", TyInt) (IntLit 3) (Let ("x", TyInt) (IntLit 4) (Infix Add (Var "x") (IntLit 2)))

exampleProgram2 :: Program
exampleProgram2 = Program [] expr2

expr2 :: Expr
expr2 = Let ("x", TyInt) (IntLit 3) (Infix Add (Var "x") (Infix Mult (IntLit 2) (Var "x")))



main :: IO ()
main = do
  let optimizedProgram = letElimP exampleProgram
  putStrLn $ "Original program:\n" ++ show exampleProgram
  putStrLn $ "Optimized program:\n" ++ show optimizedProgram
  let optimizedProgram1 = letElimP exampleProgram1
  putStrLn $ "Original program:\n" ++ show exampleProgram1
  putStrLn $ "Optimized program:\n" ++ show optimizedProgram1
  let optimizedProgram2 = letElimP exampleProgram2
  putStrLn $ "Original program:\n" ++ show exampleProgram2
  putStrLn $ "Optimized program:\n" ++ show optimizedProgram2