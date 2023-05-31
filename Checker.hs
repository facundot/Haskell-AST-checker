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

----------------------------------------------------------------------------
-- unique names
----------------------------------------------------------------------------
checkUniqueNames :: Program -> Checked
checkUniqueNames (Program defs _) =
  let allNames = getFunctionNames defs
      uniqueNames = nub allNames
      duplicatedNames = allNames \\ uniqueNames
  in
    if null duplicatedNames
      then Ok
      else Wrong (map Duplicated duplicatedNames)

getFunctionNames :: Defs -> [Name]
getFunctionNames defs = map getFunctionName defs

getFunctionName :: FunDef -> Name
getFunctionName (FunDef (name, _) _ _) = name


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
countParamsSig (Sig paramTypes _) = length paramTypes

