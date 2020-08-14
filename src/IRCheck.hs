module IRCheck where

import IR
import Data.Map
import Error

-- Get definitions referenced by the expression
getExprDefs :: Expr -> [String]
getExprDefs (LocalVar _ _) = []
getExprDefs (Def s) = [s]
getExprDefs (IntConst _) = []
getExprDefs (BoolConst _) = []
getExprDefs (PrimitiveType _) = []
getExprDefs (Assign _ e1 e2) = getExprDefs e1 ++ getExprDefs e2
getExprDefs (IfThenElse e1 e2 e3) = getExprDefs e1 ++ getExprDefs e2 ++ getExprDefs e3
getExprDefs (Call e1 e2) = getExprDefs e1 ++ getExprDefs e2
getExprDefs (Arrow e1 e2) = getExprDefs e1 ++ getExprDefs e2
getExprDefs Type = []

-- Get definitions referenced by the definition
getDefDefs :: Definition -> [String]
getDefDefs (Definition _ args typ body) =
  let args_defs = concat $ Prelude.map (getExprDefs . snd) args in
  let typ_defs = getExprDefs typ in
  let body_defs = getExprDefs body in
    args_defs ++ typ_defs ++ body_defs

-- Get definitions referenced by the program
getProgramDefs :: Program -> [String]
getProgramDefs p = concat $ elems $ Data.Map.map getDefDefs p

-- Check that all definitions referenced by the program are in the program
checkDefs :: Program -> Either Error ()
checkDefs p =
  let nonMember = Prelude.filter (\x -> notMember x p) $ getProgramDefs p in
  if Prelude.null nonMember then
    return ()
  else
    Left $ DefsNotFound nonMember

checkIR :: Program -> Either Error ()
checkIR p = checkDefs p
