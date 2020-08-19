module IRUtils where

import IR
import qualified Control.Lens
import Control.Lens ((^.), (%~), to)

-- Call with multiple arguments
callArgumentList :: IR.Expr -> [IR.Expr] -> IR.Expr
callArgumentList = foldl IR.Call

-- Substitute a De Bruijn variable to an expression
subst :: Int -> Expr -> Expr -> Expr
subst idx sub_e (LocalVar s idx') =
  case compare idx idx' of
    EQ -> sub_e
    LT -> LocalVar s (idx' - 1)
    GT -> LocalVar s idx'
subst _ _ (Def s) = Def s
subst _ _ (InductiveType s) = InductiveType s
subst _ _ (Constructor s idx) = Constructor s idx
subst _ _ (Const c) = Const c
subst idx sub_e (Assign s e1 e2) = Assign s (subst idx sub_e e1) (subst (idx + 1) sub_e e2)
subst idx sub_e (Match e s cases) =
  let e' = subst idx sub_e e
      cases' = (\c -> case_expr %~ (subst (idx + (c^.case_args.to length)) sub_e)) <$> cases in
    Match e' s cases
subst idx sub_e (IfThenElse e1 e2 e3) = IfThenElse (subst idx sub_e e1) (subst idx sub_e e2) (subst idx sub_e e3)
subst idx sub_e (Call e1 e2) = Call (subst idx sub_e e1) (subst idx sub_e e2)
subst idx sub_e (Arrow e1 e2) = Arrow (subst idx sub_e e1) (subst idx sub_e e2)
subst idx sub_e (Lambda s e1 e2) = Lambda s (subst idx sub_e e1) (subst (idx + 1) sub_e e2)
subst _ _ Type = Type

-- Get an expression from a definition
getExprFromDef :: Definition -> Expr
getExprFromDef (Definition name ((arg_name, arg) : args) typ body) =
  Lambda (DI arg_name) arg $ getExprFromDef (Definition name args typ body)
getExprFromDef (Definition _ [] _ body) = body

-- Transform ((Constructor _ idx) a b c) into (idx, [a, b, c])
getConstrAndArgs :: Expr -> (Int, [Expr])
getConstrAndArgs (Constructor _ idx) = (idx, [])
getConstrAndArgs (Call a b) =
  let (constr, args) = getConstrAndArgs a in (constr, args ++ [b])
getConstrAndArgs _ = error "Wrong arguments given to getConstrAndArgs"

-- Big step semantics of call by value
callByValue :: Program -> Expr -> Expr
callByValue _ (LocalVar s idx) = LocalVar s idx
-- Keep the definition in case it has arguments (because it is a value),
-- ortherwise unfolds it
callByValue p (Def s) =
  let def = getDefinition s p in
    if def_args def == [] then
      callByValue p (getExprFromDef def)
    else Def s
callByValue _ (InductiveType s) = InductiveType s
callByValue _ (Constructor s idx) = Constructor s idx
callByValue _ (Const c) = Const c
callByValue p (Assign _ e1 e2) =
  callByValue p $ subst 0 (callByValue p e1) e2
callByValue p (IfThenElse cond e1 e2) =
  case callByValue p cond of
    Const (BoolConst True) -> callByValue p e1
    Const (BoolConst False) -> callByValue p e2
    cond' -> IfThenElse cond' e1 e2
callByValue p (Match e ind_name cases) =
  let e' = callByValue p e
      (constr_idx,args) = getConstrAndArgs e'
      cas = (cases !! constr_idx)^.case_expr
      ind_c = ind_constr (getInductive ind_name p) !! constr_idx
      lambda_cas = foldl (flip $ Lambda $ DI "") cas (snd <$> constr_args ind_c)
  in
    callByValue p $ foldl Call lambda_cas args
callByValue p (Call fun arg) =
  case callByValue p fun of
    Lambda _ _ body -> callByValue p $ subst 0 (callByValue p arg) body
    Def s -> callByValue p $ Call (getExprFromDef (getDefinition s p)) arg
    fun' -> Call fun' arg
callByValue _ (Lambda s e1 e2) = Lambda s e1 e2
callByValue _ (Arrow e1 e2) = Arrow e1 e2
callByValue _ Type = Type
