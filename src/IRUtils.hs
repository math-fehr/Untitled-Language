module IRUtils where

import IR

-- Call with multiple arguments
callArgumentList :: IR.Expr -> [IR.Expr] -> IR.Expr
callArgumentList fun [] = fun
callArgumentList fun (arg : args) =
  callArgumentList (IR.Call fun arg) args

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
subst idx sub_e (IfThenElse e1 e2 e3) = IfThenElse (subst idx sub_e e1) (subst idx sub_e e2) (subst idx sub_e e3)
subst idx sub_e (Call e1 e2) = Call (subst idx sub_e e1) (subst idx sub_e e2)
subst idx sub_e (Arrow e1 e2) = Arrow (subst idx sub_e e1) (subst idx sub_e e2)
subst idx sub_e (Lambda s e1 e2) = Lambda s (subst idx sub_e e1) (subst (idx + 1) sub_e e2)
subst _ _ Type = Type

-- Check is a closed expression is a value
-- A value is either a lambda or a constant
isValue :: Expr -> Bool
isValue (Lambda _ _ _) = True
isValue (Const _) = True
isValue _ = False

-- Get an expression from a definition
getExprFromDef :: Definition -> Expr
getExprFromDef (Definition name ((arg_name, arg) : args) typ body) =
  Lambda arg_name arg $ getExprFromDef (Definition name args typ body)
getExprFromDef (Definition _ [] _ body) = body

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
callByValue p (Assign _ e1 e2) = callByValue p $ subst 0 e1 e2
callByValue p (IfThenElse cond e1 e2) =
  case callByValue p cond of
    Const (BoolConst True) -> callByValue p e1
    Const (BoolConst False) -> callByValue p e2
    cond' -> IfThenElse cond' e1 e2
callByValue p (Call fun arg) =
  case callByValue p fun of
    Lambda _ _ body -> callByValue p (subst 0 arg body)
    Def s -> callByValue p (Call (getExprFromDef (getDefinition s p)) arg)
    fun' -> Call fun' arg
callByValue _ (Lambda s e1 e2) = Lambda s e1 e2
callByValue _ (Arrow e1 e2) = Arrow e1 e2
callByValue _ Type = Type
